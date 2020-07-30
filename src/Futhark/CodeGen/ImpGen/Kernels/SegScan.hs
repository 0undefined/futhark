{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegScan
  ( compileSegScan )
  where

import Control.Monad.Except
import Data.Maybe
import Data.List

import Prelude hiding (quot, rem)

import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base

import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
-- import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)
-- import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem)
-- import Futhark.Tools hiding (toExp)--(letSubExp)
-- import Futhark.Construct  hiding (toExp)

after_warpscan_area::Imp.Exp
after_warpscan_area = 0


compileSegScan :: Pattern ExplicitMemory
               -> SegLevel -- At which level the *body* of a SegOp executes.
               -> SegSpace -- Index space of a SegOp.
               -> Lambda ExplicitMemory
               -> [SubExp]
               -> KernelBody ExplicitMemory
               -> CallKernelGen ()
compileSegScan  (Pattern _ pes)
                lvl
                space
                scan_op
                nes
                kbody = do
    let (gtids, dims) = unzip $ unSegSpace space
    let arrsize = head dims
    arraysize <- toExp arrsize
    let gtid = head gtids

    -- let group_size = segGroupSize lvl                                  -- Subexp
    group_size' <- traverse toExp $ segGroupSize lvl                  -- Imp.Exp

    let apg = Imp.BinOpExp (Add Int32) arraysize (unCount group_size')
    let apgmone = Imp.BinOpExp (Sub Int32) apg 1
    let num_groups_impexp = Imp.BinOpExp (SDiv Int32) apgmone (unCount group_size')
    ng <- dPrimV "num_groups" num_groups_impexp

    let (hxp, _hyp) = splitAt (length nes) $ lambdaParams scan_op

    aggregates <- forM hxp $ \p -> do
            let pt = elemType $ paramType p
            sAllocArray "aggregates" pt (Shape [Var ng]) $ Space "device"

    incprefix <- forM hxp $ \p -> do
            let pt = elemType $ paramType p
            sAllocArray "incprefix" pt (Shape [Var ng]) $ Space "device"


    statusflgs <- sAllocArray "statusflgs" int8 (Shape [Var ng]) (Space "device")

    g_dyn_id <- sAllocArray "dyn_id" int32 (Shape [intConst Int32 1]) (Space "device")
    copyDWIMFix g_dyn_id [0] (intConst Int32 0) []
    (global_id, _, _) <- fullyIndexArray g_dyn_id [0]

    available_local_mem <- dPrim "available_local_mem" int32
    sOp $ Imp.GetSizeMax available_local_mem Imp.SizeLocalMemory

    -- TODO: calc eLEMS_PER_THREAD
    -- element_sizes <- forM_ hxp $ \para -> do
    --       let pt = elemType $ paramType para
    --           shape = Shape [Constant eLEMS_PER_THREAD]
    --       return (unCount (typeSize (Array pt shape NoUniqueness)))


    let eLMPT = 9::Integer
    -- intValueType :: IntValue -> IntType
    let eLEMS_PER_THREAD = IntValue $ intValue Int32 eLMPT
    let eLEMS_PER_THREAD_const = ValueExp eLEMS_PER_THREAD -- Constant eLEMS_PER_THREAD
    let eLM_shape = Shape [Constant eLEMS_PER_THREAD]
    gsize <- dPrimV "gsize" (unCount group_size' * eLEMS_PER_THREAD_const)




    -- Spawn the kernel --------------------------------------------------------
    -- sKernelThread :: String
                -- -> Count NumGroups Imp.Exp -> Count GroupSize Imp.Exp
                -- -> VName
                -- -> (KernelConstants -> InKernelGen ())
                -- -> CallKernelGen ()
    sKernelThread "my_scan" (Imp.Count num_groups_impexp) group_size' (segFlat space) $ \constants -> everythingVolatile $ do

      -- let alm = Imp.var available_local_mem int32
      -- div2 <- dPrim "div2" int32
      -- div2 <-- Imp.BinOpExp (SDiv Int32) alm 2


      let ltid = kernelLocalThreadId constants
      let waveSize = kernelWaveSize constants

      -- Local memory allocation
      let (xp, _yp) = splitAt (length nes) $ lambdaParams scan_op
      -- let (Count gsz) = group_size
      exchange <- forM xp $ \p -> do
            let pt = elemType $ paramType p
                -- shape = Shape [gsz]                                                -- ShapeBase SubExp
                -- shape = Shape [kernelGroupSize constants * eLEMS_PER_THREAD_const] -- ShapeBase Imp.Exp
                -- shape = Shape [gsz * (Constant eLEMS_PER_THREAD)] -- No instance for (Num SubExp) arising from a use of ‘*’
                -- shape = Shape [gsz * (intConst Int32 eLMPT)] -- No instance for (Num SubExp) arising from a use of ‘*’
                shape = Shape [Var gsize]
            sAllocArray "exchange" pt shape $ Space "local"

      -- TODO: reuse exchange.
-- sAllocArray :: String -> PrimType -> ShapeBase SubExp -> Space -> ImpM lore op VName
      -- block_id <- sAllocArray "block_id" int32 (Shape [intConst Int32 1]) (Space "local")

      -- block_id <- sArray "block_id" int32 (Shape [intConst Int32 1]) $ ArrayIn (head exchange) $ IxFun.iota [1]
      -- m <- lookupArray $ head exchange
      -- TODO: reuse exchange - Fails at 4096 elements list at element 512
-- sArray :: String -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore op VName
      MemLocation m _ _ <- entryArrayLocation <$> (lookupArray $ head exchange)
      block_id <- sArray "block_id" int32 (Shape [intConst Int32 1]) $ ArrayIn m $ IxFun.iota [1]

      -- Get dynamic block id
      sWhen (ltid .==. 0) $ do
        reg_dyn_id <- dPrimV "reg_dyn_id" 0
        sOp $ Imp.Atomic DefaultSpace $ Imp.AtomicAdd reg_dyn_id global_id 0 1
        copyDWIMFix statusflgs [Imp.var reg_dyn_id int32] (intConst Int8 0) []
        copyDWIMFix block_id [0] (Var reg_dyn_id) []

      sOp Imp.LocalBarrier

      wG_ID <- dPrim "wG_ID" int32
      copyDWIMFix wG_ID [] (Var block_id) [0]

      let wG_ID_var = Imp.var wG_ID int32
      sOp Imp.LocalBarrier

      -- Read coalesced input data to registers
      -- Apply map function
      -- Write results to output
      -- chunk <- forM xp $ \para -> do
      --       let pt = elemType $ paramType para
      --       dPrim "chunk" pt

-- This works for one element of typesize 4
      -- chunk <- forM xp $ \para  -> do
      --       let pt = elemType $ paramType para
      --       let name = "chunk"
      --       mem <- sAlloc (name ++ "_mem") 4 $ Space "private"
      --       sArray name pt eLM_shape $ ArrayIn mem $ IxFun.iota [4]
            -- sAllocArray name pt eLM_shape $ ScalarSpace [intConst Int8 1] pt

      chunk <- forM xp $ \para  -> do
            let pt = elemType $ paramType para
            -- sAllocArray name pt eLM_shape $ ScalarSpace [Constant eLEMS_PER_THREAD] pt
            let loc = ScalarSpace [Constant eLEMS_PER_THREAD] pt
            sAllocArray "chunk" pt eLM_shape loc

            -- mem <- sAlloc (name ++ "_mem") 4 $ ScalarSpace [Constant eLEMS_PER_THREAD] pt
            -- sArray name pt eLM_shape $ ArrayIn mem $ IxFun.iota [eLEMS_PER_THREAD_prim]


      -- sAlloc_ :: VName -> Count Bytes Imp.Exp -> Space -> ImpM lore op ()
      -- sAlloc   (ScalarSpace  _) = do
      --   return ()
      -- sAlloc_ name' size' space = do
      --   allocator <- asks $ M.lookup space . envAllocCompilers
      --   case allocator of
      --     Nothing -> emit $ Imp.Allocate name' size' space
      --     Just allocator' -> allocator' name' size'

      -- sAlloc (ScalarSpace  _) = do
      --   return ()

      -- sAlloc_


      --       name' <- newVName name
      --       -- dArray :: VName -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore op ()
      --       dArray name' pt eLM_shape mem
      --       return name'


            -- This is the worst: tries to malloc AND mul with size
            -- sAllocArray name pt eLM_shape $ ScalarSpace [intConst Int8 1] pt

      -- chunk <- forM (zip xp element_sizes) $ \(para, size)  -> do
      --       let pt = elemType $ paramType para

      --       let name = "chunk"
      --       mem <- sAlloc (name ++ "_mem") size $ Space "private"
      --       sArray name pt eLM_shape $ ArrayIn mem $ IxFun.iota [size]



                -- shape_c = Shape [eLEMS_PER_THREAD_const]
                -- shape_c = Shape [Var eLEMS_PER_THREAD]
            -- name' <- newVName "chunk"
            -- entry <- memBoundToVarEntry Nothing $ MemArray pt shape NoUniqueness mem
            -- addVar "chunk" entry
            -- return name'

            -- dArray name' bt shape membind
            -- return name'


      sComment "Coalesced read, apply map, write result and write to reg" $ do
        block_offset <- dPrim "block_offset" int32
        block_offset <-- wG_ID_var * kernelGroupSize constants * eLEMS_PER_THREAD_const
        let block_offset_var = Imp.var block_offset int32
        sFor "i" eLEMS_PER_THREAD_const $ \i -> do
          -- let dgtid = Imp.vi32 wG_ID * unCount group_size' + ltid -- old
          let dgtid = block_offset_var + i * unCount group_size' + ltid
          -- TODO: I don't think this hack works with segmented arrays.
          dPrimV_ gtid dgtid
          let gtid_var = Imp.var gtid int32

          let inrange = compileStms mempty (kernelBodyStms kbody) $ do
                let (input_elm, map_res) = splitAt (length nes) $ kernelBodyResult kbody
                forM_ (zip exchange input_elm) $ \(arr, input) ->
                  copyDWIMFix arr [gtid_var - block_offset_var] (kernelResultSubExp input) []
                -- forM_ (zip (drop (length nes) pes) map_res) $ \(pe, se) ->
                --   copyDWIMFix (patElemName pe) (map (`Imp.var` int32) gtids)
                --   (kernelResultSubExp se) []
                forM_ (zip (drop (length nes) pes) map_res) $ \(pe, se) ->
                  copyDWIMFix (patElemName pe) (map (`Imp.var` int32) gtids)
                  (kernelResultSubExp se) []

          let padding = forM_ (zip exchange nes) $ \(arr, neutral) ->
                        copyDWIMFix arr [gtid_var - block_offset_var] neutral []

          sIf (gtid_var .<. arraysize)
              inrange
              padding

        sOp Imp.LocalBarrier
        loc_offset <- dPrim "loc_offset" int32
        loc_offset <-- ltid * eLEMS_PER_THREAD_const
        let loc_offset_var = Imp.var loc_offset int32
        sFor "i" eLEMS_PER_THREAD_const $ \i -> do
          forM_ (zip chunk exchange) $ \(ch, ex) ->
                copyDWIMFix ch [i] (Var ex) [loc_offset_var+i]

        sOp Imp.LocalBarrier

      -- Per Thread scan
      acc <- forM nes $ \ne -> do
                ne' <- toExp ne
                dPrimV "acc" ne'
      forM_ (zip acc chunk) $ \(a, c) -> copyDWIMFix a [] (Var c) [0]

      sComment "Per-Thread Scan" $ do
        sFor "i" (eLEMS_PER_THREAD_const-1) $ \i -> do
          dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
          let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op
          forM_ (zip scan_x_params acc) $ \(param, a) ->
            copyDWIMFix (paramName param) [] (Var a) []

          forM_ (zip scan_y_params chunk) $ \(param, c) ->
            copyDWIMFix (paramName param) [] (Var c) [i+1]

          compileStms mempty (bodyStms $ lambdaBody scan_op) $
            forM_ (zip acc (bodyResult $ lambdaBody scan_op)) $ \(a, sr) ->
              copyDWIMFix a [] sr []
          forM_ (zip chunk acc) $ \(c, a) -> copyDWIMFix c [i+1] (Var a) []
        forM_ (zip exchange acc) $ \(e, a) -> copyDWIMFix e [ltid] (Var a) []
        sOp Imp.LocalBarrier

      sComment "Per-Group Scan" $ do

        ress <- forM nes $ \ne -> do
                  ne' <- toExp ne
                  dPrimV "res" ne'

        -- p = 1
        p1 <- dPrim "p1" int32
        p1 <-- 1
        let p1Var = Imp.var p1 int32
        sOp Imp.LocalBarrier -- REALLY?

        -- while p < array.size
        sWhile (p1Var .<. arraysize) $ do

          -- if tid >= p then res = op array[tid-p] array[tid]
          sWhen (ltid .>=. p1Var) $ do

            dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
            let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

            forM_ (zip scan_x_params exchange) $ \(param, arr) ->
              copyDWIMFix (paramName param) [] (Var arr) [ltid]

            forM_ (zip scan_y_params exchange) $ \(param, arr) ->
              copyDWIMFix (paramName param) [] (Var arr) [ltid - p1Var]

            compileStms mempty (bodyStms $ lambdaBody scan_op) $
              forM_ (zip ress (bodyResult $ lambdaBody scan_op)) $ \(res, sr) ->
                copyDWIMFix res [] sr []

          sOp Imp.LocalBarrier

          -- if tid >= p then array[tid] = res
          sWhen (ltid .>=. p1Var) $
            forM_ (zip exchange ress) $ \(arr, res) ->
              copyDWIMFix arr [ltid] (Var res) []
          sOp Imp.LocalBarrier

          p1 <-- p1Var * 2

        -- int32_t prev_ind = (tid == 0) ? (get_local_size(0) - 1) : (tid - 1);
        let my_group_size = kernelGroupSize constants
        prev_ind <- dPrim "prev_ind" int32
        sIf (ltid .==. 0) (prev_ind <-- my_group_size - 1) (prev_ind <-- ltid - 1)

        -- acc <- forM xp $ \para -> do
        --       let pt = elemType $ paramType para
        --       dPrim "acc" pt

        forM_ (zip acc exchange) $ \(a, e) ->
          copyDWIMFix a [] (Var e) [Imp.var prev_ind int32]
        sOp Imp.LocalBarrier

      prefix <- forM nes $ \ne -> do
          ne' <- toExp ne
          dPrimV "prefix" ne'

      sComment "Compute prefix from previous blocks" $ do
        -- first group, first warp, first lane
        sWhen (Imp.var wG_ID int32 .==. 0 .&&. ltid .==. 0) $ do
          -- scanwarp <- sAllocArray "scanwarp" int8 (Shape [unCount num_groups]) (Space "local")
          forM_ (zip incprefix acc) $ \(i, a) ->
            copyDWIMFix i [0] (Var a) []
          sOp Imp.MemFenceGlobal
          copyDWIMFix statusflgs [0] (intConst Int8 2) []
          forM_ (zip acc nes) $ \(a, neutral) ->
            copyDWIMFix a [] neutral []

        -- WG_ID != 0, first warp, all lanes
        -- sWhen (Imp.var wG_ID int32 ./=. 0 .&&. ltid .<. kernelWaveSize) $ do
        sUnless (Imp.var wG_ID int32 .==. 0 .||. ltid .>=. waveSize) $ do

        -- ITEM: reuse exchange memory for warpscan
          -- This local allocation does not reuse the memory.
          --   can be investigated at the optimization phase.
          --   Note: the Shape should have the size of the wave size not just the hard coded 32.
          -- warpscan <- sAllocArray "warpscan" int8 (Shape [intConst Int32 32]) (Space "local")

          -- block_id <- sArray "block_id" int32 (Shape [intConst Int32 1]) $ ArrayIn (head exchange) $ IxFun.iota [1]
          -- m <- lookupArray $ head exchange
          -- TODO: warpscan reuse exchange -
          -- sArray :: String -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore op VName
          MemLocation mem _ _ <- entryArrayLocation <$> (lookupArray $ head exchange)
          warpscan <- sArray "warpscan" int8 (Shape [intConst Int32 (32*5)]) $ ArrayIn mem $ IxFun.iota [32*5]





          sWhen (ltid .==. 0) $ do
            forM_ (zip aggregates acc) $ \(ag, ac) ->
              copyDWIMFix ag [Imp.var wG_ID int32] (Var ac) []
            sOp Imp.MemFenceGlobal
            copyDWIMFix statusflgs [Imp.var wG_ID int32] (intConst Int8 1) []
            copyDWIMFix warpscan [0+32*4] (Var statusflgs) [Imp.var wG_ID int32 - 1]

          -- sOp Imp.MemFenceGlobal
          sOp Imp.MemFenceLocal
          -- if (warpscan[0] == STATUS_P && tid == 0) prefix = incprefix[WG_ID-1];
          status1 <- dPrim "status1" int8
          copyDWIMFix status1 [] (Var warpscan) [0+32*4]
          sIf (Imp.var status1 int8 .==. 2)
            -- if
            (sWhen (ltid .==. 0) (forM_ (zip prefix incprefix) $ \(pre,inc) ->
              copyDWIMFix pre [] (Var inc) [Imp.var wG_ID int32 - 1])) $ do

            -- else
            read_offset <- dPrim "read_offset" int32
            read_offset <-- Imp.var wG_ID int32 - waveSize
            loop_stop <- dPrim "loop_stop" int32
            loop_stop <-- -1 * waveSize

            -- while (read_offset > LOOP_STOP) {
            sWhile (Imp.var read_offset int32 .>. Imp.var loop_stop int32) $ do
              readi <- dPrim "readi" int32
              readi <-- Imp.var read_offset int32 + ltid
              let readiExp = Imp.var readi int32
              aggr <- forM nes $ \ne -> do
                ne' <- toExp ne
                dPrimV "aggr" ne'
              flag <- dPrim "flag" int8
              flag <-- 0
              let flagExp = Imp.var flag int32
              used <- dPrim "used" int8
              used <-- 0
              sWhen (readiExp .>=. 0) $ do
                copyDWIMFix flag [] (Var statusflgs) [readiExp]
                sWhen (flagExp .==. 2) $ -- STATUS_P
                  forM_ (zip aggr incprefix) $ \(ag,inc) ->
                    copyDWIMFix ag [] (Var inc) [readiExp]
                sWhen (flagExp .==. 1) $ do -- STATUS_A
                  forM_ (zip aggr aggregates) $ \(ag,ags) ->
                    copyDWIMFix ag [] (Var ags) [readiExp]
                  used <-- 1
              -- exchange[tid]       = aggr;
              forM_ (zip exchange aggr) $ \(ex,ag) ->
                copyDWIMFix ex [ltid] (Var ag) []
              -- warpscan[tid]       = mkStatusUsed(used, flag);
              combined_flg <- dPrim "combined_flg" int8
              -- TODO: The msb is signed and have to be mult with -1 or bit shiftet or something
              -- combined_flg <-- (Imp.var used int8 * 4) .|. Imp.var flag int8
              combined_flg <-- Imp.BinOpExp (Shl Int8) (Imp.var used int8) 2 .|. Imp.var flag int8
              copyDWIMFix warpscan [ltid+32*4] (Var combined_flg) []
              -- sOp Imp.MemFenceGlobal
              sOp Imp.MemFenceLocal


              -- Perform reduce
              wsmone <- dPrim "wsmone" int8
              -- wsmone <-- 0
              copyDWIMFix wsmone [] (Var warpscan) [waveSize-1+32*4]
              sUnless (Imp.var wsmone int32 .==. 2) $ do -- STATUS_P

                -- p = 1
                p <- dPrim "p" int32
                p <-- 1
                let pVar = Imp.var p int32

                -- while p < array.size
                sWhile (pVar .<. waveSize) $ do

                  -- if tid >= p then res = op array[tid-p] array[tid]
                  sWhen (ltid .>=. pVar) $ do
                    let acc_th = ltid - pVar
                    let cur_th = ltid
                    agg1 <- forM nes $ \ne -> do
                      ne' <- toExp ne
                      dPrimV "agg1" ne'
                    forM_ (zip agg1 exchange) $ \(ag, ex) ->
                      copyDWIMFix ag [] (Var ex) [acc_th]

                    agg2 <- forM nes $ \ne -> do
                      ne' <- toExp ne
                      dPrimV "agg2" ne'
                    forM_ (zip agg2 exchange) $ \(ag, ex) ->
                      copyDWIMFix ag [] (Var ex) [cur_th]

                    usd1 <- dPrim "usd1" int8
                    usd2 <- dPrim "usd2" int8
                    stat1 <- dPrim "stat1" int8
                    stat2 <- dPrim "stat2" int8
                    tmp <- dPrim "tmp" int8
                    let tmpVar = Imp.var tmp int32
                    copyDWIMFix tmp [] (Var warpscan) [acc_th+32*4]

                    stat1 <-- tmpVar .&. 3
                    usd1 <-- Imp.BinOpExp (LShr Int8) tmpVar 2

                    copyDWIMFix tmp [] (Var warpscan) [cur_th+32*4]
                    stat2 <-- tmpVar .&. 3
                    usd2 <-- Imp.BinOpExp (LShr Int8) tmpVar 2

                    sUnless (Imp.var stat2 int32 .==. 1) $ do
                      forM_ (zip agg1 nes) $ \(ag, ne) -> do
                        ne' <- toExp ne
                        ag <-- ne'
                      usd1 <-- 0
                      stat1 <-- Imp.var stat2 int8
                    usd1 <-- Imp.var usd1 int8 + Imp.var usd2 int8
                    usd1 <-- Imp.BinOpExp (Shl Int8) (Imp.var usd1 int8) 2
                    usd1 <-- Imp.var usd1 int8 .|. Imp.var stat1 int8
                    copyDWIMFix warpscan [cur_th+32*4] (Var usd1) []

                    dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
                    let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

                    forM_ (zip scan_x_params agg1) $ \(param, ag) ->
                      copyDWIMFix (paramName param) [] (Var ag) []

                    forM_ (zip scan_y_params agg2) $ \(param, ag) ->
                      copyDWIMFix (paramName param) [] (Var ag) []

                    compileStms mempty (bodyStms $ lambdaBody scan_op) $
                      forM_ (zip exchange (bodyResult $ lambdaBody scan_op)) $ \(ex, sr) ->
                        copyDWIMFix ex [cur_th] sr []

                    -- forM_ (zip exchange ress) $ \(ex, res) ->
                    --   copyDWIMFix ex [acc_th] (Var res) []

                  p <-- pVar * 2

              -- sOp Imp.MemFenceGlobal
              sOp Imp.MemFenceLocal
              sWhen (ltid .==.0) $ do
                -- read result from local data after warp reduce
                usedflg_val <- dPrim "usedflg_val" int8
                copyDWIMFix usedflg_val [] (Var warpscan) [waveSize-1+32*4]
                flag <-- Imp.var usedflg_val int8 .&. 3 -- get status
                sIf (flagExp .==. 2)
                  (read_offset <-- Imp.var loop_stop int32) $ do-- EXIT
                  used <-- Imp.BinOpExp (LShr Int8) (Imp.var usedflg_val int8) 2 -- get used: usd_flg >> 2
                  read_offset <-- Imp.var read_offset int32 - Imp.var used int32
                copyDWIMFix block_id [0] (Var read_offset) []
                -- update prefix
                forM_ (zip aggr exchange) $ \(ag,ex) ->
                  copyDWIMFix ag [] (Var ex) [waveSize-1]

                dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
                let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

                forM_ (zip scan_x_params aggr) $ \(param, agg) ->
                  copyDWIMFix (paramName param) [] (Var agg) []

                forM_ (zip scan_y_params prefix) $ \(param, pre) ->
                  copyDWIMFix (paramName param) [] (Var pre) []

                compileStms mempty (bodyStms $ lambdaBody scan_op) $
                  forM_ (zip prefix (bodyResult $ lambdaBody scan_op)) $ \(pre, sr) ->
                    copyDWIMFix pre [] sr []
              -- sOp Imp.MemFenceGlobal
              sOp Imp.MemFenceLocal
              copyDWIMFix read_offset [] (Var block_id) [0]

          sWhen (ltid .==. 0) $ do
            dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
            let (scan_x_params, scan_y_params) = splitAt (length nes) $ lambdaParams scan_op

            forM_ (zip scan_x_params prefix) $ \(param, pre) ->
              copyDWIMFix (paramName param) [] (Var pre) []

            forM_ (zip scan_y_params acc) $ \(param, ac) ->
              copyDWIMFix (paramName param) [] (Var ac) []

            compileStms mempty (bodyStms $ lambdaBody scan_op) $
              forM_ (zip incprefix (bodyResult $ lambdaBody scan_op)) $ \(ipre, sr) ->
                copyDWIMFix ipre [Imp.var wG_ID int32] sr []

            sOp Imp.MemFenceGlobal

            -- ITEM: reuse exchange memory for warpscan
            -- We move the the index of the exchange to be outside the warpscan
            -- work area with the after_warpscan_area constant
            copyDWIMFix statusflgs [Imp.var wG_ID int32] (intConst Int8 2) [] -- STATUS_P
            forM_ (zip exchange prefix) $ \(exc, pre) ->
              copyDWIMFix exc [after_warpscan_area] (Var pre) []
            forM_ (zip acc nes) $ \(a, ne) ->
              copyDWIMFix a [] ne []

        sUnless (Imp.var wG_ID int32 .==. 0) $ do
          sOp Imp.LocalBarrier
          forM_ (zip prefix exchange) $ \(pre,exc) ->
            -- ITEM: reuse exchange memory for warpscan
            -- work area with the after_warpscan_area constant
            copyDWIMFix pre [] (Var exc) [after_warpscan_area]
          sOp Imp.LocalBarrier

      sComment "Read and add prefix to every element in this workgroup" $ do
        myacc <- forM nes $ \ne -> do
                  ne' <- toExp ne
                  dPrimV "myacc" ne'

        dScope Nothing $ scopeOfLParams $ lambdaParams scan_op
        let (scan_x_params1, scan_y_params1) = splitAt (length nes) $ lambdaParams scan_op
        forM_ (zip scan_x_params1 prefix) $ \(param, pre) ->
          copyDWIMFix (paramName param) [] (Var pre) []
        forM_ (zip scan_y_params1 acc) $ \(param, ac) ->
          copyDWIMFix (paramName param) [] (Var ac) []
        compileStms mempty (bodyStms $ lambdaBody scan_op) $ do
          forM_ (zip myacc (bodyResult $ lambdaBody scan_op)) $ \(ma, sr) ->
            copyDWIMFix ma [] sr []

        sFor "i" eLEMS_PER_THREAD_const $ \i -> do
          scan_op_renamed <- renameLambda scan_op
          dScope Nothing $ scopeOfLParams $ lambdaParams scan_op_renamed
          let (scan_x_params2, scan_y_params2) = splitAt (length nes) $ lambdaParams scan_op_renamed
          forM_ (zip scan_x_params2 myacc) $ \(param, ma) ->
            copyDWIMFix (paramName param) [] (Var ma) []
          forM_ (zip scan_y_params2 chunk) $ \(param, ch) ->
            copyDWIMFix (paramName param) [] (Var ch) [i]
          compileStms mempty (bodyStms $ lambdaBody scan_op_renamed) $
            forM_ (zip exchange (bodyResult $ lambdaBody scan_op_renamed)) $ \(e, sr) ->
              copyDWIMFix e [ltid*eLEMS_PER_THREAD_const+i] sr []

        sOp Imp.LocalBarrier

        block_offset <- dPrim "block_offset" int32
        block_offset <-- wG_ID_var * kernelGroupSize constants * eLEMS_PER_THREAD_const
        let block_offset_var = Imp.var block_offset int32

        sFor "i" eLEMS_PER_THREAD_const $ \i -> do
          dgtid <- dPrim "dgtid" int32
          dgtid <-- block_offset_var + i * kernelGroupSize constants + ltid
          let dgtid_var = Imp.var dgtid int32
          -- let dgtid = block_offset_var + i * kernelGroupSize constants + ltid
          -- let dgtid = block_offset_var + i * unCount group_size' + ltid
          -- let dgtid = Imp.vi32 wG_ID * unCount group_size' + ltid -- old one
          sWhen (dgtid_var .<. arraysize) $
            forM_ (zip pes exchange) $ \(dest, arr) ->
              copyDWIMFix (patElemName dest) [dgtid_var] (Var arr) [dgtid_var - block_offset_var]

      return ()
