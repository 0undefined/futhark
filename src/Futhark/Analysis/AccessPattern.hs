module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    analyzeStm,
    ArrayIndexDescriptors,
    IterationType,
    Dependencies,
    ArrayName,
    SegMapName,
    IndexExprName,
    DimIdxPat,
  )
where

import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Futhark.IR.GPU
import Futhark.Util.Pretty

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

-- | Set of VNames of gtid's that some access is variant to.
-- Tuple of patternName and nested `level` it is created at.
type Dependencies = S.IntMap (VName, Int)

type ArrayName = VName

type SegMapName = (Int, VName)

type LoopBodyName = (Int, VName)

type CondBodyName = (Int, VName)

type IndexExprName = VName

-- | Collect all features of access to a specific dimension of an array.
data DimIdxPat = DimIdxPat
  { -- | Set of gtid's that the access is variant to.
    -- An empty set indicates that the access is invariant.
    dependencies :: Dependencies,
    -- | Whether the acess is parallel or sequential
    iterType :: IterationType
  }
  deriving (Eq, Ord, Show)

-- | Each element in the list corresponds to an access to a dimension in the given array
-- in the order of the dimensions.
type MemoryEntry = [DimIdxPat]

-- | Map variable names of arrays in a segmap to index expressions on those arrays.
type ArrayIndexDescriptors =
  M.Map SegMapName (M.Map ArrayName (M.Map IndexExprName [MemoryEntry]))

-- segmap(pattern) => A(pattern) => indexExpressionName(pattern) => [DimIdxPat]
--

-- ================ EXAMPLE 1 ================
-- segmap_0 (p)
--   ...
--   segmap_1 (q)
--     let as_1  = A[q,p,i] + A[x,z,q]
--     let as_2  = A[x+y,z,q]
--     let as_3  = A[0,0,0] + B[1,1,1]
--     let res_2 = as + A[y,0,x]
--     in res_2
--
-- ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
--
-- segmap_0:
--   A:
--     as_1:
--       [q,p,i] -> [i,p,q]
-- ...
-- segmap_1:
--   A:
--     as_1:
--       [q,p,i] -> [i,p,q]
--       [x,z,q]
--     as_2:
--       [[x 0,y 1],z 1337 ,q 1]
--     as_3:
--       [0,0,0]
--     res_2:
--       [y,0,x]
--  B:
--    as_3:
--      [1,1,1]

-- A := ...
-- segmap_0 (x,y)
--  A[y,x]
--  ...
--  segmap_1 (i,j)
--  ...
--    A[j,i]

-- seg (i,j)
--  loop l < n
--    seg (x,y)
--      A[i,j,l,x,y]

-- ================ EXAMPLE 2 ================

-- segmap_0 (x,y):
--  segmap_1 (p):
--    segmap_2 (q):
--      let as_1 = A[q,p]

-- ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

-- ctx: [segmap_0, segmap_1, segmap_2]

-- segmap_0:      -- it is a false positive[VERIFICATION NEEDED], since segmap_0
--   A:           -- does not directly provide variables to index into `as_1`
--     as_1:
--       [q,p]
-- segmap_1:
--   A:
--     as_1:
--       [q,p]
-- segmap_2:
--   A:
--     as_1:
--       [q,p]

-- OKAY, but what if
-- let Bs = segmap () As
-- let Cs = segmap () Bs
-- How is Bs and Cs tracked to their respective segmaps?

-- | Only used during the analysis to keep track of the dependencies of each
-- pattern. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data CtxVal = CtxVal
  { deps :: Names,
    iterationType :: IterationType
  }

(><) :: Context -> CtxVal -> CtxVal -> CtxVal
(><) _ctx (CtxVal lnames _ltype) (CtxVal rnames rtype) =
  -- TODO: Do some lookups in context
  -- TODO: Consider: do we need to do some combination of ltype and rtype?
  CtxVal ((<>) lnames rnames) rtype

data BodyType
  = SegMapName SegMapName
  | LoopBodyName LoopBodyName
  | CondBodyName CondBodyName

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map VName CtxVal,
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    lastBodyType :: [BodyType],
    -- | Current level of recursion
    currentLevel :: Int
  }

-- | Get the last segmap encountered in the context.
lastSegMap :: Context -> Maybe SegMapName
lastSegMap (Context _ [] _) = Nothing
lastSegMap (Context _ bodies _) = safeLast $ getSegMaps bodies
  where
    getSegMaps =
      mapMaybe
        ( \body -> case body of
            (SegMapName segmap) -> Just segmap
            _ -> Nothing
        )

    safeLast [] = Nothing
    safeLast [x] = Just x
    safeLast (_ : xs) = safeLast xs

instance Monoid Context where
  mempty =
    Context
      { assignments = mempty,
        lastBodyType = [],
        currentLevel = 0
      }

instance Semigroup Context where
  (<>)
    (Context ass0 lastSegMap0 lvl0)
    (Context ass1 lastSegMap1 lvl1) =
      Context ((<>) ass0 ass1) ((++) lastSegMap0 lastSegMap1) $ max lvl0 lvl1

instance Semigroup DimIdxPat where
  (<>) :: DimIdxPat -> DimIdxPat -> DimIdxPat
  (<>)
    (DimIdxPat adeps atypes)
    (DimIdxPat bdeps btypes)
      | atypes == btypes =
          DimIdxPat ((<>) adeps bdeps) atypes
      | otherwise =
          error "Oh no!"

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context -> Context -> Context
extend = (<>)

oneContext :: VName -> CtxVal -> [BodyType] -> Context
oneContext name ctxValue segmaps =
  Context
    { assignments = M.singleton name ctxValue,
      lastBodyType = segmaps,
      currentLevel = 0
    }

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam _i p v = oneContext (paramName p) v []

-- | Create a singular context from a segspace
contextFromSegSpace :: SegMapName -> SegSpace -> Context
contextFromSegSpace segspaceName segspace =
  foldl' (\acc (name, _subexpr) -> extend acc $ oneContext name ctxVal []) mempty $
    unSegSpace segspace
  where
    ctxVal = CtxVal (oneName $ snd segspaceName) Parallel

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams iterType pats name =
  foldl extend mempty $
    map (\pat -> contextFromParam iterType pat name) pats

-- | Analyze each `entry` and accumulate the results.
analyzeDimIdxPats :: Prog GPU -> ArrayIndexDescriptors
analyzeDimIdxPats = foldMap analyzeFunction . progFuns

-- | Analyze each statement in a function body.
analyzeFunction :: FunDef GPU -> ArrayIndexDescriptors
analyzeFunction func =
  let stms = stmsToList . bodyStms $ funDefBody func
   in let ctx =
            contextFromParams Sequential (funDefParams func) $
              -- All entries are "sequential" in nature.
              CtxVal {deps = mempty, iterationType = Sequential}
       in snd $ analyzeStmsPrimitive ctx stms

-- | Analyze each statement in a list of statements.
analyzeStmsPrimitive :: Context -> [Stm GPU] -> (Context, ArrayIndexDescriptors)
analyzeStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> let (c', r') = analyzeStm c stm in (c', M.union r r'))
    (ctx, mempty)

-- | Same as analyzeStmsPrimitive, but change the resulting context into
-- a ctxVal, mapped to pattern.
analyzeStms :: Context -> Context -> ((Int, VName) -> BodyType) -> Pat dec -> [Stm GPU] -> (Context, ArrayIndexDescriptors)
analyzeStms ctx ctxExtended bodyConstructor pats body = do
  -- 0. Recurse into body with ctx
  let (ctx'', aids) = analyzeStmsPrimitive recContext body
  -- 1. We do not want the returned context directly.
  --    however, we do want pat to map to the names what was hit in body.
  --    therefore we need to subtract the old context from the returned one,
  --    and discard all the keys within it.
  let ctxVals = M.difference (assignments ctx'') (assignments recContext)
  -- 2. We are ONLY interested in the rhs of assignments (ie. the
  --    dependencies of pat :) )
  let ctx' = concatCtxVal . map snd $ M.toList ctxVals
  -- 3. Now we have the correct context and result
  (ctx', aids)
  where
    pat = patElemName . head $ patElems pats

    concatCtxVal [] = oneContext pat (CtxVal mempty $ getIterationType ctx) []
    concatCtxVal (ne : cvals) = oneContext pat (foldl' (ctx ><) ne cvals) []

    recContext =
      extend ctxExtended $
        Context
          { assignments = mempty,
            lastBodyType = [bodyConstructor (currentLevel ctx, pat)],
            currentLevel = currentLevel ctx + 1
          }

-- | Analyze a statement
analyzeStm :: Context -> Stm GPU -> (Context, ArrayIndexDescriptors)
analyzeStm ctx (Let pats _ e) =
  case e of
    (BasicOp (Index name (Slice ee))) -> analyzeIndex ctx pat name ee
    (BasicOp op) ->
      -- TODO: Lookup basicOp
      let ctx' = extend ctx $ oneContext pat (analyzeBasicOp ctx op) []
       in (ctx', mempty)
    (Match _subexps _cases _defaultBody _) -> error "UNHANDLED: Match"
    (Loop _bindings loop body) ->
      do
        let ctx' = case loop of
              (WhileLoop iterVar) ->
                oneContext iterVar (CtxVal (oneName pat) Sequential) []
              (ForLoop iterVar _ numIter _params) ->
                oneContext
                  iterVar
                  ((><) ctx (CtxVal (oneName pat) Sequential) (analyzeSubExpr ctx numIter))
                  []
        analyzeStms ctx ctx' LoopBodyName pats $ stmsToList $ bodyStms body
    (Apply _name _ _ _) -> error "UNHANDLED: Apply"
    (WithAcc _ _) -> error "UNHANDLED: With"
    (Op (SegOp op)) -> do
      -- TODO: Consider whether we want to treat SegMap, SegRed, SegScan, and
      -- SegHist the same way.
      let segSpaceContext =
            extend ctx $
              contextFromSegSpace (currentLevel ctx, pat) $
                segSpace op
      analyzeStms ctx segSpaceContext SegMapName pats . stmsToList . kernelBodyStms $ segBody op
    (Op (SizeOp sizeop)) -> do
      let subexprsToContext =
            extend ctx . concatCtxVal . map (analyzeSubExpr ctx)
      let ctx' =
            case sizeop of
              (CmpSizeLe _name _class subexp) -> subexprsToContext [subexp]
              (CalcNumGroups lsubexp _name rsubexp) -> subexprsToContext [lsubexp, rsubexp]
              _ -> ctx
      (ctx', mempty)
    (Op (GPUBody _ body)) ->
      analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body
    (Op (OtherOp _)) -> error "UNHANDLED: OtherOp"
  where
    pat = patElemName . head $ patElems pats

    concatCtxVal [] = mempty :: Context
    concatCtxVal (ne : cvals) = oneContext pat (foldl' (ctx ><) ne cvals) []

analyzeIndex :: Context -> VName -> VName -> [DimIndex SubExp] -> (Context, ArrayIndexDescriptors)
analyzeIndex ctx pat arr_name dimIndexes = do
  -- TODO: Expand context?
  -- TODO: Should we just take the latest segmap?
  let last_segmap = lastSegMap ctx
  let memory_entries = [mapMaybe f dimIndexes]
        where
          f dimIndex = case dimIndex of
            -- TODO: Get nest level ----------------------------------,
            -- TODO: Reduce "tmp" values to gtid's ----------------,  |
            (DimFix (Var v)) -> Just $ DimIdxPat (S.fromList [(0, (v, 0))]) $ getIterationType ctx
            (DimSlice offs n stride) -> Nothing -- TODO: How should we handle slices?
  let idx_expr_name = pat --                                         IndexExprName
  let map_ixd_expr = M.singleton idx_expr_name memory_entries --     IndexExprName |-> [MemoryEntry]
  let map_array = M.singleton arr_name map_ixd_expr -- ArrayName |-> IndexExprName |-> [MemoryEntry]
  let res = case last_segmap of --      SegMapName |-> ArrayName |-> IndexExprName |-> [MemoryEntry]
        Nothing -> mempty
        (Just segmap) -> M.singleton segmap map_array
  (ctx, res)

getIterationType :: Context -> IterationType
getIterationType (Context _ bodies _) =
  getIteration_rec bodies
  where
    getIteration_rec [] = Sequential
    getIteration_rec rec =
      case last rec of
        SegMapName _ -> Parallel
        LoopBodyName _ -> Sequential
        -- We can't really trust cond/match to be sequential/parallel, so
        -- recurse a bit ya kno
        CondBodyName _ -> getIteration_rec $ init rec

analyzeSubExpr :: Context -> SubExp -> CtxVal
analyzeSubExpr ctx (Constant _) = CtxVal mempty $ getIterationType ctx
analyzeSubExpr ctx (Var v) =
  case M.lookup v (assignments ctx) of
    Nothing -> error $ "Failed to lookup variable \"" ++ baseString v
    -- If variable is found, the dependenies must be the superset
    (Just (CtxVal deps _)) -> CtxVal (oneName v <> deps) $ getIterationType ctx

analyzeBasicOp :: Context -> BasicOp -> CtxVal
analyzeBasicOp ctx expression =
  case expression of
    (SubExp subexp) -> analyzeSubExpr ctx subexp
    (Opaque _ subexp) -> analyzeSubExpr ctx subexp
    (ArrayLit subexps _t) -> concatCtxVals mempty subexps
    (UnOp _ subexp) -> analyzeSubExpr ctx subexp
    (BinOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
    (CmpOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
    (ConvOp _ subexp) -> analyzeSubExpr ctx subexp
    (Assert subexp _ _) -> analyzeSubExpr ctx subexp
    (Index name _) ->
      error $ "unhandled: Index (Skill issue?) " ++ baseString name
    (Update _ name _slice _subexp) ->
      error $ "unhandled: Update (technically skill issue?)" ++ baseString name
    -- Technically, do we need this case?
    (Concat _ _ length_subexp) -> analyzeSubExpr ctx length_subexp
    (Manifest _dim _name) ->
      error "unhandled: Manifest"
    (Iota end_subexp start_subexp stride_subexp _) -> concatCtxVals mempty [end_subexp, start_subexp, stride_subexp]
    (Replicate (Shape shape_subexp) subexp) -> concatCtxVals mempty (subexp : shape_subexp)
    (Scratch _ subexprs) -> concatCtxVals mempty subexprs
    (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
    (Rearrange _ name) -> CtxVal (oneName name) $ getIterationType ctx
    (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
    _ -> error "unhandled: match-all"
  where
    concatCtxVals ne =
      foldl' (\a -> (><) ctx a . analyzeSubExpr ctx) (CtxVal ne $ getIterationType ctx)

-- Pretty printing

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      f ((_, name), maps) = "(segmap)" <+> pretty name <+> ": {" </> indent 4 (mapprintArray $ M.toList maps) </> "}"

      mapprintArray :: [(ArrayName, M.Map IndexExprName [MemoryEntry])] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap (name, maps) = "(arr)" <+> pretty (baseName name) <+> ": {" </> indent 4 (mapprintIdxExpr (M.toList maps)) </> "}"

      mapprintIdxExpr :: [(IndexExprName, [MemoryEntry])] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty (baseName name) <+> ":" </> indent 4 (printMemoryEntryList mems)

      printMemoryEntryList :: [MemoryEntry] -> Doc ann
      printMemoryEntryList [] = ""
      printMemoryEntryList [m] = printMemoryEntry m 0
      printMemoryEntryList (m : mm) = printMemoryEntry m 0 </> printMemoryEntryList mm

      printMemoryEntry :: MemoryEntry -> Int -> Doc ann
      printMemoryEntry [] _ = ""
      printMemoryEntry [m] idx = printDim m idx
      printMemoryEntry (m : mm) idx = printDim m idx </> printMemoryEntry mm (idx + 1)

      printDim m idx = pretty idx <+> ":" <+> indent 0 (pretty m)

instance Pretty DimIdxPat where
  pretty (DimIdxPat dependencies iterType) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    vsep
      [ "dependencies" <+> equals <+> align (prettyDeps dependencies),
        "iterType    " <+> equals <+> align (pretty iterType)
      ]
    where
      prettyDeps = encloseSep "[ " " ]" " | " . map (printPair . snd) . S.toList
      printPair (name, lvl) = pretty name <+> pretty lvl

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"
