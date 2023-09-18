module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    analyzeStm,
    ArrayIndexDescriptors,
    IterationType,
    Variance,
    ArrayName,
    SegMapName,
    IndexExprName,
    DimIdxPat,
  )
where

import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Futhark.IR.GPU
import Futhark.Util.Pretty

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

-- | Set of VNames of gtid's that some access is variant to
type Variance = S.IntMap (VName, Int)

type ArrayName = VName

type SegMapName = (Int, VName)

type IndexExprName = VName

-- | Collect all features of memory access together
data DimIdxPat = DimIdxPat
  { -- | Set of gtid's that the access is variant to.
    -- | Empty set means that the access is invariant.
    variances :: Variance,
    -- | Whether the acess is parallel or sequential
    iterType :: IterationType
  }
  deriving (Eq, Ord, Show)

isInv :: DimIdxPat -> Bool
isInv (DimIdxPat n _) = S.null n

isVar :: DimIdxPat -> Bool
isVar = not . isInv

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [DimIdxPat]

-- | We map variable names of arrays to lists of memory access patterns.
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

-- | Only used during the analysis to keep track of the dependencies of each
-- pattern. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data CtxVal = CtxVal
  { deps :: Names,
    iterationType :: IterationType
  }

-- | A mapping from patterns occuring in Let expressions to their corresponding
-- variance.
data Context = Context
  { assignments :: M.Map VName CtxVal,
    lastSegMap :: [SegMapName]
  }

instance Semigroup Context where
  (<>)
    (Context ass0 lastSegMap0)
    (Context ass1 lastSegMap1) =
      Context ((<>) ass0 ass1) ((++) lastSegMap0 lastSegMap1)

instance Semigroup DimIdxPat where
  (<>) :: DimIdxPat -> DimIdxPat -> DimIdxPat
  (<>)
    (DimIdxPat avars atypes)
    (DimIdxPat bvars btypes)
      | atypes == btypes =
          DimIdxPat ((<>) avars bvars) atypes
      | otherwise =
          error "REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context -> Context -> Context
extend = (<>)

oneContext :: VName -> CtxVal -> [SegMapName] -> Context
oneContext name ctxValue segmaps = Context {assignments = M.singleton name ctxValue, lastSegMap = segmaps}

zeroContext :: Context
zeroContext = Context {assignments = mempty, lastSegMap = []}

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam _i p v = oneContext (paramName p) v []

-- type t = loop | gpuOp | funcdef

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams i pp n =
  foldl (<>) zeroContext $
    map (\p -> contextFromParam i p n) pp

-- | TODO: CHANGE THIS COMMENT
-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeDimIdxPats :: Prog GPU -> ArrayIndexDescriptors
analyzeDimIdxPats = foldMap analyzeFunction . progFuns

analyzeFunction :: FunDef GPU -> ArrayIndexDescriptors
analyzeFunction func =
  let stms = stmsToList . bodyStms $ funDefBody func
   in let ctx =
            contextFromParams Sequential (funDefParams func) $
              -- All entries are "sequential" in nature.
              CtxVal {deps = mempty, iterationType = Sequential}
       in analyzeStms ctx stms

analyzeStms :: Context -> [Stm GPU] -> ArrayIndexDescriptors
analyzeStms ctx ((Let pats _aux expr) : stms) =
  let (stm_ctx, res) = analyzeStm ctx expr
   in let ctx' = maybe ctx (\v -> extend ctx $ oneContext (patElemName . head $ patElems pats) v []) stm_ctx
       in M.union res $ analyzeStms ctx' stms
analyzeStms _ _ [] = M.empty

analyzeStm :: Context -> Exp GPU -> (Maybe CtxVal, ArrayIndexDescriptors)
analyzeStm _ctx (BasicOp (Index _n (Slice _e))) = error "UNHANDLED: Index"
analyzeStm _ctx (BasicOp _) = error "UNHANDLED: BasicOp"
analyzeStm _ctx (Match _subexps _cases _defaultBody _) = error "UNHANDLED: Match"
analyzeStm _ctx (Loop _bindings _loop _body) = error "UNHANDLED: Loop"
analyzeStm _ctx (Apply _name _ _ _) = error "UNHANDLED: Apply"
analyzeStm _ctx (WithAcc _ _) = error "UNHANDLED: With"
analyzeStm ctx (Op (SegOp o)) = analyzeSegOp ctx o
analyzeStm _ctx (Op (SizeOp _)) = error "UNHANDLED: SizeOp"
analyzeStm _ctx (Op (GPUBody _ _)) = error "UNHANDLED: GPUBody"
analyzeStm _ctx (Op (OtherOp _)) = error "UNHANDLED: OtherOp"

analyzeSegOp :: Context -> SegOp lvl GPU -> (Maybe CtxVal, ArrayIndexDescriptors)
analyzeSegOp _ctx (SegMap _lvl _idxSpace _types _kbody) = error "case SegMap"
analyzeSegOp _ctx (SegRed _lvl _idxSpace _segOps _types _kbody) = error "case SegRed"
analyzeSegOp _ctx (SegScan _lvl _idxSpace _segOps _types _kbody) = error "case SegScan"
analyzeSegOp _ctx (SegHist _lvl _idxSpace _segHistOps _types _kbody) = error "case SegHist"

-- Pretty printing

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      mapprint :: [(ArrayName, e)] -> Doc ann
      mapprint [] = ""
      mapprint [m] = memoryEntryPrint m
      mapprint (m : mm) = memoryEntryPrint m </> mapprint mm

      -- memoryEntryPrint = hsep . map pretty
      memoryEntryPrint (name, _b) = pretty $ baseName name
      f (name, maps) = pretty name </> indent 2 (mapprint $ M.toList maps)

instance Pretty DimIdxPat where
  pretty (DimIdxPat variances iterType) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "[\n variances = " <+> pretty variances <+> "\n iterType:" <+> pretty iterType <+> "]"

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty = stack . map pretty . S.toList
