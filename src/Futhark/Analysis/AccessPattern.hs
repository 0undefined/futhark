{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimAccesss,
    analyzeFunction,
    vnameFromSegOp,
    analysisPropagateByTransitivity,
    isInvariant,
    Analyze,
    IndexTable,
    ArrayName,
    DimAccess (..),
    IndexExprName,
    BodyType (..),
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
    notImplementedYet,
    Context (..),
    analyzeIndex,
    VariableInfo (..),
    VarType (..),
    isCounter,
    Dependency (..),
  )
where

import Data.Either
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem
import Futhark.Util.Pretty

class Analyze rep where
  analyzeOp :: Op rep -> (Context rep -> [VName] -> (Context rep, IndexTable rep))

-- | Map patterns of Segmented operations on arrays, to index expressions with
-- their index descriptors.
-- segmap(pattern) → A(pattern) → indexExpressionName(pattern) → [DimAccess]
-- Each DimAccess element corresponds to an access to a given dimension
-- in the given array, in the same order of the dimensions.
type IndexTable rep =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName [DimAccess rep]))

-- | SegOpName stores the nested "level" at which it is declared in the AST.
data SegOpName
  = SegmentedMap {vnameFromSegOp :: VName}
  | SegmentedRed {vnameFromSegOp :: VName}
  | SegmentedScan {vnameFromSegOp :: VName}
  | SegmentedHist {vnameFromSegOp :: VName}
  deriving (Eq, Ord, Show)

type IndexExprName = VName

-- Stores the name of the array,
-- the "stack trace" where it was allocated at,
-- and the original layout.
-- The latter is currently unused, but can prove useful when throwing
-- transpositions into the mix.
type ArrayName = (VName, [BodyType], [Int])

data BodyType
  = SegOpName SegOpName
  | LoopBodyName VName
  | CondBodyName VName
  deriving (Show, Ord, Eq)

-- | Collect all features of access to a specific dimension of an array.
data DimAccess rep = DimAccess
  { -- | Set of VNames of gtid's that some access is variant to.
    -- An empty set indicates that the access is invariant.
    -- Tuple of patternName and nested `level` it index occurred at, as well as
    -- what the actual iteration type is.
    dependencies :: M.Map VName Dependency,
    originalVar :: Maybe VName
  }
  deriving (Eq, Show)

data Dependency = Dependency
  { lvl :: Int,
    varType :: VarType
  }
  deriving (Eq, Show)

instance Semigroup (DimAccess rep) where
  (<>) :: DimAccess rep -> DimAccess rep -> DimAccess rep
  adeps <> bdeps =
    DimAccess
      (dependencies adeps <> dependencies bdeps)
      ( case originalVar adeps of
          Nothing -> originalVar bdeps
          _ -> originalVar adeps
      )

instance Monoid (DimAccess rep) where
  mempty = DimAccess mempty Nothing

isInvariant :: DimAccess rep -> Bool
isInvariant = null . dependencies

unionIndexTables :: IndexTable rep -> IndexTable rep -> IndexTable rep
unionIndexTables = M.unionWith (M.unionWith M.union)

-- | Make segops on arrays transitive, ie. if
-- > let A = segmap (..) xs -- A indexes into xs
-- > let B = segmap (..) A  -- B indexes into A
-- Then B also derives all A's array-accesses, like xs.
-- Runs in n²
analysisPropagateByTransitivity :: IndexTable rep -> IndexTable rep
analysisPropagateByTransitivity idxTable =
  M.map
    foldlArrayNameMap
    idxTable
  where
    -- VName -> M.Map ArrayName (M.Map IndexExprName ([DimAccess rep]))
    aggregateResults arrayName =
      maybe
        mempty
        foldlArrayNameMap
        ((M.!?) (M.mapKeys vnameFromSegOp idxTable) arrayName)

    foldlArrayNameMap aMap =
      foldl (M.unionWith M.union) aMap (map aggregateResults $ M.keys $ M.mapKeys (\(a, _, _) -> a) aMap)

--
-- Helper types and functions to perform the analysis.
--

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context rep = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map VName (VariableInfo rep),
    -- | Maps from sliced arrays to their respective access patterns.
    slices :: M.Map IndexExprName (ArrayName, [VName], [DimAccess rep]),
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    parents :: [BodyType],
    -- | Current level of recursion
    currentLevel :: Int
  }
  deriving (Show, Eq)

instance Monoid (Context rep) where
  mempty =
    Context
      { assignments = mempty,
        slices = mempty,
        parents = [],
        currentLevel = 0
      }

instance Semigroup (Context rep) where
  (<>)
    (Context ass0 slices0 lastBody0 lvl0)
    (Context ass1 slices1 lastBody1 lvl1) =
      Context
        ((<>) ass0 ass1)
        ((<>) slices0 slices1)
        ((++) lastBody0 lastBody1)
        $ max lvl0 lvl1

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context rep -> Context rep -> Context rep
extend = (<>)

allSegMap :: Context rep -> [SegOpName]
allSegMap (Context _ _ parents _) =
  mapMaybe
    ( \case
        (SegOpName o) -> Just o
        _ -> Nothing
    )
    parents

-- | Context Value (VariableInfo) is the type used in the context to categorize
-- assignments. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data VariableInfo rep = VariableInfo
  { deps :: Names,
    level :: Int,
    parents_nest :: [BodyType],
    variableType :: VarType
  }
  deriving (Show, Eq)

data VarType
  = ConstType
  | Variable
  | ThreadID
  | LoopVar
  deriving (Show, Eq)

isCounter :: VarType -> Bool
isCounter LoopVar = True
isCounter ThreadID = True
isCounter _ = False

varInfoFromNames :: Context rep -> Names -> VariableInfo rep
varInfoFromNames ctx names = do
  VariableInfo
    names
    (currentLevel ctx)
    (parents ctx)
    Variable

-- | Wrapper around the constructur of Context.
oneContext :: VName -> VariableInfo rep -> Context rep
oneContext name varInfo =
  Context
    { assignments = M.singleton name varInfo,
      slices = mempty,
      parents = [],
      currentLevel = 0
    }

-- | Create a singular varInfo with no dependencies.
varInfoZeroDeps :: Context rep -> VariableInfo rep
varInfoZeroDeps ctx =
  VariableInfo
    mempty
    (currentLevel ctx)
    (parents ctx)
    Variable -- variable is a very common type

-- | Create a singular context from a segspace
contextFromNames :: Context rep -> VariableInfo rep -> [VName] -> Context rep
contextFromNames ctx varInfo = foldl' extend ctx . map (`oneContext` varInfo)

-- | Analyze each `entry` and accumulate the results.
analyzeDimAccesss :: (Analyze rep) => Prog rep -> IndexTable rep
analyzeDimAccesss = foldMap' analyzeFunction . progFuns

-- | Analyze each statement in a function body.
analyzeFunction :: forall rep. (Analyze rep) => FunDef rep -> IndexTable rep
analyzeFunction func = do
  let stms = stmsToList . bodyStms $ funDefBody func
  -- Create a context containing the function parameters
  let ctx = contextFromNames mempty (varInfoZeroDeps ctx) $ map paramName $ funDefParams func
  snd $ analyzeStmsPrimitive ctx stms

-- | Analyze each statement in a list of statements.
analyzeStmsPrimitive :: (Analyze rep) => Context rep -> [Stm rep] -> (Context rep, IndexTable rep)
analyzeStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> onSnd (unionIndexTables r) $ analyzeStm c stm)
    (ctx, mempty)

-- | Same as analyzeStmsPrimitive, but change the resulting context into
-- a varInfo, mapped to pattern.
analyzeStms :: (Analyze rep) => Context rep -> (VName -> BodyType) -> [VName] -> [Stm rep] -> (Context rep, IndexTable rep)
analyzeStms ctx bodyConstructor pats body = do
  -- 0. Recurse into body with ctx
  let (ctx'', indexTable) = analyzeStmsPrimitive recContext body

  -- 0.1 Get all new slices
  let slices_new = M.difference (slices ctx'') (slices ctx)
  -- 0.2 Make "IndexExpressions" of the slices
  let slices_indices =
        foldl unionIndexTables indexTable
          $ mapMaybe
            ( uncurry $ \_idx_expression (array_name, patterns, dim_indices) ->
                -- This might crash some examples? (from undefined above)
                Just $
                  snd $
                    analyzeIndex'
                      -- Should we use recContex instead of ctx''?
                      ctx''
                      patterns
                      array_name
                      dim_indices
            )
          $ M.toList slices_new

  -- 1. We do not want the returned context directly.
  --    however, we do want pat to map to the names what was hit in body.
  --    therefore we need to subtract the old context from the returned one,
  --    and discard all the keys within it.
  let inScopeDependenciesFromBody =
        rmOutOfScopeDeps ctx'' $
          M.difference (assignments ctx'') (assignments recContext)

  -- 2. We are ONLY interested in the rhs of assignments (ie. the
  --    dependencies of pat :) )
  let ctx' = foldl extend ctx $ concatVariableInfo inScopeDependenciesFromBody
  -- 3. Now we have the correct context and result
  (ctx' {parents = parents ctx, currentLevel = currentLevel ctx, slices = slices ctx}, slices_indices)
  where
    -- Extracts and merges `Names` in `VariableInfo`s, and makes a new VariableInfo. This
    -- MAY throw away needed information, but it was my best guess at a solution
    -- at the time of writing.
    concatVariableInfo dependencies =
      map (\pat -> oneContext pat (varInfoFromNames ctx dependencies)) pats

    -- Context used for "recursion" into analyzeStmsPrimitive
    recContext =
      ctx
        { parents = parents ctx <> concatMap (\pat -> [bodyConstructor pat]) pats,
          currentLevel = currentLevel ctx + 1
        }

    -- Recursively looks up dependencies, until they're in scope or empty set.
    rmOutOfScopeDeps :: Context rep -> M.Map VName (VariableInfo rep) -> Names
    rmOutOfScopeDeps ctx' newAssignments = do
      let throwawayAssignments = assignments ctx'
      let localAssignments = assignments ctx
      M.foldlWithKey
        ( \result a varInfo ->
            let dependencies = deps varInfo
             in -- if the VName of the assignment exists in the context, we are good
                if a `M.member` localAssignments
                  then result <> oneName a
                  else -- Otherwise, recurse on its dependencies;
                  -- 0. Add dependencies in ctx to result

                    let (depsInCtx, depsNotInCtx) =
                          partitionEithers
                            $ map
                              ( \d ->
                                  if d `M.member` localAssignments
                                    then Left d
                                    else Right d
                              )
                            $ namesToList dependencies
                     in let depsNotInCtx' =
                              M.fromList $
                                mapMaybe
                                  ( \d -> case M.lookup d throwawayAssignments of
                                      Just varInfo' -> Just (d, varInfo')
                                      _ -> Nothing
                                  )
                                  depsNotInCtx
                         in result
                              <> namesFromList depsInCtx
                              <> rmOutOfScopeDeps
                                ctx'
                                depsNotInCtx'
        )
        mempty
        newAssignments

getDeps :: SubExp -> Names
getDeps subexp =
  case subexp of
    (Var v) -> oneName v
    (Constant _) -> mempty

-- | Analyze a rep statement and return the updated context and array index
-- descriptors.
analyzeStm :: (Analyze rep) => Context rep -> Stm rep -> (Context rep, IndexTable rep)
analyzeStm ctx (Let pats _ e) = do
  -- Get the name of the first element in a pattern
  let patternNames = map patElemName $ patElems pats

  -- Construct the result and Context from the subexpression. If the subexpression
  -- is a body, we recurse into it.
  case e of
    (BasicOp (Index name (Slice dimSubexp))) -> analyzeIndex ctx patternNames name dimSubexp
    (BasicOp (Update _ name (Slice dimSubexp) _subexp)) -> analyzeIndex ctx patternNames name dimSubexp
    (BasicOp op) -> analyzeBasicOp ctx op patternNames
    (Match conds cases defaultBody _) -> analyzeMatch (contextFromNames ctx (varInfoZeroDeps ctx) $ concatMap (namesToList . getDeps) conds) patternNames defaultBody $ map caseBody cases
    (Loop bindings loop body) -> analyzeLoop ctx bindings loop body patternNames
    (Apply _name diets _ _) -> analyzeApply ctx patternNames diets
    (WithAcc _ _) -> (ctx, mempty) -- ignored
    (Op op) -> analyzeOp op ctx patternNames

-- If left, this is just a regular index. If right, a slice happened.
getIndexDependencies :: Context rep -> [DimIndex SubExp] -> Either [DimAccess rep] [DimAccess rep]
getIndexDependencies ctx dims =
  fst $
    foldr
      ( \idx (a, i) -> do
          let acc =
                either
                  (matchDimIndex idx)
                  (forceRight . matchDimIndex idx)
                  a
          (acc, i - 1)
      )
      (Left [], length dims - 1)
      dims
  where
    matchDimIndex idx accumulator =
      case idx of
        (DimFix subExpression) ->
          Left $ consolidate ctx subExpression : accumulator
        -- If we encounter a DimSlice, add it to a map of `DimSlice`s and check
        -- result later.
        (DimSlice offset num_elems stride) -> do
          -- We assume that a slice is iterated sequentially, so we have to
          -- create a fake dependency for the slice.
          let dimAccess' = DimAccess (M.singleton (VName "slice" 0) $ Dependency (currentLevel ctx) LoopVar) (Just $ VName "slice" 0)
          let cons = consolidate ctx
          let dimAccess = dimAccess' <> cons offset <> cons num_elems <> cons stride
          Right $ dimAccess : accumulator

    forceRight (Left a) = Right a
    forceRight (Right a) = Right a

-- | Gets the dependencies of each dimension and either returns a result, or
-- adds a slice to the context.
analyzeIndex :: Context rep -> [VName] -> VName -> [DimIndex SubExp] -> (Context rep, IndexTable rep)
analyzeIndex ctx pats arr_name dimIndices = do
  -- Get the dependendencies of each dimension
  let dependencies = getIndexDependencies ctx dimIndices
  -- Extend the current context with current pattern(s) and its deps
  let ctx' = analyzeIndexContextFromIndices ctx dimIndices pats

  -- The bodytype(s) are used in the result construction
  let array_name' =
        -- For now, we assume the array is in row-major-order, hence the
        -- identity permutation. In the future, we might want to infer its
        -- layout, for example, if the array is the result of a transposition.
        let layout = [0 .. length dimIndices - 1]
         in -- 2. If the arrayname was not in assignments, it was not an immediately
            --    allocated array.
            fromMaybe (arr_name, [], layout)
              -- 1. Maybe find the array name, and the "stack" of body types that the
              -- array was allocated in.
              . L.find (\(n, _, _) -> n == arr_name)
              -- 0. Get the "stack" of bodytypes for each assignment
              $ map (\(n, vi) -> (n, parents_nest vi, layout)) (M.toList $ assignments ctx')

  either
    (index ctx' array_name')
    (slice ctx' array_name')
    dependencies
  where
    slice :: Context rep -> ArrayName -> [DimAccess rep] -> (Context rep, IndexTable rep)
    slice context array_name dims =
      (context {slices = M.insert (head pats) (array_name, pats, dims) $ slices context}, mempty)

    index :: Context rep -> ArrayName -> [DimAccess rep] -> (Context rep, IndexTable rep)
    index context arrayName dimAccess =
      let (name, _, _) = arrayName
       in -- If the arrayname is a `DimSlice` we want to fixup the access
          case M.lookup name $ slices context of
            Nothing -> analyzeIndex' context pats arrayName dimAccess
            Just (arrayName', pats', sliceAccess) -> do
              analyzeIndex'
                context
                pats'
                arrayName'
                (init sliceAccess ++ [head dimAccess <> last sliceAccess] ++ drop 1 dimAccess)

analyzeIndexContextFromIndices :: Context rep -> [DimIndex SubExp] -> [VName] -> Context rep
analyzeIndexContextFromIndices ctx dimAccesses pats = do
  let subExprs =
        mapMaybe
          ( \case
              (DimFix subExpression) -> case subExpression of
                (Var v) -> Just v
                (Constant _) -> Nothing
              (DimSlice _offs _n _stride) -> Nothing
          )
          dimAccesses

  -- Add each non-constant DimIndex as a dependency to the index expression
  let varInfo = varInfoFromNames ctx $ namesFromList subExprs

  -- Extend context with the dependencies index expression
  foldl' extend ctx $ map (`oneContext` varInfo) pats

analyzeIndex' :: Context rep -> [VName] -> ArrayName -> [DimAccess rep] -> (Context rep, IndexTable rep)
analyzeIndex' ctx _ _ [] = (ctx, mempty)
analyzeIndex' ctx _ _ [_] = (ctx, mempty)
analyzeIndex' ctx pats arr_name dimAccesses = do
  -- Get the name of all segmaps in the current "callstack"
  let segmaps = allSegMap ctx
  let memory_entries = dimAccesses
  let idx_expr_name = pats --                                                IndexExprName
  -- For each pattern, create a mapping to the dimensional indices
  let map_ixd_expr = map (`M.singleton` memory_entries) idx_expr_name --     IndexExprName |-> [DimAccess]
  -- For each pattern -> [DimAccess] mapping, create a mapping from the array
  -- name that was indexed.
  let map_array = map (M.singleton arr_name) map_ixd_expr --   ArrayName |-> IndexExprName |-> [DimAccess]
  -- ∀ (arrayName -> IdxExp -> [DimAccess]) mappings, create a mapping from all
  -- segmaps in current callstack (segThread & segGroups alike).
  let results = concatMap (\ma -> map (`M.singleton` ma) segmaps) map_array

  let res = foldl' unionIndexTables mempty results
  (ctx, res)

analyzeBasicOp :: Context rep -> BasicOp -> [VName] -> (Context rep, IndexTable rep)
analyzeBasicOp ctx expression pats = do
  -- Construct a VariableInfo from the subexpressions
  let ctx_val = case expression of
        (SubExp subexp) -> varInfoFromSubExpr subexp
        (Opaque _ subexp) -> varInfoFromSubExpr subexp
        (ArrayLit subexps _t) -> concatVariableInfos mempty subexps
        (UnOp _ subexp) -> varInfoFromSubExpr subexp
        (BinOp _ lsubexp rsubexp) -> concatVariableInfos mempty [lsubexp, rsubexp]
        (CmpOp _ lsubexp rsubexp) -> concatVariableInfos mempty [lsubexp, rsubexp]
        (ConvOp _ subexp) -> varInfoFromSubExpr subexp
        (Assert subexp _ _) -> varInfoFromSubExpr subexp
        (Index name _) ->
          error $ "unhandled: Index (This should NEVER happen) into " ++ prettyString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (This should NEVER happen) onto " ++ prettyString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> varInfoFromSubExpr length_subexp
        (Manifest _dim name) -> varInfoFromNames ctx $ oneName name
        (Iota end start stride _) -> concatVariableInfos mempty [end, start, stride]
        (Replicate (Shape shape) value') -> concatVariableInfos mempty (value' : shape)
        (Scratch _ subexprs) -> concatVariableInfos mempty subexprs
        (Reshape _ (Shape shape_subexp) name) -> concatVariableInfos (oneName name) shape_subexp
        (Rearrange _ name) -> varInfoFromNames ctx $ oneName name
        (UpdateAcc name lsubexprs rsubexprs) -> concatVariableInfos (oneName name) (lsubexprs ++ rsubexprs)
        (FlatIndex name _) -> varInfoFromNames ctx $ oneName name
        (FlatUpdate name _ source) -> varInfoFromNames ctx $ namesFromList [name, source]
  let ctx' = foldl' extend ctx $ map (`oneContext` ctx_val) pats
  (ctx', mempty)
  where
    concatVariableInfos ne nn =
      varInfoFromNames
        ctx
        (foldl' (\a -> (<>) a . analyzeSubExpr pats ctx) ne nn)

    varInfoFromSubExpr (Constant _) = (varInfoFromNames ctx mempty) {variableType = ConstType}
    varInfoFromSubExpr (Var v) =
      case M.lookup v (assignments ctx) of
        (Just _) -> (varInfoFromNames ctx $ oneName v) {variableType = Variable}
        Nothing ->
          error $
            "Failed to lookup variable \""
              ++ prettyString v
              ++ "\npat: "
              ++ prettyString pats
              ++ "\n\nContext\n"
              ++ show ctx

analyzeMatch :: (Analyze rep) => Context rep -> [VName] -> Body rep -> [Body rep] -> (Context rep, IndexTable rep)
analyzeMatch ctx pats body parents =
  let ctx'' = ctx {currentLevel = currentLevel ctx - 1}
   in foldl
        ( \(ctx', res) b ->
            -- This Little Maneuver's Gonna Cost Us 51 Years
            onFst constLevel
              . onSnd (unionIndexTables res)
              . analyzeStms ctx' CondBodyName pats
              . stmsToList
              $ bodyStms b
        )
        (ctx'', mempty)
        (body : parents)
  where
    constLevel context = context {currentLevel = currentLevel ctx - 1}

analyzeLoop :: (Analyze rep) => Context rep -> [(FParam rep, SubExp)] -> LoopForm -> Body rep -> [VName] -> (Context rep, IndexTable rep)
analyzeLoop ctx bindings loop body pats = do
  let nextLevel = currentLevel ctx
  let ctx'' = ctx {currentLevel = nextLevel}
  let ctx' =
        contextFromNames ctx'' ((varInfoZeroDeps ctx) {variableType = LoopVar}) $
          case loop of
            (WhileLoop iterVar) -> iterVar : map (paramName . fst) bindings
            (ForLoop iterVar _ _) -> iterVar : map (paramName . fst) bindings

  -- Extend context with the loop expression
  analyzeStms ctx' LoopBodyName pats $ stmsToList $ bodyStms body

analyzeApply :: Context rep -> [VName] -> [(SubExp, Diet)] -> (Context rep, IndexTable rep)
analyzeApply ctx pats diets =
  onFst
    ( \ctx' ->
        foldl' extend ctx' $ map (\pat -> oneContext pat $ varInfoFromNames ctx' $ mconcat $ map (getDeps . fst) diets) pats
    )
    (ctx, mempty)

segOpType :: SegOp lvl rep -> VName -> SegOpName
segOpType (SegMap {}) = SegmentedMap
segOpType (SegRed {}) = SegmentedRed
segOpType (SegScan {}) = SegmentedScan
segOpType (SegHist {}) = SegmentedHist

analyzeSegOp :: (Analyze rep) => SegOp lvl rep -> Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeSegOp op ctx pats = do
  let nextLevel = currentLevel ctx + length (unSegSpace $ segSpace op) - 1
  let ctx' = ctx {currentLevel = nextLevel}
  let segSpaceContext =
        foldl' extend ctx'
          . map (\(n, i) -> oneContext n $ VariableInfo mempty (currentLevel ctx + i) (parents ctx') ThreadID)
          . (\segspaceParams -> zip segspaceParams [0 ..])
          -- contextFromNames ctx' Parallel
          . map fst
          . unSegSpace
          $ segSpace op
  -- Analyze statements in the SegOp body
  analyzeStms segSpaceContext (SegOpName . segOpType op) pats . stmsToList . kernelBodyStms $ segBody op

analyzeSizeOp :: SizeOp -> Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeSizeOp op ctx pats = do
  let subexprsToContext =
        contextFromNames ctx (varInfoZeroDeps ctx)
          . concatMap (namesToList . analyzeSubExpr pats ctx)
  let ctx' = case op of
        (CmpSizeLe _name _class subexp) -> subexprsToContext [subexp]
        (CalcNumGroups lsubexp _name rsubexp) -> subexprsToContext [lsubexp, rsubexp]
        _ -> ctx
  -- Add sizeOp to context
  let ctx'' = foldl' extend ctx' $ map (\pat -> oneContext pat $ (varInfoZeroDeps ctx) {parents_nest = parents ctx'}) pats
  (ctx'', mempty)

-- | Analyze statements in a rep body.
analyzeGPUBody :: (Analyze rep) => Body rep -> Context rep -> (Context rep, IndexTable rep)
analyzeGPUBody body ctx =
  analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body

analyzeOtherOp :: Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeOtherOp ctx _ = (ctx, mempty)

-- | Returns an intmap of names, to be used as dependencies in construction of
-- VariableInfos.
-- Throws an error if SubExp contains a name not in context. This behaviour
-- might be thrown out in the future, as it is mostly just a very verbose way to
-- ensure that we capture all necessary variables in the context at the moment
-- of development.
analyzeSubExpr :: [VName] -> Context rep -> SubExp -> Names
analyzeSubExpr _ _ (Constant _) = mempty
analyzeSubExpr pp ctx (Var v) =
  case M.lookup v (assignments ctx) of
    (Just _) -> oneName v
    Nothing ->
      error $
        "Failed to lookup variable \""
          ++ prettyString v
          ++ "\npat: "
          ++ prettyString pp
          ++ "\n\nContext\n"
          ++ show ctx

-- | Reduce a DimFix into its set of dependencies
consolidate :: Context rep -> SubExp -> DimAccess rep
consolidate _ (Constant _) = mempty
consolidate ctx (Var v) = DimAccess (reduceDependencies ctx v) (Just v)

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context rep -> VName -> M.Map VName Dependency
reduceDependencies ctx v =
  case M.lookup v (assignments ctx) of
    Nothing -> error $ "Unable to find " ++ prettyString v
    Just (VariableInfo deps lvl _parents t) ->
      -- We detect whether it is a threadID or loop counter by checking
      -- whether or not it has any dependencies
      case t of
        ThreadID -> M.fromList [(v, Dependency lvl t)]
        LoopVar -> M.fromList [(v, Dependency lvl t)]
        Variable -> mconcat $ map (reduceDependencies ctx) $ namesToList deps
        ConstType -> mempty

-- Misc functions

-- | Apply `f` to first/left part of tuple.
onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

-- | Apply `f` to second/right part of tuple.
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

-- Instances for AST types that we actually support
instance Analyze GPU where
  analyzeOp gpuOp
    | (SegOp op) <- gpuOp = analyzeSegOp op
    | (SizeOp op) <- gpuOp = analyzeSizeOp op
    | (GPUBody _ body) <- gpuOp = pure . analyzeGPUBody body
    | (Futhark.IR.GPU.OtherOp _) <- gpuOp = analyzeOtherOp

instance Analyze GPUMem where
  analyzeOp _ = error $ notImplementedYet "GPUMem"

instance Analyze MC where
  analyzeOp mcOp
    | ParOp Nothing seqSegop <- mcOp = analyzeSegOp seqSegop
    | ParOp (Just segop) seqSegop <- mcOp = \ctx name -> do
        let (ctx', res') = analyzeSegOp segop ctx name
        let (ctx'', res'') = analyzeSegOp seqSegop ctx name
        (ctx' <> ctx'', unionIndexTables res' res'')
    | Futhark.IR.MC.OtherOp _ <- mcOp = analyzeOtherOp

instance Analyze MCMem where
  analyzeOp _mcOp = error "Unexpected?"

instance Analyze Seq where
  analyzeOp _ = error $ notImplementedYet "Seq"

instance Analyze SeqMem where
  analyzeOp _ = error $ notImplementedYet "SeqMem"

instance Analyze SOACS where
  analyzeOp _ = error $ notImplementedYet "SOACS"

notImplementedYet :: String -> String
notImplementedYet s = "Access pattern analysis for the " ++ s ++ " backend is not implemented."

instance Pretty (IndexTable rep) where
  pretty = stack . map f . M.toList :: IndexTable rep -> Doc ann
    where
      f (segop, arrayNameToIdxExprMap) = pretty segop <+> colon <+> g arrayNameToIdxExprMap

      g maps = lbrace </> indent 4 (mapprintArray $ M.toList maps) </> rbrace

      mapprintArray :: [(ArrayName, M.Map IndexExprName [DimAccess rep])] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap :: (ArrayName, M.Map IndexExprName [DimAccess rep]) -> Doc ann
      printArrayMap ((name, _, layout), maps) =
        "(arr)"
          <+> pretty name
          <+> colon
          <+> pretty layout
          <+> lbrace
          </> indent 4 (mapprintIdxExpr (M.toList maps))
          </> rbrace

      mapprintIdxExpr :: [(IndexExprName, [DimAccess rep])] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty name <+> ":" </> indent 4 (printDimAccess mems)

      printDimAccess :: [DimAccess rep] -> Doc ann
      printDimAccess dimAccesses = stack $ zipWith (curry printDim) [0 ..] dimAccesses

      printDim :: (Int, DimAccess rep) -> Doc ann
      printDim (i, m) = pretty i <+> ":" <+> indent 0 (pretty m)

instance Pretty (DimAccess rep) where
  pretty dimAccess =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    if case originalVar dimAccess of
      Nothing -> True
      Just n ->
        length (dependencies dimAccess) == 1 && n == head (map fst $ M.toList $ dependencies dimAccess)
        -- Only print the original name if it is different from the first (and single) dependency
      then "dependencies" <+> equals <+> align (prettyDeps $ dependencies dimAccess)
      else "dependencies" <+> equals <+> pretty (originalVar dimAccess) <+> "->" <+> align (prettyDeps $ dependencies dimAccess)
    where
      prettyDeps = braces . commasep . map printPair . M.toList
      printPair (name, Dependency lvl vtype) = pretty name <+> pretty lvl <+> pretty vtype

instance Pretty SegOpName where
  pretty (SegmentedMap name) = "(segmap)" <+> pretty name
  pretty (SegmentedRed name) = "(segred)" <+> pretty name
  pretty (SegmentedScan name) = "(segscan)" <+> pretty name
  pretty (SegmentedHist name) = "(seghist)" <+> pretty name

instance Pretty BodyType where
  pretty (SegOpName (SegmentedMap name)) = pretty name <+> colon <+> "segmap"
  pretty (SegOpName (SegmentedRed name)) = pretty name <+> colon <+> "segred"
  pretty (SegOpName (SegmentedScan name)) = pretty name <+> colon <+> "segscan"
  pretty (SegOpName (SegmentedHist name)) = pretty name <+> colon <+> "seghist"
  pretty (LoopBodyName name) = pretty name <+> colon <+> "loop"
  pretty (CondBodyName name) = pretty name <+> colon <+> "cond"

instance Pretty VarType where
  pretty ConstType = "const"
  pretty Variable = "var"
  pretty ThreadID = "tid"
  pretty LoopVar = "iter"
