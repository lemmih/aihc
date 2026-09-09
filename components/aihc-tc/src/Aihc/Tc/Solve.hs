-- | Top-level constraint solver.
--
-- The solver uses the worklist/inert-set architecture from OutsideIn(X):
--
-- @
-- while worklist is non-empty:
--   pop constraint from worklist
--   canonicalize it
--   attempt to solve
--   either:
--     - solve it (fill evidence)
--     - add to inert set
--     - emit new work items
-- @
module Aihc.Tc.Solve
  ( solveConstraints,
    solveWithImpls,
    SolveResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Canonicalize
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Dict (DictResult (..), reportUnsolvedDict, solveDict, solveDictWithGivens)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.Family (reducePredFamilies)
import Aihc.Tc.Solve.InertSet (InertSet (..), addInertDict, addInertEq, emptyInertSet)
import Aihc.Tc.Solve.Worklist
import Aihc.Tc.Types (Pred (..), TcKinds, TcType (..), TyVarId, Unique, mkAppTy)
import Aihc.Tc.Zonk (zonkPred, zonkType)

-- | Result of solving constraints.
data SolveResult = SolveResult
  { -- | Residual unsolved constraints.
    srResidual :: ![Ct],
    -- | Final inert set.
    srInerts :: !InertSet
  }
  deriving (Show)

-- | Solve a list of wanted constraints.
solveConstraints :: [Ct] -> TcM SolveResult
solveConstraints wanteds = solveWithImpls wanteds []

-- | Solve wanted constraints together with implication constraints.
solveWithImpls :: [Ct] -> [Implication] -> TcM SolveResult
solveWithImpls wanteds impls = do
  let wl0 = foldr addWork emptyWorkList wanteds
      wl = foldr addImpl wl0 impls
  solveLoop wl emptyInertSet

-- | Add a constraint to the appropriate bucket in the worklist.
addWork :: Ct -> WorkList -> WorkList
addWork ct = case ctPred ct of
  EqPred {} -> addEq ct
  ClassPred {} -> addDict ct
  QuantifiedPred {} -> addDict ct
  IParamPred {} -> addDict ct

-- | Main solver loop.
solveLoop :: WorkList -> InertSet -> TcM SolveResult
solveLoop wl inerts = case popWork wl of
  Nothing
    | null (inertEqs inerts) ->
        -- Done: all constraints processed.
        pure SolveResult {srResidual = [], srInerts = inerts}
    | otherwise -> do
        -- An equality that waits on a type family application gets another
        -- attempt when a solved meta variable changed it. Otherwise it is a
        -- residual that the enclosing scope solves or reports.
        (progressed, stuck) <- partitionProgress (inertEqs inerts)
        if null progressed
          then pure SolveResult {srResidual = stuck, srInerts = inerts {inertEqs = []}}
          else solveLoop (foldr addEq emptyWorkList progressed) inerts {inertEqs = stuck}
  Just (Left ct, wl') ->
    -- Process a flat constraint.
    processConstraint ct wl' inerts
  Just (Right impl, wl') -> do
    -- Solve the implication by using its given constraints to satisfy wanteds.
    -- A wanted that is stuck on a meta variable of the enclosing scope waits
    -- in the inert set for the enclosing solve.
    deferred <- solveImplication impl
    solveLoop wl' (foldr addInertDict inerts deferred)

-- | Split the stuck equalities into those that a solved meta variable
-- changed since they got stuck, and those that are unchanged.
partitionProgress :: [Ct] -> TcM ([Ct], [Ct])
partitionProgress stuckCts = do
  results <- mapM progress stuckCts
  pure ([ct | (ct, True) <- results], [ct | (ct, False) <- results])
  where
    progress ct = do
      predicate <- zonkPred (ctPred ct) >>= reducePredFamilies
      pure (ct {ctPred = predicate}, predicate /= ctPred ct)

-- | Process a single constraint from the worklist.
processConstraint :: Ct -> WorkList -> InertSet -> TcM SolveResult
processConstraint ct wl inerts = do
  predicate <- zonkPred (ctPred ct) >>= reducePredFamilies
  case predicate of
    -- Keep the parent evidence variable for the complete equality.
    EqPred {} -> do
      (wl', inerts') <- processEq (wl, inerts) (ct {ctPred = predicate})
      solveLoop wl' inerts'
    _ -> processCanonical wl inerts (canonicalize (ct {ctPred = predicate}))

processCanonical :: WorkList -> InertSet -> CanonResult -> TcM SolveResult
processCanonical wl inerts result = case result of
  CanonSolved ->
    -- Trivially solved (e.g. reflexive equality).
    solveLoop wl inerts
  CanonEqs subCts -> do
    -- Try to solve each sub-constraint.
    (wl', inerts') <- foldM processEq (wl, inerts) subCts
    solveLoop wl' inerts'
  CanonDict dictCt -> do
    -- Try to solve dictionary constraint.
    dictResult <- solveDict dictCt
    case dictResult of
      DictSolved -> solveLoop wl inerts
      DictStuck stuckCt ->
        -- Leave in inert set for now.
        solveLoop wl (addInertDict stuckCt inerts)

-- | Process an equality constraint.
processEq :: (WorkList, InertSet) -> Ct -> TcM (WorkList, InertSet)
processEq (wl, inerts) ct = do
  result <- solveEquality ct
  case result of
    EqSolved -> pure (wl, inerts)
    EqStuck stuckCt ->
      -- Cannot solve yet. The loop retries it when the worklist is empty.
      pure (wl, addInertEq stuckCt inerts)
    EqError errCt -> do
      -- Report the error.
      case ctPred errCt of
        EqPred t1 t2 ->
          emitError (ctLoc errCt) . UnificationError t1 t2 (ctOrigin errCt) =<< zonkCtEqProvenance errCt
        p ->
          emitError (ctLoc errCt) (UnsolvedWanted p (ctOrigin errCt))
      pure (wl, inerts)

-- | Solve an implication constraint.
--
-- The implication's given constraints (from GADT pattern matches) are
-- canonicalized into atomic equalities, which are then used as rewrite
-- rules to solve the implication's wanted constraints.
-- | Solve the wanteds of an implication. The result holds the dictionary
-- wanteds that are stuck on a meta variable of the enclosing scope and do
-- not mention a skolem of the implication.
solveImplication :: Implication -> TcM [Ct]
solveImplication impl = do
  outerPredicates <- getGivenPredicates
  let rawGivens = implGivenCts impl
      wanteds = implWantedCts impl
      givenPredicates = outerPredicates <> map ctPred rawGivens
  -- Canonicalize the given equalities by structural decomposition.
  givenEqs <- concat <$> mapM canonicalizeGiven rawGivens
  -- Equalities refine the types mentioned by dictionary wanteds, so preserve
  -- the main worklist's equality-before-dictionary ordering inside branches.
  let (equalityWanteds, dictionaryWanteds) = partitionWanteds wanteds
      skolems = implSkols impl
  deferredEqualities <- solveImplicationEqualities skolems givenPredicates givenEqs equalityWanteds
  deferredDictionaries <- concat <$> mapM (solveWantedWithGivens skolems givenPredicates givenEqs) dictionaryWanteds
  pure (deferredEqualities <> deferredDictionaries)

-- | Retry equalities after argument constraints solve meta variables.
-- The result holds the equality wanteds that the enclosing scope must solve.
solveImplicationEqualities :: [TyVarId] -> [Pred] -> [(TcType, TcType)] -> [Ct] -> TcM [Ct]
solveImplicationEqualities skolems predicates equalities constraints = do
  results <- withGivenPredicates predicates (mapM solveEquality constraints)
  let remaining = [constraint | (constraint, result) <- zip constraints results, case result of EqSolved -> False; _ -> True]
  if length remaining < length constraints
    then solveImplicationEqualities skolems predicates equalities remaining
    else concat <$> mapM (solveWantedWithGivens skolems predicates equalities) remaining

partitionWanteds :: [Ct] -> ([Ct], [Ct])
partitionWanteds = foldr partitionOne ([], [])
  where
    partitionOne ct (equalities, dictionaries) =
      case ctPred ct of
        EqPred {} -> (ct : equalities, dictionaries)
        ClassPred {} -> (equalities, ct : dictionaries)
        QuantifiedPred {} -> (equalities, ct : dictionaries)
        IParamPred {} -> (equalities, ct : dictionaries)

-- | Decompose a given constraint into atomic equalities.
-- For example, @GADT a ~ GADT Bool@ decomposes into @[(a, Bool)]@.
canonicalizeGiven :: Ct -> TcM [(TcType, TcType)]
canonicalizeGiven ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1
    t2' <- zonkType t2
    decomposeEq t1' t2'
  _ -> pure []
  where
    decomposeEq t1 t2
      | t1 == t2 = pure []
      | otherwise = do
          children <- decomposeNominalEquality t1 t2
          case children of
            Just pairs -> concat <$> mapM (uncurry decomposeEq) pairs
            Nothing -> pure [(t1, t2)]

-- | Apply a list of given equalities as a substitution to a type.
-- Each given @(lhs, rhs)@ rewrites occurrences of @lhs@ with @rhs@.
applyGivenSubst :: [(TcType, TcType)] -> TcType -> TcType
applyGivenSubst givens ty = foldr applyOne ty givens
  where
    applyOne (lhs, rhs) t
      | t == lhs = rhs
      | otherwise =
          case t of
            TcTyCon tc args -> TcTyCon tc (map (applyOne (lhs, rhs)) args)
            TcFunTy a b -> TcFunTy (applyOne (lhs, rhs) a) (applyOne (lhs, rhs) b)
            TcAppTy f a -> mkAppTy (applyOne (lhs, rhs) f) (applyOne (lhs, rhs) a)
            TcForAllTy tv body -> TcForAllTy tv (applyOne (lhs, rhs) body)
            TcQualTy predicates body ->
              TcQualTy (map (applyOnePred (lhs, rhs)) predicates) (applyOne (lhs, rhs) body)
            _ -> t
    applyOnePred equality predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className (map (applyOne equality) arguments)
        EqPred left right -> EqPred (applyOne equality left) (applyOne equality right)
        IParamPred name payload -> IParamPred name (applyOne equality payload)
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred variables (map (applyOnePred equality) antecedents) (applyOnePred equality consequent)

-- | Attempt to solve a wanted constraint using given equalities.
-- Equality evidence must prove the original endpoints.
solveWantedWithGivens :: [TyVarId] -> [Pred] -> [(TcType, TcType)] -> Ct -> TcM [Ct]
solveWantedWithGivens skolems givenPredicates givenEqualities ct = case ctPred ct of
  EqPred {} -> do
    result <- withGivenPredicates givenPredicates (solveEquality ct)
    case result of
      EqSolved -> pure []
      EqStuck stuck -> deferOrReport skolems stuck
      EqError errCt -> do
        case ctPred errCt of
          EqPred et1 et2 ->
            emitError (ctLoc errCt) . UnificationError et1 et2 (ctOrigin errCt) =<< zonkCtEqProvenance errCt
          p ->
            emitError (ctLoc errCt) (UnsolvedWanted p (ctOrigin errCt))
        pure []
  ClassPred className arguments -> do
    kinds <- getKinds
    arguments' <- mapM zonkType arguments
    let rewritten = ClassPred className (map (applyGivenSubst givenEqualities) arguments')
        rewrittenGivens = map (rewritePred kinds givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritten})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems stuck
  quantified@QuantifiedPred {} -> do
    kinds <- getKinds
    let rewrittenGivens = map (rewritePred kinds givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritePred kinds givenEqualities quantified})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems stuck
  IParamPred name payload -> do
    kinds <- getKinds
    payload' <- zonkType payload
    let rewritten = IParamPred name (applyGivenSubst givenEqualities payload')
        rewrittenGivens = map (rewritePred kinds givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritten})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems stuck

-- | A stuck dictionary wanted that mentions a meta variable and no skolem
-- of the implication can still be solved by the enclosing scope, so it is
-- deferred. Every other stuck wanted is an error.
deferOrReport :: [TyVarId] -> Ct -> TcM [Ct]
deferOrReport skolems stuck = do
  predicate <- zonkPred (ctPred stuck)
  let deferrable = not (null (predMetaVars predicate)) && not (any (`elem` skolems) (predTyVars predicate))
  if deferrable
    then pure [stuck {ctPred = predicate}]
    else do
      reportUnsolvedDict stuck
      pure []

predMetaVars :: Pred -> [Unique]
predMetaVars predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeMetaVars arguments
    EqPred left right -> typeMetaVars left <> typeMetaVars right
    IParamPred _ payload -> typeMetaVars payload
    QuantifiedPred _ antecedents consequent -> concatMap predMetaVars antecedents <> predMetaVars consequent

typeMetaVars :: TcType -> [Unique]
typeMetaVars ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcArrowTy -> []
    TcTyVar _ -> []
    TcTyCon _ arguments -> concatMap typeMetaVars arguments
    TcFunTy argument result -> typeMetaVars argument <> typeMetaVars result
    TcForAllTy _ body -> typeMetaVars body
    TcQualTy predicates body -> concatMap predMetaVars predicates <> typeMetaVars body
    TcAppTy function argument -> typeMetaVars function <> typeMetaVars argument

predTyVars :: Pred -> [TyVarId]
predTyVars predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeTyVars arguments
    EqPred left right -> typeTyVars left <> typeTyVars right
    IParamPred _ payload -> typeTyVars payload
    QuantifiedPred variables antecedents consequent ->
      filter (`notElem` variables) (concatMap predTyVars antecedents <> predTyVars consequent)

typeTyVars :: TcType -> [TyVarId]
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> [tyVar]
    TcMetaTv _ -> []
    TcArrowTy -> []
    TcTyCon _ arguments -> concatMap typeTyVars arguments
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body)
    TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

rewritePred :: TcKinds -> [(TcType, TcType)] -> Pred -> Pred
rewritePred kinds equalities predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map (applyGivenSubst equalities) arguments)
    EqPred left right -> EqPred (applyGivenSubst equalities left) (applyGivenSubst equalities right)
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred variables (map (rewritePred kinds equalities) antecedents) (rewritePred kinds equalities consequent)
    IParamPred name payload -> IParamPred name (applyGivenSubst equalities payload)

zonkCtEqProvenance :: Ct -> TcM (Maybe EqProvenance)
zonkCtEqProvenance ct =
  traverse zonkEqProvenance (ctEqProvenance ct)

zonkEqProvenance :: EqProvenance -> TcM EqProvenance
zonkEqProvenance provenance = do
  actual <- zonkTypeTrace (eqActualTrace provenance)
  expected <- zonkTypeTrace (eqExpectedTrace provenance)
  pure
    provenance
      { eqActualTrace = actual,
        eqExpectedTrace = expected
      }

zonkTypeTrace :: TypeTrace -> TcM TypeTrace
zonkTypeTrace trace' = do
  ty <- zonkType (typeTraceType trace')
  pure (trace' {typeTraceType = ty})

-- | Strict left fold in a monad.
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = pure acc
foldM f acc (x : xs) = do
  acc' <- f acc x
  foldM f acc' xs
