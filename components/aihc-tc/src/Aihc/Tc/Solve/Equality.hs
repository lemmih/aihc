-- | Equality solver.
--
-- Handles unification of meta-variables, decomposition of type
-- constructor equalities, and building coercion evidence.
module Aihc.Tc.Solve.Equality
  ( solveEquality,
    solveGivenEquality,
    EqResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind, unifyKindsAt)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Congruence (proveGivenEquality)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, reduceTypeFamilies, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (unless)
import Data.Map.Strict qualified as Map

-- | Preserve a proof from the current signature or pattern scope.
solveGivenEquality :: [Pred] -> Ct -> TcM Bool
solveGivenEquality givens ct = case ctPred ct of
  EqPred left right -> do
    left' <- zonkType left
    right' <- zonkType right
    predicates <- mapM zonkPred givens
    result <- proveGivenEquality predicates left' right'
    case result of
      Just proof -> do
        bindEvidence (ctEvVar ct) (EvCoercion proof)
        pure True
      Nothing -> pure False
  _ -> pure False

-- | Result of attempting to solve an equality constraint.
data EqResult
  = -- | Solved: evidence bound.
    EqSolved
  | -- | Stuck: cannot solve yet (e.g. two different skolems).
    EqStuck !Ct
  | -- | Error: types are incompatible.
    EqError !Ct
  deriving (Show)

-- | Attempt to solve an equality constraint.
solveEquality :: Ct -> TcM EqResult
solveEquality ct = do
  givens <- getGivenPredicates
  proved <- solveGivenEquality givens ct
  if proved then pure EqSolved else solveWithoutGivens ct

solveWithoutGivens :: Ct -> TcM EqResult
solveWithoutGivens ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1 >>= reduceTypeFamilies
    t2' <- zonkType t2 >>= reduceTypeFamilies
    solveEq (ct {ctPred = EqPred t1' t2'}) t1' t2'
  _ -> pure (EqStuck ct)

-- | Solve an equality between two zonked and reduced types.
solveEq :: Ct -> TcType -> TcType -> TcM EqResult
solveEq ct rawLeft rawRight = do
  -- The extra arguments of a family application decompose like an
  -- application spine.
  t1 <- unsaturateFamilyApplication rawLeft
  t2 <- unsaturateFamilyApplication rawRight
  leftIsFamily <- isTypeFamilyApplication t1
  rightIsFamily <- isTypeFamilyApplication t2
  if (leftIsFamily || rightIsFamily) && not (isMetaTv t1) && not (isMetaTv t2)
    then
      if t1 == t2
        then do
          bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
          pure EqSolved
        else -- A type family application that no equation reduces waits for
        -- its arguments to become known.
          pure (EqStuck ct)
    else solveEqShapes ct t1 t2

isMetaTv :: TcType -> Bool
isMetaTv ty =
  case ty of
    TcMetaTv _ -> True
    _ -> False

solveEqShapes :: Ct -> TcType -> TcType -> TcM EqResult
solveEqShapes ct t1 t2 = case (t1, t2) of
  -- Same meta: trivially solved.
  (TcMetaTv u1, TcMetaTv u2) | u1 == u2 -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Meta on left: solve by binding.
  (TcMetaTv u, _) -> solveMetaEq ct u t2
  -- Meta on right: solve by binding.
  (_, TcMetaTv u) -> solveMetaEq ct u t1
  -- Same rigid variable.
  (TcTyVar v1, TcTyVar v2) | v1 == v2 -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Two polymorphic types are equal up to the names of their bound
  -- variables.
  (TcForAllTy v1 b1, TcForAllTy v2 b2) ->
    solveDecomposed ct t1 [(b1, applySubst (Map.singleton (tvUnique v2) (TcTyVar v1)) b2)]
  (TcQualTy p1 b1, TcQualTy p2 b2)
    | p1 == p2 ->
        solveDecomposed ct t1 [(b1, b2)]
  _ -> do
    children <- decomposeNominalEquality t1 t2
    case children of
      Just pairs -> solveDecomposed ct t1 pairs
      Nothing -> pure (EqError ct)

-- | Solve a meta-variable equality by binding.
solveMetaEq :: Ct -> Unique -> TcType -> TcM EqResult
solveMetaEq ct u ty
  | occursIn u ty = pure (EqError ct)
  | otherwise = do
      declaredKind <- readMetaTvKind u
      solvedKind <- tcTypeKind ty
      unifyKindsAt (ctLoc ct) declaredKind solvedKind
      writeMetaTv u ty
      bindEvidence (ctEvVar ct) (EvCoercion (Refl ty))
      pure EqSolved

solveDecomposed :: Ct -> TcType -> [(TcType, TcType)] -> TcM EqResult
solveDecomposed ct witness pairs = do
  results <- mapM solvePair pairs
  case firstUnsolved results of
    Nothing -> do
      givens <- getGivenPredicates
      proved <- solveGivenEquality givens ct
      unless proved (bindEvidence (ctEvVar ct) (EvCoercion (Refl witness)))
      pure EqSolved
    Just EqStuck {} -> pure (EqStuck ct)
    Just result -> pure result
  where
    solvePair (left, right) = do
      evidence <- freshEvVar
      solveEquality (ct {ctPred = EqPred left right, ctEvVar = evidence})

firstUnsolved :: [EqResult] -> Maybe EqResult
firstUnsolved [] = Nothing
firstUnsolved (EqSolved : rest) = firstUnsolved rest
firstUnsolved (result : _) = Just result

-- | Occurs check: does meta-variable u appear in the type?
occursIn :: Unique -> TcType -> Bool
occursIn u = go
  where
    go (TcMetaTv u') = u == u'
    go (TcTyVar _) = False
    go (TcTyCon _ args) = any go args
    go (TcFunTy a b) = go a || go b
    go (TcForAllTy _ body) = go body
    go (TcQualTy preds body) = any goPred preds || go body
    go (TcAppTy f a) = go f || go a

    goPred (ClassPred _ args) = any go args
    goPred (EqPred a b) = go a || go b
    goPred (IParamPred _ payload) = go payload
    goPred (QuantifiedPred variables antecedents consequent) =
      any (go . tvKind) variables || any goPred antecedents || goPred consequent
