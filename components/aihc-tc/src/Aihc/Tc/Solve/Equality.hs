{-# LANGUAGE OverloadedStrings #-}

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
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, reduceTypeFamilies, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)

-- | Preserve a proof from the current signature or pattern scope.
solveGivenEquality :: [Pred] -> Ct -> TcM Bool
solveGivenEquality givens ct = case ctPred ct of
  EqPred left right -> do
    left' <- zonkType left
    right' <- zonkType right
    predicates <- mapM zonkGiven givens
    case prove predicates [] left' right' of
      Just proof -> do
        bindEvidence (ctEvVar ct) (EvCoercion proof)
        pure True
      Nothing -> pure False
  _ -> pure False
  where
    zonkGiven (EqPred left right) = EqPred <$> zonkType left <*> zonkType right
    zonkGiven predicate = pure predicate

    prove predicates visited left right
      | left == right = Just (Refl left)
      | left `elem` visited = Nothing
      | otherwise = listToMaybe (mapMaybe follow (edges predicates left))
      where
        follow (next, proof) = Trans proof <$> prove predicates (left : visited) next right

    edges predicates source = concatMap edge predicates
      where
        edge predicate@(EqPred left right) =
          [(right, GivenCo predicate) | left == source]
            <> [(left, Sym (GivenCo predicate)) | right == source]
        edge _ = []

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
solveEquality ct = case ctPred ct of
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
  -- Same type constructor: decompose.
  (TcTyCon tc1 args1, TcTyCon tc2 args2)
    | tc1 == tc2,
      length args1 == length args2 ->
        solveDecomposed ct t1 (zip args1 args2)
  -- Same function type shape: decompose.
  (TcFunTy a1 b1, TcFunTy a2 b2) ->
    solveDecomposed ct t1 [(a1, a2), (b1, b2)]
  -- The function type is the saturated arrow constructor, so an applied
  -- constructor variable can match it.
  (TcAppTy f a, TcFunTy argument result) -> do
    arrow <- arrowTyCon
    solveDecomposed ct t1 [(f, TcTyCon arrow [argument]), (a, result)]
  (TcFunTy argument result, TcAppTy f a) -> do
    arrow <- arrowTyCon
    solveDecomposed ct t1 [(TcTyCon arrow [argument], f), (result, a)]
  -- Applied type constructor variable against a saturated tycon.
  (TcAppTy f a, TcTyCon tc args)
    | not (null args) ->
        solveDecomposed ct t1 [(f, TcTyCon tc (init args)), (a, last args)]
  (TcTyCon tc args, TcAppTy f a)
    | not (null args) ->
        solveDecomposed ct t1 [(TcTyCon tc (init args), f), (last args, a)]
  -- Same type-application shape. This is also needed when solving wanted
  -- equalities inside an implication, where canonicalization does not first
  -- decompose the application for us.
  (TcAppTy f1 a1, TcAppTy f2 a2) ->
    solveDecomposed ct t1 [(f1, f2), (a1, a2)]
  -- Two polymorphic types are equal up to the names of their bound
  -- variables.
  (TcForAllTy v1 b1, TcForAllTy v2 b2) ->
    solveDecomposed ct t1 [(b1, applySubst (Map.singleton (tvUnique v2) (TcTyVar v1)) b2)]
  (TcQualTy p1 b1, TcQualTy p2 b2)
    | p1 == p2 ->
        solveDecomposed ct t1 [(b1, b2)]
  -- Incompatible types.
  _ -> pure (EqError ct)

-- | The function arrow type constructor.
arrowTyCon :: TcM TyCon
arrowTyCon = mkKnownTyCon "GHC.Types" "(->)" 2 (KFun KType (KFun KType KType))

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
      bindEvidence (ctEvVar ct) (EvCoercion (Refl witness))
      pure EqSolved
    Just result -> pure result
  where
    solvePair (left, right) =
      solveEquality (ct {ctPred = EqPred left right})

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
