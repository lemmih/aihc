-- | Type family reduction.
--
-- A saturated application of a type family rewrites to the right-hand
-- side of the first equation whose left-hand side matches. The solver
-- reduces both sides of an equality and the arguments of a class
-- constraint before it compares them, so @Elem Bag@ and @Nat@ are the same
-- type when an equation says so.
module Aihc.Tc.Solve.Family
  ( reduceTypeFamilies,
    reducePredFamilies,
    isTypeFamilyTyCon,
    isTypeFamilyApplication,
    unsaturateFamilyApplication,
    matchTypes,
  )
where

import Aihc.Tc.Env (TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..))
import Aihc.Tc.Monad (TcM, getKinds, getTyConEnv, getTypeFamilyInstances, lookupTyConByIdentity)
import Aihc.Tc.Types
import Control.Monad (foldM)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Rewrite every type family application in a type that an equation
-- matches. The type must be zonked.
reduceTypeFamilies :: TcType -> TcM TcType
reduceTypeFamilies ty =
  case ty of
    TcTyCon tyCon arguments -> do
      arguments' <- mapM reduceTypeFamilies arguments
      reduceHead (TcTyCon tyCon arguments')
    TcFunTy argument result -> TcFunTy <$> reduceTypeFamilies argument <*> reduceTypeFamilies result
    TcAppTy function argument -> do
      kinds <- getKinds
      function' <- reduceTypeFamilies function
      argument' <- reduceTypeFamilies argument
      reduceHead (applyType kinds function' argument')
    TcForAllTy tyVar body -> TcForAllTy tyVar <$> reduceTypeFamilies body
    TcQualTy predicates body -> TcQualTy <$> mapM reducePredFamilies predicates <*> reduceTypeFamilies body
    _ -> pure ty

-- | Rewrite the type family applications in a predicate.
reducePredFamilies :: Pred -> TcM Pred
reducePredFamilies predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className <$> mapM reduceTypeFamilies arguments
    EqPred left right -> EqPred <$> reduceTypeFamilies left <*> reduceTypeFamilies right
    IParamPred name payload -> IParamPred name <$> reduceTypeFamilies payload
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred variables <$> mapM reducePredFamilies antecedents <*> reducePredFamilies consequent

-- | Whether a type constructor is a type family.
isTypeFamilyTyCon :: TcM (TyCon -> Bool)
isTypeFamilyTyCon = do
  tyCons <- getTyConEnv
  pure (\tyCon -> maybe False ((== TypeFamilyTyCon) . tciFlavor) (Map.lookup tyCon tyCons))

-- | Whether a type is a saturated application of a type family that no
-- equation reduces yet. The equality solver waits for such a type.
isTypeFamilyApplication :: TcType -> TcM Bool
isTypeFamilyApplication ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      pure $ case maybeInfo of
        Just info -> tciFlavor info == TypeFamilyTyCon && length arguments == tciArity info
        Nothing -> False
    _ -> pure False

-- | Split a type family application with more arguments than the family
-- arity into an application spine over the saturated family application.
-- The extra arguments of a family application decompose like the arguments
-- of a type constructor; only the family arguments do not.
unsaturateFamilyApplication :: TcType -> TcM TcType
unsaturateFamilyApplication ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      pure $ case maybeInfo of
        Just info
          | tciFlavor info == TypeFamilyTyCon,
            length arguments > tciArity info ->
              let (familyArguments, extraArguments) = splitAt (tciArity info) arguments
               in foldl TcAppTy (TcTyCon tyCon familyArguments) extraArguments
        _ -> ty
    _ -> pure ty

-- | Rewrite the head of a type whose arguments are reduced.
reduceHead :: TcType -> TcM TcType
reduceHead ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo of
        Just info
          | tciFlavor info == TypeFamilyTyCon,
            length arguments >= tciArity info -> do
              let (familyArguments, extraArguments) = splitAt (tciArity info) arguments
              kinds <- getKinds
              equations <- familyEquations tyCon
              case firstEquation kinds equations familyArguments of
                Just reduced -> reduceTypeFamilies (foldl (applyType kinds) reduced extraArguments)
                Nothing -> pure ty
        _ -> pure ty
    _ -> pure ty

-- | The equations of a type family, in declaration order.
familyEquations :: TyCon -> TcM [TypeFamilyInstanceInfo]
familyEquations tyCon =
  sortOn axiomIndex . filter isEquationOf <$> getTypeFamilyInstances
  where
    isEquationOf info =
      case tfiiLeft info of
        TcTyCon familyTyCon _ -> familyTyCon == tyCon
        _ -> False

-- | The index of an equation in its family. The axiom name ends with it.
axiomIndex :: TypeFamilyInstanceInfo -> Int
axiomIndex info =
  fromMaybe 0 (readMaybe (T.unpack (T.takeWhileEnd (/= '$') (tfiiAxiomName info))))

-- | The right-hand side of the first equation that matches. In a closed
-- family, an earlier equation that could still match after the meta
-- variables are solved blocks the later equations.
firstEquation :: TcKinds -> [TypeFamilyInstanceInfo] -> [TcType] -> Maybe TcType
firstEquation kinds equations arguments =
  case equations of
    [] -> Nothing
    equation : rest ->
      case equationArguments equation of
        Just patterns
          | Just substitution <- matchTypes patterns arguments ->
              Just (applySubst kinds substitution (tfiiRight equation))
          | tfiiClosed equation,
            and (zipWith couldUnify patterns arguments) ->
              Nothing
        _ -> firstEquation kinds rest arguments

equationArguments :: TypeFamilyInstanceInfo -> Maybe [TcType]
equationArguments info =
  case tfiiLeft info of
    TcTyCon _ patterns -> Just patterns
    _ -> Nothing

-- | Whether a pattern could match a type once its meta variables are solved.
couldUnify :: TcType -> TcType -> Bool
couldUnify patternType target =
  case (patternType, target) of
    (TcTyVar _, _) -> True
    (_, TcMetaTv _) -> True
    (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments) ->
      tyCon == targetTyCon
        && length arguments == length targetArguments
        && and (zipWith couldUnify arguments targetArguments)
    (TcFunTy argument result, TcFunTy targetArgument targetResult) ->
      couldUnify argument targetArgument && couldUnify result targetResult
    (TcAppTy function argument, TcAppTy targetFunction targetArgument) ->
      couldUnify function targetFunction && couldUnify argument targetArgument
    _ -> patternType == target

applyType :: TcKinds -> TcType -> TcType -> TcType
applyType kinds (TcTyCon tyCon arguments) argument = mkTyConApp kinds tyCon (arguments <> [argument])
applyType _ function argument = TcAppTy function argument

-- | Match pattern types against target types. The type variables of the
-- patterns are the pattern variables.
matchTypes :: [TcType] -> [TcType] -> Maybe (Map Unique TcType)
matchTypes patterns targets
  | length patterns /= length targets = Nothing
  | otherwise = foldM matchOne Map.empty (zip patterns targets)

matchOne :: Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType)
matchOne subst (TcTyVar tv, target) =
  case Map.lookup (tvUnique tv) subst of
    Nothing -> Just (Map.insert (tvUnique tv) target subst)
    Just existing
      | existing == target -> Just subst
      | otherwise -> Nothing
matchOne subst (TcTyCon tc args, TcTyCon targetTc targetArgs)
  | tc == targetTc,
    length args == length targetArgs =
      foldM matchOne subst (zip args targetArgs)
matchOne subst (TcFunTy a b, TcFunTy targetA targetB) =
  matchOne subst (a, targetA) >>= \subst' -> matchOne subst' (b, targetB)
matchOne subst (TcAppTy f a, TcAppTy targetF targetA) =
  matchOne subst (f, targetF) >>= \subst' -> matchOne subst' (a, targetA)
matchOne subst (patternTy, targetTy)
  | patternTy == targetTy = Just subst
  | otherwise = Nothing
