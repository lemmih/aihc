{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Infer the contexts of the deriving plans of a module batch.
--
-- An anyclass context comes from the instantiated superclasses and default
-- signatures of the class. A stock context asks the class of every
-- constructor field, and a newtype context asks the class of the
-- representation type. The whole batch is visible while simplifying those
-- predicates, so recursive and mutually recursive derived instances are
-- independent of source order.
module Aihc.Tc.Deriving.Context
  ( inferDerivingContexts,
    derivingObligations,
    newtypeRepresentation,
    stockFieldTypes,
    isSupportedStockClass,
    typeTyVars,
    moduleDerivingPlans,
    replaceModulePlans,
    planKey,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Module (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Annotations
  ( TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
  )
import Aihc.Tc.Constraint (CtOrigin (..))
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..), InstanceInfo (..), TyConFlavor (..), instanceIsForClass)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Dict (matchTypes)
import Aihc.Tc.Types
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer every attached context in the batch and record it in the plan.
-- A plan whose obligations cannot be met reports an unsolved constraint and
-- keeps its unresolved context, so no instance is generated for it.
inferDerivingContexts :: [Module] -> TcM [Module]
inferDerivingContexts modules = do
  kinds <- getKinds
  existingInstances <- getInstances
  let originalPlans = concatMap moduleDerivingPlans modules
      trialEnvironment = derivingEnv kinds existingInstances originalPlans
      selectPlan plan
        | tcDerivingStockFallback plan,
          TcDerivingInferContext <- tcDerivingContext plan,
          Left _ <- inferredContext trialEnvironment plan =
            plan {tcDerivingStrategy = TcDerivingStock}
        | otherwise = plan
      selectedPlans = map selectPlan originalPlans
      environment = derivingEnv kinds existingInstances selectedPlans
  contextPlans <- mapM (inferPlanContext kinds environment) selectedPlans
  pure (map (replaceModulePlans contextPlans) modules)

-- | Everything context simplification needs about the batch: the instances
-- and deriving plans in scope, indexed by class source name, and the context
-- inferred for each plan whose context is left to the compiler.
--
-- Instance and plan lists keep their original order, because simplification
-- commits to the first alternative that succeeds.
data DerivingEnv = DerivingEnv
  { derivingEnvInstances :: !(Map Text [InstanceInfo]),
    derivingEnvPlans :: !(Map Text [TcDerivingPlan]),
    derivingEnvContexts :: !(Map PlanKey (Either Pred [Pred]))
  }

derivingEnv :: TcKinds -> [InstanceInfo] -> [TcDerivingPlan] -> DerivingEnv
derivingEnv kinds existingInstances plans =
  base {derivingEnvContexts = solveContexts (length inferable + 2) (Map.fromList (map initialContext inferable))}
  where
    base =
      DerivingEnv
        { derivingEnvInstances = groupByClass iiClassName existingInstances,
          derivingEnvPlans = groupByClass tcDerivingClassName plans,
          derivingEnvContexts = Map.empty
        }
    groupByClass className = Map.fromListWith (flip (<>)) . map (\value -> (className value, [value]))

    inferable = [(plan, obligations) | plan <- plans, Just (Right obligations) <- [inferableObligations kinds plan]]

    -- Reject cycles for anyclass and newtype plans.
    -- Stock plans can use recursive structural instances.
    initialContext (plan, _)
      | tcDerivingStrategy plan `elem` [TcDerivingAnyclass, TcDerivingNewtype] = (planKey plan, Left (planPredicate plan))
      | otherwise = (planKey plan, Right [])

    -- Contexts are inferred simultaneously, so a plan can refer to a plan
    -- declared later, or to itself through a cycle, without the search
    -- re-deriving the same plan once per path through the batch.
    solveContexts :: Int -> Map PlanKey (Either Pred [Pred]) -> Map PlanKey (Either Pred [Pred])
    solveContexts fuel contexts
      | fuel <= 0 = contexts
      | next == contexts = contexts
      | otherwise = solveContexts (fuel - 1) next
      where
        environment = base {derivingEnvContexts = contexts}
        next =
          Map.fromList
            [ (planKey plan, nub . concat <$> mapM (simplifyPredicate kinds environment plan) obligations)
            | (plan, obligations) <- inferable
            ]

-- | The obligations of a plan whose context the compiler has to infer, or
-- 'Nothing' when the plan carries its context or needs no inference. A
-- 'Left' reports why the plan cannot be derived.
inferableObligations :: TcKinds -> TcDerivingPlan -> Maybe (Either String [Pred])
inferableObligations kinds plan =
  case tcDerivingContext plan of
    TcDerivingInferContext -> derivingObligations kinds plan
    TcDerivingExplicitContext {} -> Nothing

-- | The predicates that the generated instance body of a plan needs, before
-- simplification, or 'Nothing' for a strategy that generates nothing.
derivingObligations :: TcKinds -> TcDerivingPlan -> Maybe (Either String [Pred])
derivingObligations kinds plan =
  case tcDerivingStrategy plan of
    TcDerivingAnyclass -> Just (Right (anyClassObligations kinds plan))
    TcDerivingStock
      | isSupportedStockClass (tcDerivingClassName plan) ->
          Just (map (ClassPred (tcDerivingClassTyCon plan) . (: [])) . concat <$> stockFieldTypes plan)
      | otherwise -> Nothing
    TcDerivingNewtype ->
      Just $ do
        representation <- newtypeRepresentation plan
        let substitution = Map.fromList (zip (map tvUnique (tcDerivingClassTyVars plan)) (tcDerivingHeadTypes plan))
            supers = mapMaybe (constraintTypeToPred kinds . applySubst substitution . tcDictBinderType) (tcDerivingClassSuperClasses plan)
            methods = [ClassPred (tcDerivingClassTyCon plan) (init (tcDerivingHeadTypes plan) <> [representation]) | not (null (tcDerivingClassMethods plan))]
        pure (supers <> methods)
    TcDerivingVia {} -> Nothing

-- | The stock classes that the generator can write an instance for.
isSupportedStockClass :: Text -> Bool
isSupportedStockClass className = className `elem` ["Eq", "Ord", "Show", "Read", "Bounded"]

inferPlanContext :: TcKinds -> DerivingEnv -> TcDerivingPlan -> TcM TcDerivingPlan
inferPlanContext kinds environment plan =
  case inferableObligations kinds plan of
    Nothing -> pure plan
    Just (Left message) -> do
      emitError (tcDerivingSourceSpan plan) (OtherError message)
      pure plan
    Just (Right _) ->
      case inferredContext environment plan of
        Left predicate -> do
          emitError
            (tcDerivingSourceSpan plan)
            (UnsolvedWanted predicate (InstOrigin (tcDerivingClassName plan)))
          pure plan
        Right context ->
          pure plan {tcDerivingContext = TcDerivingExplicitContext context}

inferredContext :: DerivingEnv -> TcDerivingPlan -> Either Pred [Pred]
inferredContext environment plan =
  Map.findWithDefault (Left (planPredicate plan)) (planKey plan) (derivingEnvContexts environment)

simplifyPredicate :: TcKinds -> DerivingEnv -> TcDerivingPlan -> Pred -> Either Pred [Pred]
simplifyPredicate kinds environment owner predicate
  | isBareVariablePredicate (tcDerivingTyVars owner) predicate = Right [predicate]
  | ClassPred typeableTyCon _ <- predicate,
    Just arguments <- typeableArguments predicate =
      concat
        <$> mapM
          (simplifyPredicate kinds environment owner . ClassPred typeableTyCon . (: []))
          arguments
  | otherwise =
      case firstSuccessful (map simplifyExisting matchingExisting <> map simplifyDerived matchingDerived) of
        Just context -> Right context
        Nothing
          | isAdmissibleContextPredicate owner predicate -> Right [predicate]
          | otherwise -> Left predicate
  where
    className = predClassName predicate
    matchingExisting =
      [ (instanceInfo, substitution)
      | instanceInfo <- Map.findWithDefault [] className (derivingEnvInstances environment),
        predIsForInstance predicate instanceInfo,
        Just substitution <- [matchTypes (iiHead instanceInfo) (predArguments predicate)]
      ]
    matchingDerived =
      [ (candidate, substitution)
      | candidate <- Map.findWithDefault [] className (derivingEnvPlans environment),
        predIsForPlan predicate candidate,
        Just substitution <- [matchTypes (tcDerivingHeadTypes candidate) (predArguments predicate)]
      ]
    simplifyExisting (instanceInfo, substitution) =
      concat
        <$> mapM
          (simplifyPredicate kinds environment owner . applySubstPred substitution)
          (iiContext instanceInfo)
    simplifyDerived (candidate, substitution) = do
      context <- candidateContext candidate
      concat
        <$> mapM
          (simplifyPredicate kinds environment owner . applySubstPred substitution)
          context
    candidateContext candidate =
      case tcDerivingContext candidate of
        TcDerivingExplicitContext context -> Right context
        TcDerivingInferContext ->
          case Map.lookup (planKey candidate) (derivingEnvContexts environment) of
            Just context -> context
            Nothing -> Left predicate

firstSuccessful :: [Either error value] -> Maybe value
firstSuccessful results =
  case results of
    [] -> Nothing
    Left _ : rest -> firstSuccessful rest
    Right value : _ -> Just value

anyClassObligations :: TcKinds -> TcDerivingPlan -> [Pred]
anyClassObligations kinds plan =
  mapMaybe
    (constraintTypeToPred kinds . applySubst substitution . tcDictBinderType)
    (tcDerivingClassSuperClasses plan)
    <> concat
      [ map (applySubstPred substitution) predicates
      | (methodName, predicates) <- tcDerivingDefaultSignatures plan,
        methodName `elem` tcDerivingDefaultMethods plan
      ]
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]

-- | The field types of every constructor of a stock deriving target, with
-- the datatype parameters instantiated to the instance head.
stockFieldTypes :: TcDerivingPlan -> Either String [[TcType]]
stockFieldTypes plan = do
  dataType <-
    maybe
      (Left (mechanism <> " requires checked datatype metadata"))
      Right
      (tcDerivingDataType plan)
  targetArguments <- targetTypeArguments mechanism dataType plan
  validateStockDataType mechanism dataType
  let substitution =
        Map.fromList
          [ (tvUnique tyVar, argument)
          | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments
          ]
  pure
    [ [applySubst substitution (dcfiType field) | field <- dciFields constructor]
    | constructor <- dtiConstructors dataType
    ]
  where
    mechanism = "stock " <> T.unpack (tcDerivingClassName plan) <> " deriving"

-- | The representation type of a newtype deriving target: the field type
-- of the newtype constructor, eta-reduced over the datatype parameters
-- that the instance head leaves out.
newtypeRepresentation :: TcDerivingPlan -> Either String TcType
newtypeRepresentation plan = do
  dataType <-
    maybe
      (Left "newtype deriving requires checked datatype metadata")
      Right
      (tcDerivingDataType plan)
  constructor <-
    case (dtiFlavor dataType, dtiConstructors dataType) of
      (NewtypeTyCon, [constructor]) -> Right constructor
      _ -> Left "newtype deriving requires a newtype declaration"
  field <-
    case dciFields constructor of
      [field] -> Right (dcfiType field)
      _ -> Left "newtype deriving requires a newtype with one field"
  targetArguments <- targetTypeArguments "newtype deriving" dataType plan
  let supplied = length targetArguments
      substitution =
        Map.fromList
          [ (tvUnique tyVar, argument)
          | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments
          ]
      dropped = drop supplied (dtiTyVars dataType)
  case etaReduce dropped (applySubst substitution field) of
    Just representation
      | not (any (`elem` typeTyVars representation) dropped) -> Right representation
    _ -> Left "newtype deriving cannot eta-reduce the representation type to the instance head"

-- | Remove trailing arguments that are exactly the given type variables.
etaReduce :: [TyVarId] -> TcType -> Maybe TcType
etaReduce [] ty = Just ty
etaReduce tyVars ty =
  case (ty, last tyVars) of
    (TcAppTy function (TcTyVar argument), expected)
      | argument == expected -> etaReduce (init tyVars) function
    (TcTyCon tyCon arguments, expected)
      | not (null arguments),
        TcTyVar argument <- last arguments,
        argument == expected ->
          etaReduce (init tyVars) (TcTyCon tyCon (init arguments))
    _ -> Nothing

-- | The arguments the instance head applies the target type constructor to.
-- A head may apply fewer arguments than the datatype has parameters.
targetTypeArguments :: String -> DataTypeInfo -> TcDerivingPlan -> Either String [TcType]
targetTypeArguments mechanism dataType plan =
  case reverse (tcDerivingHeadTypes plan) of
    TcTyCon tyCon arguments : _
      | tyConKey tyCon == tyConKey (dtiTyCon dataType),
        length arguments <= length (dtiTyVars dataType) ->
          Right arguments
    _ -> Left (mechanism <> " target does not match its checked datatype metadata")

validateStockDataType :: String -> DataTypeInfo -> Either String ()
validateStockDataType mechanism dataType
  | dtiFlavor dataType `notElem` [DataTyCon, NewtypeTyCon] =
      Left (mechanism <> " requires a data or newtype declaration")
  | null constructors =
      Left (mechanism <> " does not yet support empty data declarations")
  | not (all (null . dciExTyVars) constructors) =
      Left (mechanism <> " does not yet support existential constructors")
  | not (all (null . dciTheta) constructors) =
      Left (mechanism <> " does not yet support constrained constructors")
  | any ((/= expectedResult) . dciResTy) constructors =
      Left (mechanism <> " does not yet support refined GADT result types")
  | otherwise = Right ()
  where
    constructors = dtiConstructors dataType
    expectedResult = TcTyCon (dtiTyCon dataType) (map TcTyVar (dtiTyVars dataType))

moduleDerivingPlans :: Module -> [TcDerivingPlan]
moduleDerivingPlans = concatMap declDerivingPlans . moduleDecls

declDerivingPlans :: Decl -> [TcDerivingPlan]
declDerivingPlans decl =
  case decl of
    DeclAnn annotation inner ->
      maybe [] tcDerivingPlans (fromAnnotation @TcDerivingAnnotation annotation)
        <> declDerivingPlans inner
    _ -> []

replaceModulePlans :: [TcDerivingPlan] -> Module -> Module
replaceModulePlans plans modu =
  modu {moduleDecls = map replaceDecl (moduleDecls modu)}
  where
    replaceDecl decl =
      case decl of
        DeclAnn annotation inner
          | Just derivingAnnotation <- fromAnnotation @TcDerivingAnnotation annotation ->
              DeclAnn
                (mkAnnotation (derivingAnnotation {tcDerivingPlans = map replacePlan (tcDerivingPlans derivingAnnotation)}))
                (replaceDecl inner)
          | otherwise -> DeclAnn annotation (replaceDecl inner)
        _ -> decl
    replacePlan original =
      fromMaybe original (find ((== planKey original) . planKey) plans)

type PlanKey = (TyCon, [TcType])

planKey :: TcDerivingPlan -> PlanKey
planKey plan = (tcDerivingClassTyCon plan, tcDerivingHeadTypes plan)

planPredicate :: TcDerivingPlan -> Pred
planPredicate plan = ClassPred (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan)

predIsForInstance :: Pred -> InstanceInfo -> Bool
predIsForInstance predicate instanceInfo =
  case predicate of
    ClassPred classTyCon _ -> instanceIsForClass classTyCon instanceInfo
    _ -> iiClassName instanceInfo == predClassName predicate

predIsForPlan :: Pred -> TcDerivingPlan -> Bool
predIsForPlan predicate plan =
  case predicate of
    ClassPred classTyCon _ -> tyConKey classTyCon == tyConKey (tcDerivingClassTyCon plan)
    _ -> tcDerivingClassName plan == predClassName predicate

predClassName :: Pred -> Text
predClassName predicate =
  case predicate of
    ClassPred className _ -> tyConName className
    EqPred {} -> "~"
    QuantifiedPred {} -> "quantified"
    IParamPred name _ -> name

predArguments :: Pred -> [TcType]
predArguments predicate =
  case predicate of
    ClassPred _ arguments -> arguments
    EqPred left right -> [left, right]
    QuantifiedPred _ antecedents consequent -> concatMap predArguments antecedents <> predArguments consequent
    IParamPred _ payload -> [payload]

typeableArguments :: Pred -> Maybe [TcType]
typeableArguments predicate =
  case predicate of
    ClassPred classTyCon [ty]
      | tyConName classTyCon == "Typeable" ->
          case ty of
            TcTyCon _ arguments -> Just arguments
            TcFunTy argument result -> Just [argument, result]
            TcTyVar {} -> Nothing
            TcMetaTv {} -> Nothing
            TcArrowTy -> Nothing
            TcForAllTy {} -> Nothing
            TcQualTy {} -> Nothing
            TcAppTy {} -> Nothing
    _ -> Nothing

isBareVariablePredicate :: [TyVarId] -> Pred -> Bool
isBareVariablePredicate tyVars predicate =
  case predicate of
    ClassPred _ arguments ->
      not (null arguments)
        && all isPlanTyVar arguments
    EqPred {} -> False
    QuantifiedPred {} -> False
    IParamPred {} -> False
  where
    isPlanTyVar (TcTyVar tyVar) = tyVar `elem` tyVars
    isPlanTyVar _ = False

isAdmissibleContextPredicate :: TcDerivingPlan -> Pred -> Bool
isAdmissibleContextPredicate plan predicate =
  not (null mentionedVariables)
    && all (`elem` tcDerivingTyVars plan) mentionedVariables
    && maybe True (not . predicateMentionsTyCon predicate) (derivedTargetTyCon plan)
  where
    mentionedVariables = predTyVars predicate

derivedTargetTyCon :: TcDerivingPlan -> Maybe Text
derivedTargetTyCon plan =
  case reverse (tcDerivingHeadTypes plan) of
    target : _ -> typeHeadTyCon target
    [] -> Nothing

typeHeadTyCon :: TcType -> Maybe Text
typeHeadTyCon ty =
  case ty of
    TcTyCon tyCon _ -> Just (tyConName tyCon)
    TcAppTy function _ -> typeHeadTyCon function
    _ -> Nothing

predicateMentionsTyCon :: Pred -> Text -> Bool
predicateMentionsTyCon predicate name =
  any (typeMentionsTyCon name) (predArguments predicate)

typeMentionsTyCon :: Text -> TcType -> Bool
typeMentionsTyCon name ty =
  case ty of
    TcTyVar {} -> False
    TcMetaTv {} -> False
    TcArrowTy -> False
    TcTyCon tyCon arguments -> tyConName tyCon == name || any (typeMentionsTyCon name) arguments
    TcFunTy argument result -> typeMentionsTyCon name argument || typeMentionsTyCon name result
    TcForAllTy _ body -> typeMentionsTyCon name body
    TcQualTy predicates body -> any (`predicateMentionsTyCon` name) predicates || typeMentionsTyCon name body
    TcAppTy function argument -> typeMentionsTyCon name function || typeMentionsTyCon name argument

predTyVars :: Pred -> [TyVarId]
predTyVars predicate =
  case predicate of
    QuantifiedPred variables antecedents consequent ->
      filter (`notElem` variables) (nub (concatMap predTyVars antecedents <> predTyVars consequent))
    _ -> nub (concatMap typeTyVars (predArguments predicate))

typeTyVars :: TcType -> [TyVarId]
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> [tyVar]
    TcMetaTv {} -> []
    TcArrowTy -> []
    TcTyCon _ arguments -> concatMap typeTyVars arguments
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body)
    TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument
