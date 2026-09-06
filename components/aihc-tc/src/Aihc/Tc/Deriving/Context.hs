{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize strategy-specific deriving contexts as a module batch.
--
-- AnyClass contexts come from instantiated superclasses and default
-- signatures. Stock Eq contexts come from checked constructor fields. The
-- whole batch is visible while simplifying those predicates so recursive and
-- mutually recursive derived dictionaries are independent of source order.
module Aihc.Tc.Deriving.Context
  ( finalizeDerivingModulesTc,
    typeTyVars,
    derivingPlanInstanceInfo,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Module (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Annotations
  ( TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
    TcStockDerivingPlan (..),
  )
import Aihc.Tc.Constraint (Ct (..), CtOrigin (..), mkWantedCt)
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..), InstanceInfo (..), TyConFlavor (..), instanceIsForClass)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Dict (DictResult (..), matchTypes, solveDictWithGivens)
import Aihc.Tc.Types
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

finalizeDerivingModulesTc :: [(Text, Text)] -> [Module] -> TcM [Module]
finalizeDerivingModulesTc moduleOrigins modules = do
  existingInstances <- getInstances
  let originalPlans = concatMap moduleDerivingPlans modules
      originalOrigins = concat (zipWith (\origin modu -> replicate (length (moduleDerivingPlans modu)) origin) moduleOrigins modules)
      environment = derivingEnv existingInstances originalPlans
  contextPlans <- mapM (inferPlanContext environment) originalPlans
  let derivedInstances = mapMaybe (uncurry derivingPlanInstanceInfo) (zip originalOrigins contextPlans)
  mapM_ addInstance derivedInstances
  evidencePlans <- mapM attachDerivingEvidence contextPlans
  pure (map (replaceModulePlans evidencePlans) modules)

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

derivingEnv :: [InstanceInfo] -> [TcDerivingPlan] -> DerivingEnv
derivingEnv existingInstances plans =
  base {derivingEnvContexts = solveContexts (length inferable + 2) (Map.fromList (map initialContext inferable))}
  where
    base =
      DerivingEnv
        { derivingEnvInstances = groupByClass iiClassName existingInstances,
          derivingEnvPlans = groupByClass tcDerivingClassName plans,
          derivingEnvContexts = Map.empty
        }
    groupByClass className = Map.fromListWith (flip (<>)) . map (\value -> (className value, [value]))

    inferable = [(plan, obligations) | plan <- plans, Just obligations <- [inferableObligations plan]]

    -- A plan that mentions itself starts out with the context the old
    -- depth-first search cut a cycle with: anyclass deriving rejects the
    -- cycle, stock Eq assumes the recursive occurrence needs nothing.
    initialContext (plan, _)
      | tcDerivingStrategy plan == TcDerivingAnyclass = (planKey plan, Left (planPredicate plan))
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
            [ (planKey plan, nub . concat <$> mapM (simplifyPredicate environment plan) obligations)
            | (plan, obligations) <- inferable
            ]

-- | The obligations of a plan whose context the compiler has to infer, or
-- 'Nothing' when the plan carries its context or needs no inference.
inferableObligations :: TcDerivingPlan -> Maybe [Pred]
inferableObligations plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (TcDerivingAnyclass, TcDerivingInferContext) -> Just (anyClassObligations plan)
    (TcDerivingStock, TcDerivingInferContext)
      | tcDerivingClassName plan == "Eq",
        Right obligations <- stockEqObligations plan ->
          Just (concat obligations)
      | isSimpleStockClass (tcDerivingClassName plan),
        Right obligations <- stockFieldObligations plan ->
          Just (concat obligations)
    _ -> Nothing

-- | The stock classes of kind @Type@ whose derived instance context is the
-- class at every constructor field, as for Eq. Their dictionaries are not
-- generated yet; the inferred context lets the instances take part in
-- instance resolution, such as the Ord superclass of a Real instance.
isSimpleStockClass :: Text -> Bool
isSimpleStockClass className = className `elem` ["Ord", "Show", "Read", "Bounded", "Enum", "Ix"]

inferPlanContext :: DerivingEnv -> TcDerivingPlan -> TcM TcDerivingPlan
inferPlanContext environment plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (TcDerivingAnyclass, TcDerivingInferContext) ->
      case inferredContext environment plan of
        Left predicate -> do
          emitError
            (tcDerivingSourceSpan plan)
            (UnsolvedWanted predicate (InstOrigin (tcDerivingClassName plan)))
          pure plan
        Right context ->
          pure plan {tcDerivingContext = TcDerivingExplicitContext context}
    (TcDerivingStock, context)
      | tcDerivingClassName plan == "Eq" ->
          case stockEqObligations plan of
            Left message -> do
              emitError (tcDerivingSourceSpan plan) (OtherError message)
              pure plan
            Right _ ->
              case context of
                TcDerivingExplicitContext {} -> pure plan
                TcDerivingInferContext ->
                  case inferredContext environment plan of
                    Left predicate -> do
                      emitError
                        (tcDerivingSourceSpan plan)
                        (UnsolvedWanted predicate (InstOrigin "Eq"))
                      pure plan
                    Right inferred ->
                      pure plan {tcDerivingContext = TcDerivingExplicitContext inferred}
      | isSimpleStockClass (tcDerivingClassName plan),
        TcDerivingInferContext <- context,
        Right _ <- stockFieldObligations plan ->
          case inferredContext environment plan of
            Left predicate -> do
              emitError
                (tcDerivingSourceSpan plan)
                (UnsolvedWanted predicate (InstOrigin (tcDerivingClassName plan)))
              pure plan
            Right inferred ->
              pure plan {tcDerivingContext = TcDerivingExplicitContext inferred}
    _ -> pure plan

inferredContext :: DerivingEnv -> TcDerivingPlan -> Either Pred [Pred]
inferredContext environment plan =
  Map.findWithDefault (Left (planPredicate plan)) (planKey plan) (derivingEnvContexts environment)

simplifyPredicate :: DerivingEnv -> TcDerivingPlan -> Pred -> Either Pred [Pred]
simplifyPredicate environment owner predicate
  | isBareVariablePredicate (tcDerivingTyVars owner) predicate = Right [predicate]
  | ClassPred typeableTyCon _ <- predicate,
    Just arguments <- typeableArguments predicate =
      concat
        <$> mapM
          (simplifyPredicate environment owner . ClassPred typeableTyCon . (: []))
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
          (simplifyPredicate environment owner . applySubstPred substitution)
          (iiContext instanceInfo)
    simplifyDerived (candidate, substitution) = do
      context <- candidateContext candidate
      concat
        <$> mapM
          (simplifyPredicate environment owner . applySubstPred substitution)
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

anyClassObligations :: TcDerivingPlan -> [Pred]
anyClassObligations plan =
  mapMaybe
    (constraintTypeToPred . applySubst substitution . tcDictBinderType)
    (tcDerivingClassSuperClasses plan)
    <> concatMap snd (instantiatedDefaultSignaturePredicates plan)
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]

attachDerivingEvidence :: TcDerivingPlan -> TcM TcDerivingPlan
attachDerivingEvidence plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (TcDerivingAnyclass, TcDerivingExplicitContext context) -> do
      superClassEvidence <- mapM (solveObligation context) superClasses
      superClassBinders <- mapM predDictBinder superClasses
      defaultMethodEvidence <-
        mapM
          (traverse (mapM (solveObligation context)))
          (instantiatedDefaultSignaturePredicates plan)
      pure
        plan
          { tcDerivingSuperClasses = zip superClassBinders superClassEvidence,
            tcDerivingDefaultMethodEvidence = defaultMethodEvidence
          }
    (TcDerivingStock, TcDerivingExplicitContext context)
      | tcDerivingClassName plan == "Eq",
        Right obligations <- stockEqObligations plan -> do
          fieldEvidence <- mapM (mapM (solveObligation context)) obligations
          pure plan {tcDerivingStockPlan = Just (TcStockEqPlan fieldEvidence)}
    _ -> pure plan
  where
    superClasses =
      mapMaybe
        (constraintTypeToPred . applySubst substitution . tcDictBinderType)
        (tcDerivingClassSuperClasses plan)
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]
    solveObligation context predicate = do
      evidenceVariable <- freshEvVar
      let constraint = mkWantedCt predicate evidenceVariable (InstOrigin (tcDerivingClassName plan)) (tcDerivingSourceSpan plan)
      result <- solveDictWithGivens context constraint
      case result of
        DictSolved -> do
          evidence <- lookupEvidence evidenceVariable
          case evidence of
            Just term -> pure term
            Nothing -> pure (EvVarTerm evidenceVariable)
        DictStuck stuck -> do
          emitError (ctLoc stuck) (UnsolvedWanted (ctPred stuck) (ctOrigin stuck))
          pure (EvVarTerm evidenceVariable)

instantiatedDefaultSignaturePredicates :: TcDerivingPlan -> [(Text, [Pred])]
instantiatedDefaultSignaturePredicates plan =
  [ (methodName, map (applySubstPred substitution) predicates)
  | (methodName, predicates) <- tcDerivingDefaultSignatures plan,
    methodName `elem` tcDerivingDefaultMethods plan
  ]
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]

derivingPlanInstanceInfo :: (Text, Text) -> TcDerivingPlan -> Maybe InstanceInfo
derivingPlanInstanceInfo origin plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (strategy, TcDerivingExplicitContext context)
      | strategy == TcDerivingAnyclass || isValidStockEqPlan plan || isSimpleStockPlan plan ->
          Just
            InstanceInfo
              { iiClassName = tcDerivingClassName plan,
                iiDictName = tcDerivingDictName plan,
                iiDictOrigin = origin,
                iiDictType = foldr TcForAllTy (TcQualTy context (planPredicateType plan)) (tcDerivingTyVars plan),
                iiTyVars = tcDerivingTyVars plan,
                iiContext = context,
                iiHead = tcDerivingHeadTypes plan
              }
    _ -> Nothing

isValidStockEqPlan :: TcDerivingPlan -> Bool
isValidStockEqPlan plan =
  tcDerivingStrategy plan == TcDerivingStock
    && tcDerivingClassName plan == "Eq"
    && case stockEqObligations plan of
      Right {} -> True
      Left {} -> False

stockEqObligations :: TcDerivingPlan -> Either String [[Pred]]
stockEqObligations plan = do
  dataType <-
    maybe
      (Left "stock Eq deriving requires checked datatype metadata")
      Right
      (tcDerivingDataType plan)
  targetArguments <- stockEqTargetArguments dataType plan
  validateStockEqClass plan
  validateStockEqDataType dataType
  pure (stockFieldPredicates plan dataType targetArguments)

isSimpleStockPlan :: TcDerivingPlan -> Bool
isSimpleStockPlan plan =
  tcDerivingStrategy plan == TcDerivingStock
    && isSimpleStockClass (tcDerivingClassName plan)
    && case stockFieldObligations plan of
      Right {} -> True
      Left {} -> False

-- | The class at every constructor field of the target, per constructor,
-- for a stock class of kind @Type@ other than Eq.
stockFieldObligations :: TcDerivingPlan -> Either String [[Pred]]
stockFieldObligations plan = do
  dataType <-
    maybe
      (Left "stock deriving requires checked datatype metadata")
      Right
      (tcDerivingDataType plan)
  targetArguments <- stockEqTargetArguments dataType plan
  case tcDerivingClassTyVars plan of
    [_] -> Right ()
    _ -> Left "stock deriving requires a class with one parameter"
  pure (stockFieldPredicates plan dataType targetArguments)

stockFieldPredicates :: TcDerivingPlan -> DataTypeInfo -> [TcType] -> [[Pred]]
stockFieldPredicates plan dataType targetArguments =
  [ [ClassPred (tcDerivingClassTyCon plan) [applySubst substitution (dcfiType field)] | field <- dciFields constructor]
  | constructor <- dtiConstructors dataType
  ]
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, argument)
        | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments
        ]

stockEqTargetArguments :: DataTypeInfo -> TcDerivingPlan -> Either String [TcType]
stockEqTargetArguments dataType plan =
  case reverse (tcDerivingHeadTypes plan) of
    TcTyCon tyCon arguments : _
      | tyConName tyCon == dtiName dataType,
        length arguments == length (dtiTyVars dataType) ->
          Right arguments
    _ -> Left "stock Eq deriving target does not match its checked datatype metadata"

validateStockEqClass :: TcDerivingPlan -> Either String ()
validateStockEqClass plan
  | [_] <- tcDerivingClassTyVars plan,
    null (tcDerivingClassSuperClasses plan),
    map tcClassMethodName (tcDerivingClassMethods plan) == ["==", "/="],
    all validMethod (tcDerivingClassMethods plan) =
      Right ()
  | otherwise = Left "stock Eq deriving requires the standard Eq class layout"
  where
    validMethod method =
      tcClassMethodName method `elem` ["==", "/="]
        && case methodTypeParts (tcClassMethodType method) of
          ( [classVar],
            [ClassPred eqTyCon [TcTyVar predicateVar]],
            TcFunTy (TcTyVar left) (TcFunTy (TcTyVar right) (TcTyCon boolTyCon []))
            ) ->
              tyConName eqTyCon == "Eq"
                && [classVar] == tcDerivingClassTyVars plan
                && predicateVar == classVar
                && left == classVar
                && right == classVar
                && tyConName boolTyCon == "Bool"
          _ -> False

    methodTypeParts ty =
      let (tyVars, qualified) = peelMethodForAlls ty
       in case qualified of
            TcQualTy predicates body -> (tyVars, predicates, body)
            body -> (tyVars, [], body)

    peelMethodForAlls (TcForAllTy tyVar body) =
      let (tyVars, inner) = peelMethodForAlls body
       in (tyVar : tyVars, inner)
    peelMethodForAlls ty = ([], ty)

validateStockEqDataType :: DataTypeInfo -> Either String ()
validateStockEqDataType dataType
  | dtiFlavor dataType `notElem` [DataTyCon, NewtypeTyCon] =
      Left "stock Eq deriving requires a data or newtype declaration"
  | null constructors =
      Left "stock Eq deriving does not yet support empty data declarations"
  | not (all (null . dciExTyVars) constructors) =
      Left "stock Eq deriving does not yet support existential constructors"
  | not (all (null . dciTheta) constructors) =
      Left "stock Eq deriving does not yet support constrained constructors"
  | any ((/= expectedResult) . dciResTy) constructors =
      Left "stock Eq deriving does not yet support refined GADT result types"
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

type PlanKey = (Text, [TcType])

planKey :: TcDerivingPlan -> PlanKey
planKey plan = (tcDerivingClassName plan, tcDerivingHeadTypes plan)

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
    TcTyCon _ arguments -> concatMap typeTyVars arguments
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body)
    TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

planPredicateType :: TcDerivingPlan -> TcType
planPredicateType plan =
  TcTyCon (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan)

predDictBinder :: Pred -> TcM TcDictBinderAnnotation
predDictBinder predicate =
  case predicate of
    ClassPred classTyCon arguments ->
      pure (TcDictBinderAnnotation (tyConName classTyCon) arguments (TcTyCon classTyCon arguments))
    EqPred left right -> do
      equalityTyCon <- mkKnownTyCon "GHC.Types" "~" 2 (KFun KType (KFun KType KConstraint))
      pure (TcDictBinderAnnotation "<constraint>" [] (TcTyCon equalityTyCon [left, right]))
    quantified@QuantifiedPred {} ->
      TcDictBinderAnnotation "<quantified>" [] <$> predicateType quantified
    implicit@(IParamPred name payload) ->
      TcDictBinderAnnotation name [payload] <$> predicateType implicit

predicateType :: Pred -> TcM TcType
predicateType predicate =
  case predicate of
    ClassPred classTyCon arguments -> pure (TcTyCon classTyCon arguments)
    EqPred left right -> do
      equalityTyCon <- mkKnownTyCon "GHC.Types" "~" 2 (KFun KType (KFun KType KConstraint))
      pure (TcTyCon equalityTyCon [left, right])
    IParamPred name payload -> implicitParamType name payload
    QuantifiedPred variables antecedents consequent -> do
      consequentType <- predicateType consequent
      let qualified
            | null antecedents = consequentType
            | otherwise = TcQualTy antecedents consequentType
      pure (foldr TcForAllTy qualified variables)
