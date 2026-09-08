{-# LANGUAGE OverloadedStrings #-}

-- | Dictionary (class constraint) solver.
--
-- For the MVP, this is a stub. The full implementation will match
-- wanted class constraints against given dictionaries and instance
-- declarations.
module Aihc.Tc.Solve.Dict
  ( solveDict,
    solveDictWithGivens,
    DictResult (..),
    callStackOrigin,
    isCallStackPred,
    reportUnsolvedDict,
    matchTypes,
    classFieldTypes,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Constraint
import Aihc.Tc.Env (ClassInfo (..), InstanceInfo (..), instanceIsForClass)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (CallSite (..), Coercion (..), EvTerm (..))
import Aihc.Tc.Monad (TcM, bindEvidence, emitError, freshEvVar, freshSkolemTv, getClassInstances, getKinds, implicitParamType, lookupClass, lookupClassByName, lookupEvidence, wiredTyCon)
import Aihc.Tc.Solve.Coercible (isCoercibleClass, solveCoercible)
import Aihc.Tc.Solve.Family (matchTypes, reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unify)
import Aihc.Tc.Wiring (TcWiring (..))
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Applicative ((<|>))
import Control.Monad (foldM, (<=<))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

-- | Result of attempting to solve a dictionary constraint.
data DictResult
  = -- | Solved by given or instance.
    DictSolved
  | -- | Cannot solve yet; leave in inert set.
    DictStuck !Ct
  deriving (Show)

-- | Attempt to solve a dictionary (class) constraint.
--
-- This covers the Haskell 2010 instance path used by the current Prelude:
-- match a wanted class predicate against an in-scope instance head, solve the
-- instance context recursively, and bind the wanted evidence to a dictionary
-- term. The plain entry point has no local givens; annotation generation uses
-- 'solveDictWithGivens' when elaborating inside a qualified binding.
solveDict :: Ct -> TcM DictResult
solveDict = solveDictWithGivens []

solveDictWithGivens :: [Pred] -> Ct -> TcM DictResult
solveDictWithGivens = solveDictWithGivensVisited []

solveDictWithGivensVisited :: [Pred] -> [Pred] -> Ct -> TcM DictResult
solveDictWithGivensVisited visited givens ct
  | ctPred ct `elem` visited = pure (DictStuck ct)
  | otherwise =
      case ctPred ct of
        ClassPred className args -> do
          args' <- mapM (reduceTypeFamilies <=< zonkType) args
          coercibleClass <- isCoercibleClass className
          givens' <- mapM zonkPred givens
          givenEvidence <- givenDict (ctPred ct : visited) givens' className args'
          case givenEvidence of
            Just evidence -> do
              bindEvidence (ctEvVar ct) evidence
              pure DictSolved
            Nothing ->
              case (tyConName className, args') of
                (_, [left, right]) | coercibleClass -> do
                  info <- lookupClass className
                  solved <- case info of
                    Just classInfo
                      | null (ciMethods classInfo),
                        null (ciSuperClassTypes classInfo),
                        null (ciKindTyVars classInfo) ->
                          solveCoercible left right
                    _ -> pure False
                  if solved
                    then do
                      bindEvidence (ctEvVar ct) (EvCoercible className left right)
                      pure DictSolved
                    else pure (DictStuck ct)
                ("Typeable", [ty]) -> tryTypeable className ty
                _ -> do
                  instances <- getClassInstances (tyConName className)
                  tryInstances (ctPred ct : visited) className args' instances
        quantified@QuantifiedPred {} -> solveQuantifiedWanted visited givens quantified
        EqPred {} -> pure (DictStuck ct)
        IParamPred name payload -> do
          payload' <- zonkType payload
          givens' <- mapM zonkPred givens
          -- The innermost binding of the name wins. Givens are outermost first.
          case [given | given@(IParamPred givenName _) <- reverse givens', givenName == name] of
            given@(IParamPred _ givenPayload) : _ -> do
              -- The name determines the type of an implicit parameter.
              unify (ctLoc ct) (ctOrigin ct) payload' givenPayload
              bindEvidence (ctEvVar ct) (implicitParamEvidence ct name givenPayload (EvGiven given))
              pure DictSolved
            _ -> pure (DictStuck ct)
  where
    givenDict visited' zonkedGivens className args =
      firstGivenOrSuperclass visited' (ClassPred className args) zonkedGivens

    firstGivenOrSuperclass _ _ [] = pure Nothing
    firstGivenOrSuperclass visited' target (given : rest)
      | target == given = pure (Just (EvGiven given))
      | otherwise = do
          quantified <- useQuantifiedEvidence visited' target (EvGiven given) given
          projected <- superclassEvidence [] visited' target (EvGiven given) given
          case quantified <|> projected of
            Just evidence -> pure (Just evidence)
            Nothing -> firstGivenOrSuperclass visited' target rest

    superclassEvidence classVisited solveVisited target sourceEvidence sourcePredicate =
      case sourcePredicate of
        ClassPred sourceClass sourceArgs
          | sourceClass `elem` classVisited -> pure Nothing
          | otherwise -> do
              classInfo <- lookupClass sourceClass
              case classInfo of
                Nothing -> pure Nothing
                Just info -> do
                  kinds <- getKinds
                  let substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (ciTyVars info) sourceArgs]
                      fieldTypes = classFieldTypes kinds info substitution
                  case traverse (constraintTypeToPred kinds . applySubst kinds substitution) (ciSuperClassTypes info) of
                    Just superClasses -> searchSuperClasses (sourceClass : classVisited) solveVisited sourceEvidence (ciOrigin info) sourcePredicate fieldTypes target 0 superClasses
                    Nothing -> pure Nothing
        _ -> pure Nothing

    searchSuperClasses _ _ _ _ _ _ _ _ [] = pure Nothing
    searchSuperClasses classVisited solveVisited sourceEvidence sourceOrigin sourcePredicate fieldTypes target index (superClass : rest)
      | superClass == target =
          pure (Just (EvSuperClass sourceEvidence sourceOrigin sourcePredicate fieldTypes index))
      | otherwise = do
          let projection = EvSuperClass sourceEvidence sourceOrigin sourcePredicate fieldTypes index
          quantified <- useQuantifiedEvidence solveVisited target projection superClass
          nested <- superclassEvidence classVisited solveVisited target projection superClass
          case quantified <|> nested of
            Just evidence -> pure (Just evidence)
            Nothing -> searchSuperClasses classVisited solveVisited sourceEvidence sourceOrigin sourcePredicate fieldTypes target (index + 1) rest

    tryInstances _ _ _ [] = pure (DictStuck ct)
    tryInstances visited' className args (instanceInfo : rest)
      | not (instanceIsForClass className instanceInfo) =
          tryInstances visited' className args rest
      | otherwise =
          case matchTypes (iiHead instanceInfo) args of
            Nothing -> tryInstances visited' className args rest
            Just subst -> do
              kinds <- getKinds
              let context = map (applySubstPred kinds subst) (iiContext instanceInfo)
                  typeArgs = map (applySubst kinds subst . TcTyVar) (iiTyVars instanceInfo)
              contextEvidence <- mapM (solveSubPred visited') context
              case sequence contextEvidence of
                Just evidence -> do
                  bindEvidence (ctEvVar ct) (EvDict (iiDictOrigin instanceInfo) (iiDictName instanceInfo) typeArgs evidence)
                  pure DictSolved
                Nothing -> tryInstances visited' className args rest

    solveSubPred visited' pred' = do
      ev <- freshEvVar
      case pred' of
        EqPred left right
          | pred' `elem` givens -> pure (Just (EvGiven pred'))
          | left == right -> pure (Just (EvCoercion (Refl left)))
          | otherwise -> pure Nothing
        _ -> do
          result <- solveDictWithGivensVisited visited' givens (ct {ctPred = pred', ctEvVar = ev})
          case result of
            DictSolved -> lookupEvidence ev
            DictStuck _ -> pure Nothing

    tryTypeable typeableTyCon ty =
      case typeableArguments ty of
        Nothing -> pure (DictStuck ct)
        Just arguments -> do
          classOrigin <- maybe Nothing ciOrigin <$> lookupClassByName "Typeable"
          argumentEvidence <- mapM (solveSubPred [ctPred ct] . ClassPred typeableTyCon . (: [])) arguments
          case sequence argumentEvidence of
            Just evidence -> do
              bindEvidence (ctEvVar ct) (EvTypeable classOrigin ty evidence)
              pure DictSolved
            Nothing -> pure (DictStuck ct)

    solveQuantifiedWanted visited' localGivens (QuantifiedPred variables antecedents consequent) = do
      kinds <- getKinds
      (freshVariables, substitution) <- freshQuantifiedVariables variables
      let instantiatedAntecedents = map (applySubstPred kinds substitution) antecedents
          instantiatedConsequent = applySubstPred kinds substitution consequent
      consequenceVariable <- freshEvVar
      result <-
        solveDictWithGivensVisited
          (ctPred ct : visited')
          (localGivens <> instantiatedAntecedents)
          (ct {ctPred = instantiatedConsequent, ctEvVar = consequenceVariable})
      case result of
        DictStuck _ -> pure (DictStuck ct)
        DictSolved -> do
          maybeBody <- lookupEvidence consequenceVariable
          case maybeBody of
            Nothing -> pure (DictStuck ct)
            Just body -> do
              antecedentTypes <- mapM predicateType instantiatedAntecedents
              let dictionaryBody = foldr (uncurry EvDictLam) body (zip instantiatedAntecedents antecedentTypes)
                  quantifiedBody = foldr EvTypeLam dictionaryBody freshVariables
              bindEvidence (ctEvVar ct) quantifiedBody
              pure DictSolved
    solveQuantifiedWanted _ _ _ = pure (DictStuck ct)

    useQuantifiedEvidence visited' target source (QuantifiedPred variables antecedents consequent) =
      searchQuantifiedChain
        visited'
        target
        variables
        (applyQuantifiedEvidence visited' source variables antecedents)
        consequent
        []
    useQuantifiedEvidence _ _ _ _ = pure Nothing

    applyQuantifiedEvidence visited' source variables antecedents substitution = do
      kinds <- getKinds
      let typeArguments = map (\variable -> Map.findWithDefault (TcTyVar variable) (tvUnique variable) substitution) variables
          instantiatedAntecedents = map (applySubstPred kinds substitution) antecedents
      antecedentEvidence <- mapM (solveSubPred visited') instantiatedAntecedents
      pure $ do
        evidence <- sequence antecedentEvidence
        pure (foldl EvDictApp (foldl EvTypeApp source typeArguments) evidence)

    searchQuantifiedChain visited' target variables build sourcePredicate classVisited =
      case matchQuantifiedPredicate variables sourcePredicate target of
        Just substitution -> build substitution
        Nothing ->
          case sourcePredicate of
            ClassPred sourceClass sourceArguments
              | sourceClass `elem` classVisited -> pure Nothing
              | otherwise -> do
                  classInfo <- lookupClass sourceClass
                  case classInfo of
                    Nothing -> pure Nothing
                    Just info -> do
                      kinds <- getKinds
                      let classSubstitution =
                            Map.fromList
                              [ (tvUnique variable, argument)
                              | (variable, argument) <- zip (ciTyVars info) sourceArguments
                              ]
                          fieldTypes = classFieldTypes kinds info classSubstitution
                      case traverse (constraintTypeToPred kinds . applySubst kinds classSubstitution) (ciSuperClassTypes info) of
                        Nothing -> pure Nothing
                        Just superClasses ->
                          searchQuantifiedSuperClasses
                            visited'
                            target
                            variables
                            build
                            sourcePredicate
                            (sourceClass : classVisited)
                            (ciOrigin info)
                            fieldTypes
                            0
                            superClasses
            _ -> pure Nothing

    searchQuantifiedSuperClasses _ _ _ _ _ _ _ _ _ [] = pure Nothing
    searchQuantifiedSuperClasses visited' target variables build sourcePredicate classVisited sourceOrigin fieldTypes index (superClass : rest) = do
      kinds <- getKinds
      let project substitution = do
            source <- build substitution
            pure $ do
              sourceEvidence <- source
              pure
                ( EvSuperClass
                    sourceEvidence
                    sourceOrigin
                    (applySubstPred kinds substitution sourcePredicate)
                    (map (applySubst kinds substitution) fieldTypes)
                    index
                )
      result <-
        case superClass of
          QuantifiedPred newVariables antecedents consequent ->
            searchQuantifiedChain
              visited'
              target
              (variables <> newVariables)
              ( \substitution -> do
                  projected <- project substitution
                  case projected of
                    Nothing -> pure Nothing
                    Just evidence -> applyQuantifiedEvidence visited' evidence newVariables antecedents substitution
              )
              consequent
              classVisited
          _ ->
            searchQuantifiedChain visited' target variables project superClass classVisited
      case result of
        Just evidence -> pure (Just evidence)
        Nothing ->
          searchQuantifiedSuperClasses visited' target variables build sourcePredicate classVisited sourceOrigin fieldTypes (index + 1) rest

    freshQuantifiedVariables = foldM freshOne ([], Map.empty)
      where
        freshOne (variables, substitution) variable = do
          kinds <- getKinds
          fresh <- freshSkolemTv (tvName variable)
          let kind = applySubst kinds substitution (tvKind variable)
              freshVariable = setTyVarKind kind fresh
          pure (variables <> [freshVariable], Map.insert (tvUnique variable) (TcTyVar freshVariable) substitution)

    predicateType predicate =
      case predicate of
        ClassPred classTyCon arguments -> pure (TcTyCon classTyCon arguments)
        EqPred left right -> do
          kinds <- getKinds
          equalityTyCon <- wiredTyCon tcWiringEqualityTyCon (KFun (typeKind kinds) (KFun (typeKind kinds) (constraintKind kinds)))
          pure (TcTyCon equalityTyCon [left, right])
        IParamPred name payload -> implicitParamType name payload
        QuantifiedPred variables antecedents consequent -> do
          consequentType <- predicateType consequent
          let qualified = if null antecedents then consequentType else TcQualTy antecedents consequentType
          pure (foldr TcForAllTy qualified variables)

typeableArguments :: TcType -> Maybe [TcType]
typeableArguments ty =
  case ty of
    TcTyCon _ arguments -> Just arguments
    TcFunTy argument result -> Just [argument, result]
    TcTyVar {} -> Nothing
    TcMetaTv {} -> Nothing
    TcForAllTy {} -> Nothing
    TcQualTy {} -> Nothing
    TcAppTy {} -> Nothing

classFieldTypes :: TcKinds -> ClassInfo -> Map Unique TcType -> [TcType]
classFieldTypes kinds classInfo substitution =
  map (applySubst kinds substitution) (ciSuperClassTypes classInfo)
    <> map (methodFieldType kinds classInfo substitution . snd) (ciMethods classInfo)

methodFieldType :: TcKinds -> ClassInfo -> Map Unique TcType -> TypeScheme -> TcType
methodFieldType kinds classInfo substitution (ForAll typeVariables predicates body) =
  applySubst kinds substitution $
    foldr TcForAllTy qualifiedBody extraTypeVariables
  where
    classVariables = ciTyVars classInfo
    extraTypeVariables = filter (`notElem` classVariables) typeVariables
    remainingPredicates = filter (not . isClassPredicate) predicates
    qualifiedBody
      | null remainingPredicates = body
      | otherwise = TcQualTy remainingPredicates body
    isClassPredicate predicate =
      case predicate of
        ClassPred className _ -> tyConKey className == tyConKey (ciTyCon classInfo)
        EqPred {} -> False
        QuantifiedPred {} -> False
        IParamPred {} -> False

-- | The evidence for a wanted implicit parameter from the evidence of its binding.
--
-- An occurrence of a function with a @HasCallStack@ constraint pushes its call
-- site onto the parent call stack.
implicitParamEvidence :: Ct -> Text -> TcType -> EvTerm -> EvTerm
implicitParamEvidence ct name payload parent =
  case (callStackOrigin name payload, ctOrigin ct, ctLoc ct) of
    (Just origin, OccurrenceOf function, SourceSpan file startLine startColumn endLine endColumn _ _) ->
      EvCallStackPush origin function (CallSite (T.pack file) startLine startColumn endLine endColumn) parent
    _ -> parent

-- | The package and module of the @CallStack@ type when the implicit
-- parameter is @?callStack :: CallStack@.
callStackOrigin :: Text -> TcType -> Maybe (Text, Text)
callStackOrigin name payload =
  case payload of
    TcTyCon tyCon []
      | name == "?callStack",
        tyConName tyCon == "CallStack" ->
          Just (packageIdText (tyConPackageId tyCon), tyConModuleName tyCon)
    _ -> Nothing

isCallStackPred :: Pred -> Bool
isCallStackPred predicate =
  case predicate of
    IParamPred name payload -> isJust (callStackOrigin name payload)
    _ -> False

-- | Report an unsolved dictionary constraint.
--
-- An unsolved call-stack parameter is not an error. It gets the empty call
-- stack, as in GHC.
reportUnsolvedDict :: Ct -> TcM ()
reportUnsolvedDict ct = do
  predicate <- zonkPred (ctPred ct)
  case predicate of
    IParamPred name payload
      | Just origin <- callStackOrigin name payload ->
          bindEvidence (ctEvVar ct) (implicitParamEvidence ct name payload (EvCallStackEmpty origin))
    _ -> emitError (ctLoc ct) (UnsolvedWanted predicate (ctOrigin ct))

matchQuantifiedPredicate :: [TyVarId] -> Pred -> Pred -> Maybe (Map Unique TcType)
matchQuantifiedPredicate variables patternPredicate targetPredicate =
  case (patternPredicate, targetPredicate) of
    (ClassPred patternClass patternArguments, ClassPred targetClass targetArguments)
      | patternClass == targetClass,
        length patternArguments == length targetArguments ->
          foldM matchOneQuantified Map.empty (zip patternArguments targetArguments)
    (EqPred patternLeft patternRight, EqPred targetLeft targetRight) ->
      foldM matchOneQuantified Map.empty [(patternLeft, targetLeft), (patternRight, targetRight)]
    _ -> Nothing
  where
    quantified = map tvUnique variables
    matchOneQuantified = matchTypeQuantified quantified

matchTypeQuantified :: [Unique] -> Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType)
matchTypeQuantified quantified substitution (TcTyVar variable, target)
  | tvUnique variable `elem` quantified =
      case Map.lookup (tvUnique variable) substitution of
        Nothing -> Just (Map.insert (tvUnique variable) target substitution)
        Just existing
          | existing == target -> Just substitution
          | otherwise -> Nothing
matchTypeQuantified quantified substitution (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments)
  | tyCon == targetTyCon,
    length arguments == length targetArguments =
      foldM (matchTypeQuantified quantified) substitution (zip arguments targetArguments)
matchTypeQuantified quantified substitution (TcFunTy argument result, TcFunTy targetArgument targetResult) =
  matchTypeQuantified quantified substitution (argument, targetArgument)
    >>= \substitution' -> matchTypeQuantified quantified substitution' (result, targetResult)
matchTypeQuantified quantified substitution (TcAppTy function argument, TcAppTy targetFunction targetArgument) =
  matchTypeQuantified quantified substitution (function, targetFunction)
    >>= \substitution' -> matchTypeQuantified quantified substitution' (argument, targetArgument)
matchTypeQuantified _ substitution (patternType, targetType)
  | patternType == targetType = Just substitution
  | otherwise = Nothing
