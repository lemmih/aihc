{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Normalize deriving syntax into typed plans for later strategy-specific
-- checking and System FC lowering. This module checks the shared structure;
-- individual deriving mechanisms remain responsible for inferring attached
-- contexts and validating their class-specific rules.
module Aihc.Tc.Deriving
  ( annotateAttachedDerivingTc,
    annotateStandaloneDerivingTc,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BinderHead,
    Decl (..),
    DerivingClause (..),
    DerivingStrategy (..),
    Extension,
    Name (..),
    SourceSpan (..),
    StandaloneDerivingDecl (..),
    Type (..),
    UnqualifiedName,
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    instanceHeadName,
    instanceHeadTypes,
    mkAnnotation,
    nameText,
    tyVarBinderName,
    unqualifiedNameAnns,
    unqualifiedNameText,
  )
import Aihc.Resolve (ResolutionAnnotation)
import Aihc.Tc.Annotations
  ( TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
  )
import Aihc.Tc.Deriving.Strategy (checkDerivingStrategy, defaultStockFallback)
import Aihc.Tc.Env (ClassInfo (..), DataTypeInfo, TyConFlavor (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (ParamInfo (..), TvKindEnv, checkSurfaceType, defaultKindMetas, freeTypeVars, freshKindMeta, makeParamEnv, surfacePredToPred, takeVisibleArgumentKinds, tcTypeKind, unifyKinds)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Aihc.Tc.Zonk (defaultPredKinds, defaultTyVarKinds, defaultTypeKinds)
import Control.Monad (filterM, zipWithM, zipWithM_)
import Data.List (nub, (\\))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

annotateAttachedDerivingTc :: [Extension] -> TyConFlavor -> BinderHead UnqualifiedName -> [DerivingClause] -> Decl -> TcM Decl
annotateAttachedDerivingTc extensions targetFlavor targetHead clauses decl = do
  plans <- checkAttachedDerivingPlans extensions targetFlavor targetHead clauses
  pure (annotateDerivingPlans plans decl)

annotateDerivingPlans :: [TcDerivingPlan] -> Decl -> Decl
annotateDerivingPlans [] decl = decl
annotateDerivingPlans plans decl =
  DeclAnn (mkAnnotation (TcDerivingAnnotation plans)) decl

checkAttachedDerivingPlans :: [Extension] -> TyConFlavor -> BinderHead UnqualifiedName -> [DerivingClause] -> TcM [TcDerivingPlan]
checkAttachedDerivingPlans _ _ targetHead [] = do
  mapM_ (freshSkolemTv . tyVarBinderName) (binderHeadParams targetHead)
  pure []
checkAttachedDerivingPlans extensions targetFlavor targetHead clauses = do
  rawParams <- makeParamEnv (binderHeadParams targetHead)
  let targetName = unqualifiedNameText (binderHeadName targetHead)
  targetInfo <-
    case mapMaybe (fromAnnotation @ResolutionAnnotation) (unqualifiedNameAnns (binderHeadName targetHead)) of
      resolution : _ -> lookupResolvedTypeSyntax resolution
      [] -> lookupTyCon targetName
  case targetInfo of
    Nothing -> missingTypeInfo ("deriving target " <> T.unpack targetName)
    Just info -> do
      -- The parameters take the kinds that the declaration inferred, so a
      -- higher-kinded parameter is not defaulted to 'Type' here.
      zipWithM_
        (unifyKinds . paramKind)
        rawParams
        (takeVisibleArgumentKinds (length rawParams) (typeSchemeBody (tciKindScheme info)))
      params <- mapM defaultParam rawParams
      let tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params]
      dataType <- lookupDataType (tciTyCon info)
      concat <$> mapM (checkClause info dataType params tvEnv) clauses
  where
    defaultParam param = do
      tyVar <- defaultTyVarKinds (paramTyVar param)
      pure param {paramTyVar = tyVar, paramKind = tvKind tyVar}

    checkClause targetInfo dataType params tvEnv clause = do
      classHeads <- attachedDerivingClassHeads clause
      catMaybes <$> mapM (checkOne targetInfo dataType params tvEnv (derivingStrategy clause)) classHeads

    checkOne targetInfo dataType params tvEnv strategy classHead = do
      (plan, hadErrors) <-
        withErrorTracking (checkAttachedDerivingPlan extensions targetFlavor targetInfo dataType params tvEnv strategy classHead)
      pure (if hadErrors then Nothing else plan)

data AttachedDerivingClassHead = AttachedDerivingClassHead
  { attachedClassName :: !Name,
    attachedClassArguments :: ![Type],
    attachedClassSpan :: !SourceSpan
  }

attachedDerivingClassHeads :: DerivingClause -> TcM [AttachedDerivingClassHead]
attachedDerivingClassHeads clause =
  case derivingClasses clause of
    Left className ->
      pure [AttachedDerivingClassHead className [] (nameSourceSpan className)]
    Right classTypes -> catMaybes <$> mapM classTypeHead classTypes
  where
    classTypeHead classType =
      case instanceHeadName classType of
        Just className ->
          pure
            ( Just
                AttachedDerivingClassHead
                  { attachedClassName = className,
                    attachedClassArguments = instanceHeadTypes classType,
                    attachedClassSpan = nameSourceSpan className `orSourceSpan` typeSpan classType
                  }
            )
        Nothing -> do
          emitError (typeSpan classType) (OtherError "invalid class in deriving clause")
          pure Nothing

checkAttachedDerivingPlan :: [Extension] -> TyConFlavor -> TyConInfo -> Maybe DataTypeInfo -> [ParamInfo] -> TvKindEnv -> Maybe DerivingStrategy -> AttachedDerivingClassHead -> TcM (Maybe TcDerivingPlan)
checkAttachedDerivingPlan extensions targetFlavor targetInfo dataType params tvEnv strategy classHead = do
  let className = nameText (attachedClassName classHead)
      classSpan = attachedClassSpan classHead
      suppliedArguments = attachedClassArguments classHead
  maybeClassInfo <- lookupClassNamed (attachedClassName classHead)
  case maybeClassInfo of
    Nothing -> do
      emitError classSpan (OtherError ("deriving target " <> T.unpack className <> " is not a type class"))
      pure Nothing
    Just classInfo ->
      case unsnoc (ciTyVars classInfo) of
        Nothing -> do
          emitError classSpan (OtherError ("deriving class " <> T.unpack className <> " has no target parameter"))
          pure Nothing
        Just (prefixClassVars, targetClassVar)
          | length suppliedArguments /= length prefixClassVars -> do
              emitError classSpan (derivingArityError className (length prefixClassVars) (length suppliedArguments))
              pure Nothing
          | otherwise -> do
              checkedArguments <- zipWithM (checkSurfaceType tvEnv) suppliedArguments (map tvKind prefixClassVars)
              targetKind <- defaultKindMetas (tvKind targetClassVar)
              targetType <- attachedTargetType classSpan targetInfo params targetKind
              checkedStrategy <- checkDerivingStrategy extensions targetFlavor className (ciOrigin classInfo) tvEnv targetKind classSpan strategy
              fallback <- defaultStockFallback className (ciOrigin classInfo) strategy checkedStrategy
              methods <- derivingClassMethods classInfo
              let headTypes = checkedArguments <> [targetType]
                  strategyTypes = case checkedStrategy of TcDerivingVia viaType -> [viaType]; _ -> []
                  quantified = filter (\param -> any (typeMentionsTyVar (paramTyVar param)) (headTypes <> strategyTypes)) params
              pure (Just ((mkDerivingPlan classSpan checkedStrategy classInfo (map paramTyVar quantified) headTypes dataType TcDerivingInferContext methods) {tcDerivingStockFallback = fallback}))

attachedTargetType :: SourceSpan -> TyConInfo -> [ParamInfo] -> TcType -> TcM TcType
attachedTargetType sourceSpan targetInfo params expectedKind = do
  let tyCon = tciTyCon targetInfo
      arguments = map (TcTyVar . paramTyVar) params
      candidates =
        [ TcTyCon tyCon (take argumentCount arguments)
        | argumentCount <- [length arguments, length arguments - 1 .. 0]
        ]
  matching <- filterM (fmap (kindAccepts expectedKind) . tcTypeKind) candidates
  case matching of
    target : _ -> pure target
    [] -> do
      emitError sourceSpan (KindMismatch expectedKind (typeSchemeBody (tciKindScheme targetInfo)))
      pure (TcTyCon tyCon arguments)

-- | Whether a target kind fits the kind that a class needs. A class whose
-- parameter has kind @TYPE r@ with a variable representation accepts a
-- type of any representation.
kindAccepts :: TcType -> TcType -> Bool
kindAccepts expected actual =
  expected == actual
    || case (expected, actual) of
      (KTYPE (TcTyVar _), KTYPE _) -> True
      (KTYPE (TcMetaTv _), KTYPE _) -> True
      _ -> False

annotateStandaloneDerivingTc :: [Extension] -> StandaloneDerivingDecl -> TcM Decl
annotateStandaloneDerivingTc extensions derivingDecl = do
  (maybePlan, hadErrors) <- withErrorTracking (checkStandaloneDerivingPlan extensions derivingDecl)
  let plans = if hadErrors then [] else maybeToList maybePlan
  pure (annotateDerivingPlans plans (DeclStandaloneDeriving derivingDecl))

checkStandaloneDerivingPlan :: [Extension] -> StandaloneDerivingDecl -> TcM (Maybe TcDerivingPlan)
checkStandaloneDerivingPlan extensions derivingDecl =
  case instanceHeadName (standaloneDerivingHead derivingDecl) of
    Nothing -> do
      emitError (typeSpan (standaloneDerivingHead derivingDecl)) (OtherError "invalid standalone deriving instance head")
      pure Nothing
    Just classNameSyntax -> do
      let className = nameText classNameSyntax
          classSpan = nameSourceSpan classNameSyntax `orSourceSpan` typeSpan (standaloneDerivingHead derivingDecl)
          headArguments = instanceHeadTypes (standaloneDerivingHead derivingDecl)
          surfaceTypes = standaloneDerivingContext derivingDecl <> headArguments <> derivingStrategyTypes (standaloneDerivingStrategy derivingDecl)
          explicitNames = map tyVarBinderName (standaloneDerivingForall derivingDecl)
          implicitNames = nub (concatMap freeTypeVars surfaceTypes) \\ explicitNames
      explicitParams <- makeParamEnv (standaloneDerivingForall derivingDecl)
      implicitParams <- mapM implicitParam implicitNames
      let params = explicitParams <> implicitParams
          tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params]
      maybeClassInfo <- lookupClassNamed classNameSyntax
      case maybeClassInfo of
        Nothing -> do
          emitError classSpan (OtherError ("deriving target " <> T.unpack className <> " is not a type class"))
          pure Nothing
        Just classInfo
          | length headArguments /= length (ciTyVars classInfo) -> do
              emitError classSpan (standaloneDerivingArityError className (length (ciTyVars classInfo)) (length headArguments))
              pure Nothing
          | otherwise -> do
              checkedHead <- zipWithM (checkSurfaceType tvEnv) headArguments (map tvKind (ciTyVars classInfo))
              checkedContext <- mapM (surfacePredToPred tvEnv) (standaloneDerivingContext derivingDecl)
              let targetKind = maybe KType (tvKind . snd) (unsnoc (ciTyVars classInfo))
              targetFlavor <- standaloneTargetFlavor checkedHead
              checkedStrategy <- checkDerivingStrategy extensions targetFlavor className (ciOrigin classInfo) tvEnv targetKind classSpan (standaloneDerivingStrategy derivingDecl)
              tyVars <- mapM (defaultTyVarKinds . paramTyVar) params
              headTypes <- mapM defaultTypeKinds checkedHead
              context <- mapM defaultPredKinds checkedContext
              fallback <- defaultStockFallback className (ciOrigin classInfo) (standaloneDerivingStrategy derivingDecl) checkedStrategy
              strategy <- defaultDerivingStrategyKinds checkedStrategy
              methods <- derivingClassMethods classInfo
              dataType <- standaloneTargetDataType headTypes
              pure (Just ((mkDerivingPlan classSpan strategy classInfo tyVars headTypes dataType (TcDerivingExplicitContext context) methods) {tcDerivingStockFallback = fallback}))
  where
    implicitParam name = do
      rawTyVar <- freshSkolemTv name
      kind <- freshKindMeta
      let tyVar = setTyVarKind kind rawTyVar
      pure ParamInfo {paramName = name, paramTyVar = tyVar, paramKind = kind}

mkDerivingPlan :: SourceSpan -> TcDerivingStrategy -> ClassInfo -> [TyVarId] -> [TcType] -> Maybe DataTypeInfo -> TcDerivingContext -> [TcClassMethodAnnotation] -> TcDerivingPlan
mkDerivingPlan sourceSpan strategy classInfo tyVars headTypes dataType context methods =
  TcDerivingPlan
    { tcDerivingSourceSpan = sourceSpan,
      tcDerivingStrategy = strategy,
      tcDerivingStockFallback = False,
      tcDerivingClassName = className,
      tcDerivingClassTyCon = ciTyCon classInfo,
      tcDerivingClassOrigin = ciOrigin classInfo,
      tcDerivingTyVars = tyVars,
      tcDerivingHeadTypes = headTypes,
      tcDerivingDataType = dataType,
      tcDerivingContext = context,
      tcDerivingClassTyVars = ciTyVars classInfo,
      tcDerivingClassSuperClasses = map constraintTypeDictBinder (ciSuperClassTypes classInfo),
      tcDerivingClassMethods = methods,
      tcDerivingDefaultMethods = ciDefaultMethods classInfo,
      tcDerivingDefaultSignatures = [(methodName, predicates) | (methodName, ForAll _ predicates _) <- ciDefaultSignatures classInfo]
    }
  where
    className = ciName classInfo

derivingClassMethods :: ClassInfo -> TcM [TcClassMethodAnnotation]
derivingClassMethods classInfo =
  zipWithM method [0 :: Int ..] (ciMethods classInfo)
  where
    method index (methodName, scheme) = do
      let methodType = schemeToType scheme
          dictType = classDictionaryType classInfo
      pure
        TcClassMethodAnnotation
          { tcClassMethodName = methodName,
            tcClassMethodType = methodType,
            tcClassMethodTyVars = fst (peelForAlls methodType),
            tcClassMethodDictType = dictType,
            tcClassMethodIndex = index
          }

classDictionaryType :: ClassInfo -> TcType
classDictionaryType classInfo =
  TcTyCon (ciTyCon classInfo) (map TcTyVar (ciTyVars classInfo))

derivingStrategyTypes :: Maybe DerivingStrategy -> [Type]
derivingStrategyTypes (Just (DerivingVia viaType)) = [viaType]
derivingStrategyTypes _ = []

standaloneTargetFlavor :: [TcType] -> TcM TyConFlavor
standaloneTargetFlavor headTypes =
  case reverse headTypes of
    targetType : _
      | Just targetName <- tcTypeConstructorName targetType -> do
          maybeInfo <- lookupTyCon targetName
          pure (maybe DataTyCon tciFlavor maybeInfo)
    _ -> pure DataTyCon

standaloneTargetDataType :: [TcType] -> TcM (Maybe DataTypeInfo)
standaloneTargetDataType headTypes =
  case reverse headTypes of
    targetType : _ -> maybe (pure Nothing) lookupDataType (tcTypeConstructor targetType)
    _ -> pure Nothing

tcTypeConstructor :: TcType -> Maybe TyCon
tcTypeConstructor ty =
  case ty of
    TcTyCon tyCon _ -> Just tyCon
    TcAppTy function _ -> tcTypeConstructor function
    _ -> Nothing

tcTypeConstructorName :: TcType -> Maybe Text
tcTypeConstructorName = fmap tyConName . tcTypeConstructor

defaultDerivingStrategyKinds :: TcDerivingStrategy -> TcM TcDerivingStrategy
defaultDerivingStrategyKinds strategy =
  case strategy of
    TcDerivingVia viaType -> TcDerivingVia <$> defaultTypeKinds viaType
    _ -> pure strategy

constraintTypeDictBinder :: TcType -> TcDictBinderAnnotation
constraintTypeDictBinder ty =
  case constraintTypeToPred ty of
    Just (ClassPred classTyCon arguments) -> TcDictBinderAnnotation (tyConName classTyCon) arguments ty
    _ -> TcDictBinderAnnotation "<constraint>" [] ty

schemeToType :: TypeScheme -> TcType
schemeToType (ForAll [] [] ty) = ty
schemeToType (ForAll tyVars [] ty) = foldr TcForAllTy ty tyVars
schemeToType (ForAll [] predicates ty) = TcQualTy predicates ty
schemeToType (ForAll tyVars predicates ty) = foldr TcForAllTy (TcQualTy predicates ty) tyVars

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tyVar body) =
  let (tyVars, inner) = peelForAlls body
   in (tyVar : tyVars, inner)
peelForAlls ty = ([], ty)

typeMentionsTyVar :: TyVarId -> TcType -> Bool
typeMentionsTyVar target ty =
  case ty of
    TcTyVar tyVar -> tyVar == target
    TcMetaTv {} -> False
    TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments
    TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result
    TcForAllTy tyVar body -> tyVar /= target && typeMentionsTyVar target body
    TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body
    TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument

predicateMentionsTyVar :: TyVarId -> Pred -> Bool
predicateMentionsTyVar target predicate =
  case predicate of
    ClassPred _ arguments -> any (typeMentionsTyVar target) arguments
    EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right
    IParamPred _ payload -> typeMentionsTyVar target payload
    QuantifiedPred variables antecedents consequent ->
      target `notElem` variables
        && (any (predicateMentionsTyVar target) antecedents || predicateMentionsTyVar target consequent)

derivingArityError :: Text -> Int -> Int -> TcErrorKind
derivingArityError className expected supplied =
  OtherError
    ( "deriving class "
        <> T.unpack className
        <> " expects "
        <> show expected
        <> " argument(s) before the instance target, but got "
        <> show supplied
    )

standaloneDerivingArityError :: Text -> Int -> Int -> TcErrorKind
standaloneDerivingArityError className expected supplied =
  OtherError
    ( "standalone deriving class "
        <> T.unpack className
        <> " expects "
        <> show expected
        <> " instance argument(s), but got "
        <> show supplied
    )

nameSourceSpan :: Name -> SourceSpan
nameSourceSpan = sourceSpanFromAnns . nameAnns

sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns annotations =
  case [sourceSpan | annotation <- annotations, Just sourceSpan <- [fromAnnotation @SourceSpan annotation]] of
    sourceSpan : _ -> sourceSpan
    [] -> NoSourceSpan

typeSpan :: Type -> SourceSpan
typeSpan ty =
  case ty of
    TAnn annotation inner -> fromMaybe (typeSpan inner) (fromAnnotation @SourceSpan annotation)
    TParen inner -> typeSpan inner
    TForall _ inner -> typeSpan inner
    TContext _ inner -> typeSpan inner
    TKindSig inner _ -> typeSpan inner
    _ -> NoSourceSpan

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sourceSpan _ = sourceSpan

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc values = Just (init values, last values)

missingTypeInfo :: String -> TcM a
missingTypeInfo message =
  abortTc ("internal type annotation error: missing " <> message)
