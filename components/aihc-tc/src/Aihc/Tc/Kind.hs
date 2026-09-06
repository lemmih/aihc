{-# LANGUAGE OverloadedStrings #-}

module Aihc.Tc.Kind
  ( TvKindEnv,
    ParamInfo (..),
    checkSurfaceType,
    checkRuntimeType,
    convertSurfaceType,
    convertSurfaceTypeWithKinds,
    defaultKindMetas,
    freeTypeVars,
    freshKindMeta,
    classPredicateArgKinds,
    makeParamEnv,
    makeParamEnvWith,
    sigToScheme,
    explicitForallNames,
    scopedSigTyVars,
    standaloneKindSigToScheme,
    surfacePredToPred,
    takeVisibleArgumentKinds,
    tyConKindFromParams,
    tyConKindFromParamsWith,
    tcTypeKind,
    unifyKinds,
    unifyKindsAt,
    surfaceTypeSpan,
    zonkKind,
  )
where

import Aihc.Parser.Syntax
  ( Name (..),
    SourceSpan (..),
    TupleFlavor (..),
    TyVarBinder (..),
    Type (..),
    TypeBuiltinCon (..),
    UnqualifiedName (..),
    forallTelescopeBinders,
    fromAnnotation,
    instanceHeadName,
    instanceHeadTypes,
    nameText,
    peelTypeHead,
    tyVarBinderKind,
    tyVarBinderName,
    unqualifiedNameText,
  )
import Aihc.Resolve (ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Env (TyConInfo (..), TypeSynonymInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Instantiate (instantiate)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (foldM, zipWithM, zipWithM_)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

type TvKindEnv = Map Text (TyVarId, TcType)

data ParamInfo = ParamInfo
  { paramName :: !Text,
    paramTyVar :: !TyVarId,
    paramKind :: !TcType
  }
  deriving (Show)

-- | Convert a signature to a type scheme. A free type variable that is a
-- lexically scoped type variable refers to that variable and is not
-- quantified again.
sigToScheme :: Type -> TcM TypeScheme
sigToScheme ty = do
  scoped <- getScopedTyVars
  let (explicitBinders, qualifiedBody) = splitForalls ty
      (context, body) = splitContext qualifiedBody
      freeVars = filter (`Map.notMember` scoped) (freeTypeVars ty)
  rawTvs <- mapM freshSkolemTv freeVars
  kinds <- mapM (const freshKindMeta) freeVars
  let implicitTvs = zipWith setTyVarKind kinds rawTvs
  let implicitEnv = scoped <> Map.fromList (zip freeVars (zip implicitTvs kinds))
  explicitParams <- makeParamEnvWith implicitEnv explicitBinders
  let explicitTvs = map paramTyVar explicitParams
      tvEnv =
        implicitEnv
          <> Map.fromList
            [ (paramName param, (paramTyVar param, paramKind param))
            | param <- explicitParams
            ]
  tcTy <- checkRuntimeType tvEnv body
  preds <- mapM (surfacePredToPred tvEnv) (filter (not . isEmptyContext) context)
  pure (ForAll (implicitTvs <> explicitTvs) preds tcTy)

-- | The names of the variables of the explicit outer @forall@ of a
-- signature.
explicitForallNames :: Type -> [Text]
explicitForallNames ty = map tyVarBinderName (fst (splitForalls ty))

-- | The type variables that a signature scopes over its binding. Only the
-- variables of an explicit outer @forall@ scope, as in GHC. The given
-- variables are the opened variables of the checked scheme; they keep the
-- source names.
scopedSigTyVars :: [Text] -> [TyVarId] -> Map Text (TyVarId, TcType)
scopedSigTyVars explicitNames tyVars =
  Map.fromList
    [ (tvName tyVar, (tyVar, tvKind tyVar))
    | tyVar <- tyVars,
      tvName tyVar `elem` explicitNames
    ]

-- | The empty context @() =>@. A pattern synonym signature uses it for an
-- empty required context before a provided context.
isEmptyContext :: Type -> Bool
isEmptyContext ty =
  case ty of
    TAnn _ inner -> isEmptyContext inner
    TParen inner -> isEmptyContext inner
    TTuple _ _ [] -> True
    TCon name _ -> nameText name == "()"
    _ -> False

standaloneKindSigToScheme :: Type -> TcM TypeScheme
standaloneKindSigToScheme ty = do
  let (explicitBinders, bodyType) = splitForalls ty
      freeVars = freeTypeVars ty
  rawTyVars <- mapM freshSkolemTv freeVars
  implicitKinds <- mapM (const freshKindMeta) freeVars
  let implicitTyVars = zipWith setTyVarKind implicitKinds rawTyVars
      implicitEnv = Map.fromList [(tvName tyVar, (tyVar, tvKind tyVar)) | tyVar <- implicitTyVars]
  explicitParams <- makeParamEnvWith implicitEnv explicitBinders
  let explicitTyVars = map paramTyVar explicitParams
      tyVarEnv =
        implicitEnv
          <> Map.fromList
            [ (paramName param, (paramTyVar param, paramKind param))
            | param <- explicitParams
            ]
  body <- kindFromSurfaceType tyVarEnv bodyType
  let (nestedTyVars, body') = prenexKindForalls body
  pure (ForAll (implicitTyVars <> explicitTyVars <> nestedTyVars) [] body')

prenexKindForalls :: TcType -> ([TyVarId], TcType)
prenexKindForalls kind =
  case kind of
    TcForAllTy tyVar body ->
      let (tyVars, body') = prenexKindForalls body
       in (tyVar : tyVars, body')
    TcFunTy argument result ->
      let (tyVars, result') = prenexKindForalls result
       in (tyVars, TcFunTy argument result')
    _ -> ([], kind)

convertSurfaceType :: Map Text TyVarId -> Type -> TcM TcType
convertSurfaceType tvMap ty = do
  let tvEnv = Map.map (\tv -> (tv, tvKind tv)) tvMap
  checkRuntimeType tvEnv ty

checkSurfaceType :: TvKindEnv -> Type -> TcType -> TcM TcType
checkSurfaceType tvEnv ty expected = do
  (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
  unifyKindsAt (surfaceTypeSpan ty) expected actual
  pure tcTy

-- | The first source span that a list of surface types gives.
surfaceTypeSpans :: [Type] -> SourceSpan
surfaceTypeSpans tys =
  case filter (/= NoSourceSpan) (map surfaceTypeSpan tys) of
    sp : _ -> sp
    [] -> NoSourceSpan

-- | The source span of a surface type, when its annotations give one.
surfaceTypeSpan :: Type -> SourceSpan
surfaceTypeSpan ty =
  case ty of
    TAnn ann inner ->
      case fromAnnotation ann of
        Just sp -> sp
        Nothing -> surfaceTypeSpan inner
    TParen inner -> surfaceTypeSpan inner
    _ -> NoSourceSpan

-- | Check that a surface type is a value-bearing type of kind @TYPE rep@.
-- Unconstrained kind metas default to lifted representation; explicitly
-- unlifted types retain their fixed representation.
checkRuntimeType :: TvKindEnv -> Type -> TcM TcType
checkRuntimeType tvEnv ty = do
  (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
  actual' <- zonkKind actual
  case actual' of
    KTYPE {} -> pure tcTy
    KMeta unique -> bindKindMetaAt (surfaceTypeSpan ty) unique KType >> pure tcTy
    _ -> emitError (surfaceTypeSpan ty) (KindMismatch KType actual') >> pure tcTy

convertSurfaceTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertSurfaceTypeWithKinds tvEnv ty =
  case ty of
    TAnn ann _
      | Just resolution <- (fromAnnotation ann :: Maybe ResolutionAnnotation),
        resolutionNamespace resolution == ResolutionNamespaceTerm ->
          convertNonSynonymTypeWithKinds tvEnv ty
    _ -> do
      expanded <- expandTypeSynonym tvEnv (peelTypeHead ty)
      case expanded of
        Just result -> pure result
        Nothing -> convertNonSynonymTypeWithKinds tvEnv (peelTypeHead ty)

convertNonSynonymTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertNonSynonymTypeWithKinds tvEnv ty =
  case ty of
    TAnn ann inner ->
      case fromAnnotation ann of
        Just resolution
          | resolutionNamespace resolution == ResolutionNamespaceTerm ->
              convertPromotedSyntaxType tvEnv resolution inner
        _ -> convertSurfaceTypeWithKinds tvEnv inner
    TVar name ->
      inferTypeVariable tvEnv name
    TCon name _ ->
      inferTypeConstructor name
    TBuiltinCon builtin ->
      inferBuiltinTypeConstructor builtin
    TStar {} ->
      knownType "GHC.Types" "Type" KType
    TApp f a -> do
      (fTy, fKind) <- convertSurfaceTypeWithKinds tvEnv f
      (aTy, aKind) <- convertSurfaceTypeWithKinds tvEnv a
      resultKind <- freshKindMeta
      unifyKindsAt (surfaceTypeSpans [a, f]) fKind (KFun aKind resultKind)
      resultKind' <- zonkKind resultKind
      pure (applyType fTy aTy, resultKind')
    TTypeApp f a -> do
      (fTy, fKind) <- convertSurfaceTypeWithKinds tvEnv f
      (aTy, aKind) <- convertSurfaceTypeWithKinds tvEnv a
      resultKind <- freshKindMeta
      unifyKindsAt (surfaceTypeSpans [a, f]) fKind (KFun aKind resultKind)
      resultKind' <- zonkKind resultKind
      pure (applyType fTy aTy, resultKind')
    TInfix lhs name _ rhs -> do
      constructor <- inferTypeConstructor name
      applySurfaceTypeArguments tvEnv constructor [lhs, rhs]
    TFun _ a b -> do
      aTy <- checkRuntimeType tvEnv a
      bTy <- checkRuntimeType tvEnv b
      pure (TcFunTy aTy bTy, KType)
    TTuple flavor _ args ->
      convertTupleType tvEnv flavor args
    TUnboxedSum args -> do
      tys <- mapM (checkRuntimeType tvEnv) args
      argumentKinds <- mapM tcTypeKind tys
      let arity = length tys
          resultKind = KTYPE (SumRep (map runtimeRepOrLifted argumentKinds))
          tyConKind' = foldr KFun resultKind argumentKinds
          name = "(#" <> bars (arity - 1) <> "#)"
      tyCon <- mkKnownTyCon "GHC.Types" name arity tyConKind'
      pure (TcTyCon tyCon tys, resultKind)
    TList _ [arg] ->
      convertListType tvEnv arg
    TKindSig inner kindTy -> do
      expected <- kindFromSurfaceType tvEnv kindTy
      checkSurfaceType tvEnv inner expected >>= \innerTy -> pure (innerTy, expected)
    TContext preds inner -> do
      predicates <- mapM (surfacePredToPred tvEnv) preds
      (innerType, innerKind) <- convertSurfaceTypeWithKinds tvEnv inner
      pure (TcQualTy predicates innerType, innerKind)
    TImplicitParam name payload -> do
      payloadType <- checkSurfaceType tvEnv payload KType
      constraintType <- implicitParamType name payloadType
      pure (constraintType, KConstraint)
    TForall telescope inner -> do
      params <- makeParamEnvWith tvEnv (forallTelescopeBinders telescope)
      let tvEnv' = tvEnv <> Map.fromList [(paramName p, (paramTyVar p, paramKind p)) | p <- params]
      (innerTy, innerKind) <- convertSurfaceTypeWithKinds tvEnv' inner
      pure (foldr (TcForAllTy . paramTyVar) innerTy params, innerKind)
    _ -> do
      emitError NoSourceSpan (OtherError ("unsupported surface type in kind checker: " <> take 80 (show ty)))
      meta <- freshMetaTv
      pure (meta, KType)

convertPromotedSyntaxType :: TvKindEnv -> ResolutionAnnotation -> Type -> TcM (TcType, TcType)
convertPromotedSyntaxType tvEnv resolution syntax =
  case peelTypeHead syntax of
    TList _ arguments ->
      convertDataConstructorList tvEnv arguments
    TTuple _ _ arguments ->
      convertResolvedConstructorApplication tvEnv resolution arguments
    _ -> convertSurfaceTypeWithKinds tvEnv syntax

convertListType :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertListType tvEnv argument = do
  argumentType <- checkSurfaceType tvEnv argument KType
  result <- listType argumentType
  pure (result, KType)

convertDataConstructorList :: TvKindEnv -> [Type] -> TcM (TcType, TcType)
convertDataConstructorList tvEnv arguments = do
  elementKind <- freshKindMeta
  argumentTypes <- mapM (\argument -> checkSurfaceType tvEnv argument elementKind) arguments
  resultKind <- listType elementKind
  let consKind = TcFunTy elementKind (TcFunTy resultKind resultKind)
  nilTyCon <- mkKnownDataCon "GHC.Types" "[]" 0 resultKind
  consTyCon <- mkKnownDataCon "GHC.Types" ":" 2 consKind
  let nil = TcTyCon nilTyCon []
      cons field rest = TcTyCon consTyCon [field, rest]
  pure (foldr cons nil argumentTypes, resultKind)

convertTupleType :: TvKindEnv -> TupleFlavor -> [Type] -> TcM (TcType, TcType)
convertTupleType tvEnv flavor arguments = do
  argumentTypes <-
    case flavor of
      Boxed -> mapM (\argument -> checkSurfaceType tvEnv argument KType) arguments
      Unboxed -> mapM (checkRuntimeType tvEnv) arguments
  argumentKinds <- mapM tcTypeKind argumentTypes
  let argumentReps = map runtimeRepOrLifted argumentKinds
      arity = length argumentTypes
      fallbackResultKind =
        case flavor of
          Boxed -> KType
          Unboxed -> KTYPE (TupleRep argumentReps)
      fallbackKind = foldr KFun fallbackResultKind argumentKinds
      typeName = tupleTyConText flavor arity
  maybeTyCon <- lookupTyCon typeName
  tyCon <-
    case maybeTyCon of
      Just info -> pure (tciTyCon info)
      Nothing -> mkKnownTyCon (tupleTyConModule flavor) typeName arity fallbackKind
  let tupleType = TcTyCon tyCon argumentTypes
  tupleKind <- tcTypeKind tupleType
  pure (tupleType, tupleKind)

convertResolvedConstructorApplication :: TvKindEnv -> ResolutionAnnotation -> [Type] -> TcM (TcType, TcType)
convertResolvedConstructorApplication tvEnv resolution arguments = do
  maybeInfo <- lookupResolvedTypeSyntax resolution
  case maybeInfo of
    Nothing -> inferUnknownType
    Just info -> do
      constructorKind <- instantiateTyConKind info
      applySurfaceTypeArguments tvEnv (TcTyCon (tciTyCon info) [], constructorKind) arguments

applySurfaceTypeArguments :: TvKindEnv -> (TcType, TcType) -> [Type] -> TcM (TcType, TcType)
applySurfaceTypeArguments tvEnv = foldM applyArgument
  where
    applyArgument (functionType, functionKind) argument = do
      (argumentType, argumentKind) <- convertSurfaceTypeWithKinds tvEnv argument
      resultKind <- freshKindMeta
      unifyKindsAt (surfaceTypeSpan argument) functionKind (KFun argumentKind resultKind)
      resultKind' <- zonkKind resultKind
      pure (applyType functionType argumentType, resultKind')

expandTypeSynonym :: TvKindEnv -> Type -> TcM (Maybe (TcType, TcType))
expandTypeSynonym tvEnv ty =
  case typeApplicationSpine ty of
    (TCon name _, arguments) -> do
      maybeInfo <- lookupResolvedTyCon name
      case maybeInfo >>= tciTypeSynonym of
        Just synonym
          | Just {} <- tsiBody synonym -> Just <$> instantiateTypeSynonym tvEnv (nameText name) synonym arguments
        _ -> pure Nothing
    _ -> pure Nothing

instantiateTypeSynonym :: TvKindEnv -> Text -> TypeSynonymInfo -> [Type] -> TcM (TcType, TcType)
instantiateTypeSynonym tvEnv synonymName synonym arguments =
  case tsiBody synonym of
    Nothing -> do
      emitError NoSourceSpan (OtherError ("recursive or incomplete type synonym: " <> T.unpack synonymName))
      meta <- freshMetaTv
      pure (meta, KType)
    Just body -> do
      let params = tsiParams synonym
          arity = length params
          (synonymArguments, remainingArguments) = splitAt arity arguments
      if length synonymArguments /= arity
        then do
          emitError NoSourceSpan (OtherError ("type synonym " <> T.unpack synonymName <> " is not fully applied"))
          meta <- freshMetaTv
          pure (meta, KType)
        else do
          checkedArguments <- zipWithM checkArgument params synonymArguments
          let substitution = Map.fromList (zip (map tvUnique params) checkedArguments)
          expandedBody <- expandTcTypeSynonyms Set.empty (applySubst substitution body)
          expandedKind <- tcTypeKind expandedBody
          applyRemainingArguments (expandedBody, expandedKind) remainingArguments
  where
    checkArgument param argument = checkSurfaceType tvEnv argument (tvKind param)

    applyRemainingArguments result [] = pure result
    applyRemainingArguments (functionType, functionKind) (argument : rest) = do
      (argumentType, argumentKind) <- convertSurfaceTypeWithKinds tvEnv argument
      resultKind <- freshKindMeta
      unifyKindsAt (surfaceTypeSpan argument) functionKind (KFun argumentKind resultKind)
      zonkedResultKind <- zonkKind resultKind
      applyRemainingArguments (applyType functionType argumentType, zonkedResultKind) rest

typeApplicationSpine :: Type -> (Type, [Type])
typeApplicationSpine = go []
  where
    go arguments (TAnn _ inner) = go arguments inner
    go arguments (TApp function argument) = go (argument : arguments) function
    go arguments (TTypeApp function argument) = go (argument : arguments) function
    go arguments headType = (headType, arguments)

expandTcTypeSynonyms :: Set TcTypeKey -> TcType -> TcM TcType
expandTcTypeSynonyms expanding ty =
  case ty of
    TcTyVar {} -> pure ty
    TcMetaTv {} -> pure ty
    TcTyCon tyCon arguments -> do
      expandedArguments <- mapM (expandTcTypeSynonyms expanding) arguments
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo >>= tciTypeSynonym of
        Just synonym
          | Just body <- tsiBody synonym,
            let params = tsiParams synonym,
            length expandedArguments >= length params ->
              if tyConKey tyCon `Set.member` expanding
                then do
                  emitError NoSourceSpan (OtherError ("recursive type synonym: " <> T.unpack (tyConName tyCon)))
                  pure (TcTyCon tyCon expandedArguments)
                else do
                  let (synonymArguments, remainingArguments) = splitAt (length params) expandedArguments
                      substitution = Map.fromList (zip (map tvUnique params) synonymArguments)
                      expandedBody = applySubst substitution body
                      expanding' = Set.insert (tyConKey tyCon) expanding
                  normalizedBody <- expandTcTypeSynonyms expanding' expandedBody
                  expandTcTypeSynonyms expanding' (foldl applyType normalizedBody remainingArguments)
        _ -> pure (TcTyCon tyCon expandedArguments)
    TcFunTy argument result -> TcFunTy <$> expandTcTypeSynonyms expanding argument <*> expandTcTypeSynonyms expanding result
    TcForAllTy tyVar body -> TcForAllTy tyVar <$> expandTcTypeSynonyms expanding body
    TcQualTy predicates body -> TcQualTy <$> mapM expandPredicate predicates <*> expandTcTypeSynonyms expanding body
    TcAppTy function argument -> applyType <$> expandTcTypeSynonyms expanding function <*> expandTcTypeSynonyms expanding argument
  where
    expandPredicate predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM (expandTcTypeSynonyms expanding) arguments
        EqPred left right -> EqPred <$> expandTcTypeSynonyms expanding left <*> expandTcTypeSynonyms expanding right
        IParamPred name payload -> IParamPred name <$> expandTcTypeSynonyms expanding payload
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            <$> mapM expandVariable variables
            <*> mapM expandPredicate antecedents
            <*> expandPredicate consequent
    expandVariable variable = do
      kind <- expandTcTypeSynonyms expanding (tvKind variable)
      pure (setTyVarKind kind variable)

inferTypeVariable :: TvKindEnv -> UnqualifiedName -> TcM (TcType, TcType)
inferTypeVariable tvEnv name =
  let n = unqualifiedNameText name
   in case Map.lookup n tvEnv of
        Just (tv, kind) -> pure (TcTyVar tv, kind)
        Nothing -> inferUnknownType

inferTypeConstructor :: Name -> TcM (TcType, TcType)
inferTypeConstructor name = do
  mInfo <- lookupResolvedTyCon name
  case mInfo of
    Just info
      | tyConModuleName (tciTyCon info) == "GHC.Types",
        tciName info == "Type" ->
          knownType "GHC.Types" "Type" KType
    Just info
      | tyConModuleName (tciTyCon info) == "GHC.Types",
        tciName info == "Constraint" ->
          knownType "GHC.Types" "Constraint" KType
    Just info -> do
      kind <- instantiateTyConKind info
      pure (TcTyCon (tciTyCon info) [], kind)
    Nothing ->
      case nameText name of
        "Type" -> knownType "GHC.Types" "Type" KType
        "Constraint" -> knownType "GHC.Types" "Constraint" KType
        _ -> inferUnknownType

instantiateTyConKind :: TyConInfo -> TcM TcType
instantiateTyConKind info = do
  (kindType, _) <- instantiate (tciKindScheme info)
  pure kindType

inferBuiltinTypeConstructor :: TypeBuiltinCon -> TcM (TcType, TcType)
inferBuiltinTypeConstructor builtin =
  case builtin of
    TBuiltinList ->
      do
        maybeInfo <- lookupTyCon "[]"
        tyCon <- maybe (mkKnownTyCon "GHC.Types" "[]" 1 (KFun KType KType)) (pure . tciTyCon) maybeInfo
        pure (TcTyCon tyCon [], KFun KType KType)
    TBuiltinCons -> do
      let kind = KFun KType (KFun (listTypeKind KType) (listTypeKind KType))
      tyCon <- mkKnownDataCon "GHC.Types" ":" 2 kind
      pure (TcTyCon tyCon [], kind)
    TBuiltinTuple arity ->
      let argKinds = replicate arity KType
          kind = foldr KFun KType argKinds
       in knownTypeWithArity "GHC.Tuple" (tupleTyConText Boxed arity) arity kind
    TBuiltinArrow -> do
      let kind = KFun KType (KFun KType KType)
      tyCon <- mkKnownTyCon "GHC.Types" "(->)" 2 kind
      pure (TcTyCon tyCon [], kind)

knownType :: Text -> Text -> TcType -> TcM (TcType, TcType)
knownType moduleName name = knownTypeWithArity moduleName name 0

knownTypeWithArity :: Text -> Text -> Int -> TcType -> TcM (TcType, TcType)
knownTypeWithArity moduleName name arity kind = do
  maybeInfo <- lookupTyCon name
  tyCon <- maybe (mkKnownTyCon moduleName name arity kind) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [], kind)

inferUnknownType :: TcM (TcType, TcType)
inferUnknownType = do
  kind <- freshKindMeta
  ty <- freshMetaTvOfKind kind
  pure (ty, kind)

makeParamEnv :: [TyVarBinder] -> TcM [ParamInfo]
makeParamEnv = makeParamEnvWith Map.empty

makeParamEnvWith :: TvKindEnv -> [TyVarBinder] -> TcM [ParamInfo]
makeParamEnvWith = go
  where
    go _ [] = pure []
    go tvEnv (binder : rest) = do
      rawTv <- freshSkolemTv (tyVarBinderName binder)
      kind <- maybe freshKindMeta (kindFromSurfaceType tvEnv) (tyVarBinderKind binder)
      let tv = setTyVarKind kind rawTv
          param =
            ParamInfo
              { paramName = tyVarBinderName binder,
                paramTyVar = tv,
                paramKind = kind
              }
          tvEnv' = Map.insert (paramName param) (tv, kind) tvEnv
      (param :) <$> go tvEnv' rest

-- | The kinds of the first visible arguments of a type constructor kind.
takeVisibleArgumentKinds :: Int -> TcType -> [TcType]
takeVisibleArgumentKinds = go
  where
    go remaining (KFun argument result)
      | remaining > 0 = argument : go (remaining - 1) result
    go _ _ = []

tyConKindFromParams :: [ParamInfo] -> Maybe Type -> TcM TcType
tyConKindFromParams = tyConKindFromParamsWith Map.empty

tyConKindFromParamsWith :: TvKindEnv -> [ParamInfo] -> Maybe Type -> TcM TcType
tyConKindFromParamsWith outerEnv params maybeResultKind = do
  let tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params] <> outerEnv
  resultKind <- maybe (pure KType) (kindFromSurfaceType tvEnv) maybeResultKind
  pure (foldr (KFun . paramKind) resultKind params)

kindFromSurfaceType :: TvKindEnv -> Type -> TcM TcType
kindFromSurfaceType tvEnv ty =
  case peelTypeHead ty of
    TStar {} -> pure KType
    other -> do
      (tcType, kind) <- convertSurfaceTypeWithKinds tvEnv other
      unifyKindsAt (surfaceTypeSpan ty) kind KType
      pure tcType

unifyKinds :: TcType -> TcType -> TcM ()
unifyKinds = unifyKindsAt NoSourceSpan

-- | Unify two kinds and report a mismatch at the given span.
unifyKindsAt :: SourceSpan -> TcType -> TcType -> TcM ()
unifyKindsAt sp expected actual = do
  expected' <- zonkKind expected
  actual' <- zonkKind actual
  case (expected', actual') of
    (TcMetaTv unique, kind) -> bindKindMetaAt sp unique kind
    (kind, TcMetaTv unique) -> bindKindMetaAt sp unique kind
    (TcTyVar left, TcTyVar right)
      | left == right -> pure ()
    (TcTyVar {}, kind)
      | isConcreteRuntimeRep kind -> pure ()
    (kind, TcTyVar {})
      | isConcreteRuntimeRep kind -> pure ()
    (TcTyCon left leftArguments, TcTyCon right rightArguments)
      | left == right,
        length leftArguments == length rightArguments ->
          zipWithM_ (unifyKindsAt sp) leftArguments rightArguments
    (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
      unifyKindsAt sp leftArgument rightArgument >> unifyKindsAt sp leftResult rightResult
    (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) ->
      unifyKindsAt sp leftFunction rightFunction >> unifyKindsAt sp leftArgument rightArgument
    (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody)
      | leftVar == rightVar -> unifyKindsAt sp leftBody rightBody
    (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody)
      | leftPredicates == rightPredicates -> unifyKindsAt sp leftBody rightBody
    _ -> emitError sp (KindMismatch expected' actual')

isConcreteRuntimeRep :: TcType -> Bool
isConcreteRuntimeRep ty =
  case ty of
    TcTyCon tyCon _ ->
      tyConModuleName tyCon == "GHC.Types"
        && tyConNamespace tyCon == ResolutionNamespaceTerm
        && "Rep" `T.isSuffixOf` tyConName tyCon
    _ -> False

bindKindMetaAt :: SourceSpan -> Unique -> TcType -> TcM ()
bindKindMetaAt sp u kind
  | kind == TcMetaTv u = pure ()
  | occursInKind u kind = emitError sp (KindMismatch (KMeta u) kind)
  | otherwise = writeMetaTv u kind

zonkKind :: TcType -> TcM TcType
zonkKind kind =
  case kind of
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      case solution of
        Nothing -> pure kind
        Just solved -> do
          zonked <- zonkKind solved
          writeMetaTv unique zonked
          pure zonked
    TcTyVar tyVar -> do
      kind' <- zonkKind (tvKind tyVar)
      pure (TcTyVar (setTyVarKind kind' tyVar))
    TcTyCon tyCon arguments -> do
      tyCon' <- configuredTyCon tyCon
      maybeSynonym <- lookupKindSynonym tyCon'
      case maybeSynonym of
        Just synonym
          | Just {} <- tsiBody synonym,
            length arguments >= length (tsiParams synonym) ->
              zonkKind =<< expandTcTypeSynonyms Set.empty (TcTyCon tyCon' arguments)
        _ -> TcTyCon tyCon' <$> mapM zonkKind arguments
    TcFunTy argument result -> TcFunTy <$> zonkKind argument <*> zonkKind result
    TcForAllTy tyVar body -> do
      kind' <- zonkKind (tvKind tyVar)
      TcForAllTy (setTyVarKind kind' tyVar) <$> zonkKind body
    TcQualTy predicates body -> TcQualTy <$> mapM zonkKindPred predicates <*> zonkKind body
    TcAppTy function argument -> TcAppTy <$> zonkKind function <*> zonkKind argument
  where
    zonkKindPred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM zonkKind arguments
        EqPred left right -> EqPred <$> zonkKind left <*> zonkKind right
        IParamPred name payload -> IParamPred name <$> zonkKind payload
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            <$> mapM zonkVariable variables
            <*> mapM zonkKindPred antecedents
            <*> zonkKindPred consequent
    zonkVariable variable = setTyVarKind <$> zonkKind (tvKind variable) <*> pure variable

-- | The synonym declaration of a type constructor in a kind, if it has one.
--
-- A promoted data constructor is never a synonym. The common kind
-- constructors @BoxedRep@ and @Lifted@ thus need no environment lookup.
lookupKindSynonym :: TyCon -> TcM (Maybe TypeSynonymInfo)
lookupKindSynonym tyCon
  | tyConNamespace tyCon == ResolutionNamespaceTerm = pure Nothing
  | otherwise = do
      maybeInfo <- lookupTyConByIdentity tyCon
      pure (maybeInfo >>= tciTypeSynonym)

defaultKindMetas :: TcType -> TcM TcType
defaultKindMetas kind =
  case kind of
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      case solution of
        Just solved -> do
          -- A partially solved kind such as @k1 -> k2@ keeps its shape; only
          -- the metas that remain inside it default.
          defaulted <- defaultKindMetas =<< zonkKind solved
          writeMetaTv unique defaulted
          pure defaulted
        Nothing -> do
          tracked <- isTrackedKindMeta unique
          if tracked
            then writeMetaTv unique KType >> pure KType
            else pure kind
    TcTyVar tyVar -> do
      kind' <- defaultKindMetas (tvKind tyVar)
      pure (TcTyVar (setTyVarKind kind' tyVar))
    KTYPE (TcMetaTv representation) -> do
      -- An open representation defaults to lifted, not to 'Type'. The meta
      -- may come from instantiating a representation-polymorphic kind, so
      -- this does not depend on kind-meta tracking.
      solution <- readMetaTv representation
      case solution of
        Just {} -> defaultKindMetas =<< zonkKind kind
        Nothing -> writeMetaTv representation liftedRep >> pure KType
    TcTyCon tyCon arguments -> TcTyCon tyCon <$> mapM defaultKindMetas arguments
    TcFunTy argument result -> TcFunTy <$> defaultKindMetas argument <*> defaultKindMetas result
    TcForAllTy tyVar body -> do
      kind' <- defaultKindMetas (tvKind tyVar)
      TcForAllTy (setTyVarKind kind' tyVar) <$> defaultKindMetas body
    TcQualTy predicates body -> TcQualTy <$> mapM defaultKindPred predicates <*> defaultKindMetas body
    TcAppTy function argument -> TcAppTy <$> defaultKindMetas function <*> defaultKindMetas argument
  where
    defaultKindPred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM defaultKindMetas arguments
        EqPred left right -> EqPred <$> defaultKindMetas left <*> defaultKindMetas right
        IParamPred name payload -> IParamPred name <$> defaultKindMetas payload
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            <$> mapM defaultVariable variables
            <*> mapM defaultKindPred antecedents
            <*> defaultKindPred consequent
    defaultVariable variable = setTyVarKind <$> defaultKindMetas (tvKind variable) <*> pure variable

freshKindMeta :: TcM TcType
freshKindMeta = do
  unique <- freshUnique
  trackKindMeta unique
  pure (TcMetaTv unique)

occursInKind :: Unique -> TcType -> Bool
occursInKind needle kind =
  case kind of
    TcMetaTv unique -> unique == needle
    TcTyVar tyVar -> occursInKind needle (tvKind tyVar)
    TcTyCon _ arguments -> any (occursInKind needle) arguments
    TcFunTy argument result -> occursInKind needle argument || occursInKind needle result
    TcForAllTy tyVar body -> occursInKind needle (tvKind tyVar) || occursInKind needle body
    TcQualTy predicates body -> any occursInPred predicates || occursInKind needle body
    TcAppTy function argument -> occursInKind needle function || occursInKind needle argument
  where
    occursInPred predicate =
      case predicate of
        ClassPred _ arguments -> any (occursInKind needle) arguments
        EqPred left right -> occursInKind needle left || occursInKind needle right
        IParamPred _ payload -> occursInKind needle payload
        QuantifiedPred variables antecedents consequent ->
          any (occursInKind needle . tvKind) variables
            || any occursInPred antecedents
            || occursInPred consequent

tcTypeKind :: TcType -> TcM TcType
tcTypeKind ty =
  case ty of
    TcTyVar tyVar -> zonkKind (tvKind tyVar)
    TcMetaTv unique -> readMetaTvKind unique >>= zonkKind
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      initialKind <-
        case maybeInfo of
          Just info -> instantiateTyConKind info
          Nothing -> do
            emitError NoSourceSpan (OtherError ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
            pure (foldr KFun KType (replicate (tyConArity tyCon) KType))
      foldM applyArgument initialKind arguments
    TcFunTy {} -> pure KType
    TcForAllTy _ body -> tcTypeKind body
    TcQualTy _ _ -> pure KType
    TcAppTy function argument -> tcTypeKind function >>= (`applyArgument` argument)
  where
    applyArgument functionKind argument = do
      functionKind' <- zonkKind functionKind
      case functionKind' of
        TcFunTy argumentKind resultKind -> do
          actualKind <- tcTypeKind argument
          unifyKinds argumentKind actualKind
          zonkKind resultKind
        TcMetaTv {} -> do
          argumentKind <- tcTypeKind argument
          resultKind <- freshKindMeta
          unifyKinds functionKind' (TcFunTy argumentKind resultKind)
          zonkKind resultKind
        _ -> do
          emitError NoSourceSpan (KindMismatch (TcFunTy KType KType) functionKind')
          pure KType

applyType :: TcType -> TcType -> TcType
applyType (TcTyCon tc args) arg = mkTyConApp tc (args ++ [arg])
applyType f arg = TcAppTy f arg

listType :: TcType -> TcM TcType
listType ty = do
  maybeInfo <- lookupTyCon "[]"
  tyCon <- maybe (mkKnownTyCon "GHC.Types" "[]" 1 (KFun KType KType)) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [ty])

listTypeKind :: TcType -> TcType
listTypeKind kind = KFun kind kind

runtimeRepOrLifted :: TcType -> TcType
runtimeRepOrLifted kind =
  case runtimeRepFromKind kind of
    Right runtimeRep -> runtimeRep
    Left _ -> liftedRep

freeTypeVars :: Type -> [Text]
freeTypeVars = nub . go
  where
    go (TVar name) = [unqualifiedNameText name]
    go (TApp f a) = go f ++ go a
    go (TTypeApp f a) = go f ++ go a
    go (TInfix lhs _ _ rhs) = go lhs ++ go rhs
    go (TFun _ a b) = go a ++ go b
    go (TTuple _ _ args) = concatMap go args
    go (TUnboxedSum args) = concatMap go args
    go (TList _ args) = concatMap go args
    go (TParen inner) = go inner
    go (TAnn _ inner) = go inner
    go (TKindSig inner kindTy) = go inner ++ go kindTy
    go (TContext preds inner) = concatMap go preds ++ go inner
    go (TForall telescope inner) =
      filter
        (`Set.notMember` boundNames)
        (concatMap binderKindVars binders ++ go inner)
      where
        binders = forallTelescopeBinders telescope
        boundNames = Set.fromList (map tyVarBinderName binders)
    go _ = []
    binderKindVars binder = maybe [] go (tyVarBinderKind binder)

splitContext :: Type -> ([Type], Type)
splitContext (TAnn _ inner) = splitContext inner
splitContext (TContext preds inner) = (preds, inner)
splitContext ty = ([], ty)

splitForalls :: Type -> ([TyVarBinder], Type)
splitForalls ty =
  case ty of
    TAnn _ inner -> splitForalls inner
    TParen inner -> splitForalls inner
    TForall telescope inner ->
      let (binders, body) = splitForalls inner
       in (forallTelescopeBinders telescope <> binders, body)
    _ -> ([], ty)

surfacePredToPred :: TvKindEnv -> Type -> TcM Pred
surfacePredToPred tvEnv ty = do
  let (binders, qualifiedBody) = splitForalls (peelTypeHead ty)
      (antecedentTypes, consequentType) = splitContext (peelTypeHead qualifiedBody)
  if null binders && null antecedentTypes
    then surfaceAtomicPredToPred tvEnv consequentType
    else do
      params <- makeParamEnvWith tvEnv binders
      let quantifiedEnv =
            tvEnv
              <> Map.fromList
                [ (paramName param, (paramTyVar param, paramKind param))
                | param <- params
                ]
      antecedents <- mapM (surfaceAtomicPredToPred quantifiedEnv) antecedentTypes
      consequent <- surfaceAtomicPredToPred quantifiedEnv consequentType
      pure (QuantifiedPred (map paramTyVar params) antecedents consequent)

surfaceAtomicPredToPred :: TvKindEnv -> Type -> TcM Pred
surfaceAtomicPredToPred tvEnv ty =
  case peelTypeHead ty of
    TImplicitParam name payload ->
      IParamPred name <$> checkSurfaceType tvEnv payload KType
    _ -> surfaceClassPredToPred tvEnv ty

surfaceClassPredToPred :: TvKindEnv -> Type -> TcM Pred
surfaceClassPredToPred tvEnv ty =
  case instanceHeadName (peelTypeHead ty) of
    Just className -> do
      let classNameText = nameText className
          headArgs = instanceHeadTypes (peelTypeHead ty)
      maybeClassInfo <- lookupResolvedTyCon className
      case maybeClassInfo of
        Just classInfo
          | Just {} <- tciTypeSynonym classInfo -> do
              -- A constraint synonym expands to one constraint.
              (expanded, _) <- convertSurfaceTypeWithKinds tvEnv ty
              case constraintTypeToPred expanded of
                Just predicate -> pure predicate
                Nothing -> do
                  emitError NoSourceSpan (OtherError ("constraint synonym does not expand to one constraint: " <> T.unpack classNameText))
                  abortTc "invalid constraint synonym expansion"
        Just classInfo -> do
          classKind <- predicateClassKind classInfo
          let argKinds = takeClassArgKinds (length headArgs) classKind
          args <- zipWithM (checkSurfaceType tvEnv) headArgs argKinds
          case (classNameText, args) of
            ("~", [left, right]) -> pure (EqPred left right)
            _ -> pure (ClassPred (tciTyCon classInfo) args)
        Nothing -> do
          emitError NoSourceSpan (OtherError ("unknown class predicate: " <> T.unpack classNameText))
          abortTc ("missing checked type constructor for class predicate " <> T.unpack classNameText)
    Nothing -> do
      emitError NoSourceSpan (OtherError ("invalid class predicate: " <> show ty))
      abortTc "invalid checked class predicate"

classPredicateArgKinds :: Name -> Int -> TcM [TcType]
classPredicateArgKinds className argCount = do
  mInfo <- lookupResolvedTyCon className
  case mInfo of
    Just info -> takeClassArgKinds argCount <$> predicateClassKind info
    Nothing -> mapM (const freshKindMeta) [1 .. argCount]

predicateClassKind :: TyConInfo -> TcM TcType
predicateClassKind info
  | tciName info == "Lift" = do
      representation <- freshMetaTvOfKind runtimeRepKind
      pure (KFun (KTYPE representation) KConstraint)
predicateClassKind info = do
  kind <- instantiateTyConKind info
  zonkKind kind

takeClassArgKinds :: Int -> TcType -> [TcType]
takeClassArgKinds n kind
  | n <= 0 = []
  | otherwise =
      case kind of
        KFun arg rest -> arg : takeClassArgKinds (n - 1) rest
        _ -> replicate n KType

tupleTyConText :: TupleFlavor -> Int -> Text
tupleTyConText flavor arity =
  case flavor of
    Boxed -> boxedTupleTyConName arity
    Unboxed -> unboxedTupleTyConName arity

tupleTyConModule :: TupleFlavor -> Text
tupleTyConModule flavor =
  case flavor of
    Boxed -> "GHC.Tuple"
    Unboxed -> "GHC.Types"

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = mconcat (replicate n "|")
