{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Convert checked kinds and types into System FC types.
module Aihc.Fc.Convert
  ( dictionaryPredicates,
    isEqualityPred,
    ConvertEnv (..),
    emptyConvertEnv,
    withTyVar,
    withTyVars,
    withKindEnv,
    withClassTyCons,
    withExportedNames,
    exportedVis,
    convertKind,
    convertRep,
    convertType,
    convertTypeWithExpectedKind,
    convertPred,
    tyVarBinder,
    tyConNameFc,
    classDictTypeName,
    classDictConName,
    lookupAxiomName,
    funType,
    liftedRepType,
    typeRep,
    extraKindVars,
    invisibleKindArgs,
    typeKindInEnv,
    evidenceArrows,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.Wired
import Aihc.Resolve (PackageId, ResolutionNamespace (..))
import Aihc.Tc.Types
  ( Pred (..),
    TcAxiomKey (..),
    TcKindEnv,
    TcType (..),
    TcTypeKey,
    TyCon,
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    applySubst,
    liftedRep,
    runtimeRepFromKind,
    tvKind,
    tyConKey,
    tyConModuleName,
    tyConName,
    tyConNamespace,
    tyConPackageId,
    pattern AddrRep,
    pattern BoxedRep,
    pattern DoubleRep,
    pattern FloatRep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern Int8Rep,
    pattern IntRep,
    pattern KConstraint,
    pattern KFun,
    pattern KLevity,
    pattern KMeta,
    pattern KRuntimeRep,
    pattern KTYPE,
    pattern KVecCount,
    pattern KVecElem,
    pattern Lifted,
    pattern SumRep,
    pattern TupleRep,
    pattern Unlifted,
    pattern VecRep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern Word8Rep,
    pattern WordRep,
  )
import Aihc.Tc.Types qualified as Tc
import Control.Monad (zipWithM)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data ConvertEnv = ConvertEnv
  { cePrimPackage :: PackageId,
    ceTyVars :: Map Unique TyVarId,
    ceKindEnv :: TcKindEnv,
    ceClassTyCons :: Set TcTypeKey,
    -- | The visible top-level names of the module, as
    -- 'Aihc.Resolve.exportedLocalNames' gives them. 'Nothing' comes from a
    -- caller that knows of no export list, and keeps every name public.
    ceExportedNames :: !(Maybe (Set (ResolutionNamespace, Text)))
  }

emptyConvertEnv :: PackageId -> ConvertEnv
emptyConvertEnv package =
  ConvertEnv
    { cePrimPackage = package,
      ceTyVars = Map.empty,
      ceKindEnv = Map.empty,
      ceClassTyCons = Set.empty,
      ceExportedNames = Nothing
    }

withClassTyCons :: [TcTypeKey] -> ConvertEnv -> ConvertEnv
withClassTyCons keys env =
  env {ceClassTyCons = Set.fromList keys <> ceClassTyCons env}

withExportedNames :: Maybe (Set (ResolutionNamespace, Text)) -> ConvertEnv -> ConvertEnv
withExportedNames names env = env {ceExportedNames = names}

-- | Whether one top-level name of the module is visible to other modules.
-- The namespace is part of the question: a data constructor and the type
-- constructor beside it can share a name but never an export item.
exportedVis :: ConvertEnv -> ResolutionNamespace -> Text -> Vis
exportedVis env namespace name =
  case ceExportedNames env of
    Nothing -> Pub
    Just names -> if Set.member (namespace, name) names then Pub else Private

classDictTypeName :: TyCon -> Name
classDictTypeName tyCon =
  Name ("$Dict$" <> tyConName tyCon) SortTypeConstructor (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

classDictConName :: TyCon -> Name
classDictConName tyCon =
  Name ("$Dict$" <> tyConName tyCon) SortDataConstructor (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

lookupAxiomName :: TcAxiomKey -> Name
lookupAxiomName (TcAxiomKey package moduleName' name) =
  Name name SortAxiom (OriginTop package moduleName')

withTyVar :: TyVarId -> ConvertEnv -> ConvertEnv
withTyVar tyVar env =
  env {ceTyVars = Map.insert (tvUnique tyVar) tyVar (ceTyVars env)}

withTyVars :: [TyVarId] -> ConvertEnv -> ConvertEnv
withTyVars tyVars env = foldr withTyVar env tyVars

withKindEnv :: TcKindEnv -> ConvertEnv -> ConvertEnv
withKindEnv kindEnv env = env {ceKindEnv = kindEnv <> ceKindEnv env}

convertKind :: ConvertEnv -> TcType -> Either String Type
convertKind env kind =
  case kind of
    KTYPE runtimeRep
      | runtimeRep == liftedRep -> Right (typeSynonym (cePrimPackage env))
      | otherwise -> TyApp (TyCon (typeConstructor (cePrimPackage env))) <$> convertRep env runtimeRep
    KConstraint -> Right (TyCon (constraintName (cePrimPackage env)))
    KRuntimeRep -> Right (TyCon (runtimeRepConstructor (cePrimPackage env)))
    KLevity -> Right (TyCon (levityConstructor (cePrimPackage env)))
    KVecCount -> Right (TyCon (wiredGhcTypes (cePrimPackage env) "VecCount" SortTypeConstructor))
    KVecElem -> Right (TyCon (wiredGhcTypes (cePrimPackage env) "VecElem" SortTypeConstructor))
    KFun argument result ->
      funType env <$> convertKind env argument <*> convertKind env result
    KMeta {} -> Left "kind still has a meta variable"
    _ -> convertType env kind

convertRep :: ConvertEnv -> TcType -> Either String Type
convertRep env runtimeRep =
  case runtimeRep of
    BoxedRep Lifted -> Right (TyCon (liftedRepName (cePrimPackage env)))
    BoxedRep Unlifted -> Right (TyCon (unliftedRepName (cePrimPackage env)))
    IntRep -> Right (repCon env "IntRep")
    Int8Rep -> Right (repCon env "Int8Rep")
    Int16Rep -> Right (repCon env "Int16Rep")
    Int32Rep -> Right (repCon env "Int32Rep")
    Int64Rep -> Right (repCon env "Int64Rep")
    WordRep -> Right (repCon env "WordRep")
    Word8Rep -> Right (repCon env "Word8Rep")
    Word16Rep -> Right (repCon env "Word16Rep")
    Word32Rep -> Right (repCon env "Word32Rep")
    Word64Rep -> Right (repCon env "Word64Rep")
    AddrRep -> Right (repCon env "AddrRep")
    FloatRep -> Right (repCon env "FloatRep")
    DoubleRep -> Right (repCon env "DoubleRep")
    TupleRep fields -> convertTuple fields
    SumRep fields -> do
      converted <- mapM (convertRep env) fields
      Right (TyApp (repCon env "SumRep") (runtimeRepList env converted))
    VecRep count element ->
      Right
        ( TyApp
            (TyApp (repCon env "VecRep") (repCon env (T.pack (show count))))
            (repCon env (T.pack (show element)))
        )
    TcTyVar tyVar ->
      let unique@(Unique uniqueValue) = tvUnique tyVar
       in case Map.lookup unique (ceTyVars env) of
            Just found -> Right (tyVarType found)
            Nothing -> Left ("unbound runtime-representation variable: rep" <> show uniqueValue)
    TcMetaTv {} -> Left "runtime representation still has a meta variable"
    _ -> convertType env runtimeRep
  where
    convertTuple fields = do
      converted <- mapM (convertRep env) fields
      Right (TyApp (repCon env "TupleRep") (runtimeRepList env converted))

runtimeRepList :: ConvertEnv -> [Type] -> Type
runtimeRepList env =
  foldr cons nil
  where
    runtimeRep = TyCon (runtimeRepConstructor (cePrimPackage env))
    nil = TyApp (repCon env "[]") runtimeRep
    cons item = TyApp (TyApp (TyApp (repCon env ":") runtimeRep) item)

repCon :: ConvertEnv -> Text -> Type
repCon env name = TyCon (wiredGhcTypes (cePrimPackage env) name SortDataConstructor)

convertType :: ConvertEnv -> TcType -> Either String Type
convertType env = convertTypeWithExpectedKind env Nothing

convertTypeWithExpectedKind :: ConvertEnv -> Maybe TcType -> TcType -> Either String Type
convertTypeWithExpectedKind env expectedKind ty =
  case ty of
    -- A saturated application of the arrow constructor is the function type.
    _ | Just (argument, result) <- saturatedArrowApplication ty -> convertTypeWithExpectedKind env expectedKind (TcFunTy argument result)
    TcTyVar tyVar -> Right (tyVarType tyVar)
    TcMetaTv {} -> Left "type still has a meta variable"
    -- The constraint type of an implicit parameter is the type of its value.
    TcTyCon tyCon [payload]
      | Tc.isImplicitParamTyConName (Tc.tyConName tyCon) -> convertType env payload
    -- The constraint type of an equality is the coercion type.
    TcTyCon tyCon [left, right]
      | Tc.tyConName tyCon == "~" -> TyEq <$> convertType env left <*> convertType env right
    TcTyCon tyCon arguments -> do
      kindArgs <- invisibleKindArgs env tyCon arguments expectedKind
      argumentKinds <- visibleArgumentKinds env tyCon arguments expectedKind
      converted <- zipWithM (convertTypeWithExpectedKind env) (map Just argumentKinds <> repeat Nothing) arguments
      pure (foldl TyApp (TyCon (tyConNameFc env tyCon)) (kindArgs <> converted))
    TcFunTy argument result -> do
      convertedArgument <- convertType env argument
      convertedResult <- convertType env result
      r1 <- typeRep env argument
      r2 <- typeRep env result
      pure (TyFun r1 r2 convertedArgument convertedResult)
    TcForAllTy tyVar body -> do
      binder <- tyVarBinder env tyVar
      convertedBody <- convertType (withTyVar tyVar env) body
      pure (TyForAll binder convertedBody)
    TcQualTy predicates body -> do
      convertedPredicates <- mapM (convertPred env) (dictionaryPredicates predicates)
      convertedBody <- convertType env body
      pure (evidenceArrows env body convertedPredicates convertedBody)
    TcAppTy function argument ->
      TyApp <$> convertType env function <*> convertType env argument

convertPred :: ConvertEnv -> Pred -> Either String Type
convertPred env predicate =
  case predicate of
    ClassPred tyCon arguments -> do
      kindArguments <- invisibleKindArgs env tyCon arguments Nothing
      argumentKinds <- visibleArgumentKinds env tyCon arguments Nothing
      converted <- zipWithM (convertTypeWithExpectedKind env) (map Just argumentKinds <> repeat Nothing) arguments
      pure (foldl TyApp (TyCon (classDictTypeName tyCon)) (kindArguments <> converted))
    EqPred left right ->
      TyEq <$> convertType env left <*> convertType env right
    -- The evidence for an implicit parameter is a plain value of its type.
    IParamPred _ payload -> convertType env payload
    QuantifiedPred variables antecedents consequent -> do
      let quantifiedEnv = withTyVars variables env
      binders <- mapM (tyVarBinder quantifiedEnv) variables
      convertedAntecedents <- mapM (convertPred quantifiedEnv) antecedents
      convertedConsequent <- convertPred quantifiedEnv consequent
      pure (foldr TyForAll (foldr (funType quantifiedEnv) convertedConsequent convertedAntecedents) binders)

-- | The argument and result of a saturated application of the arrow
-- constructor, in any of its forms.
saturatedArrowApplication :: TcType -> Maybe (TcType, TcType)
saturatedArrowApplication ty =
  case ty of
    TcAppTy (TcAppTy (TcTyCon tyCon []) argument) result
      | Tc.isArrowTyCon tyCon -> Just (argument, result)
    TcAppTy (TcTyCon tyCon [argument]) result
      | Tc.isArrowTyCon tyCon -> Just (argument, result)
    TcTyCon tyCon [argument, result]
      | Tc.isArrowTyCon tyCon -> Just (argument, result)
    _ -> Nothing

typeRep :: ConvertEnv -> TcType -> Either String Type
typeRep env ty = do
  kind <- typeKindInEnv env ty
  case runtimeRepFromKind kind of
    Left message -> Left (message <> " for " <> show ty)
    Right runtimeRep ->
      case convertRep env runtimeRep of
        Left message -> Left (message <> " for " <> show ty)
        Right converted -> Right converted

typeKindInEnv :: ConvertEnv -> TcType -> Either String TcType
typeKindInEnv env = Tc.typeKindInEnv (ceKindEnv env)

-- | The evidence arrows of a qualified type.
--
-- The innermost arrow returns the body, which can have a
-- representation-polymorphic kind, as in @HasCallStack => a@.
evidenceArrows :: ConvertEnv -> TcType -> [Type] -> Type -> Type
evidenceArrows env body convertedPredicates convertedBody = go convertedPredicates
  where
    bodyRep = fromRight (liftedRepType env) (typeRep env body)
    go [] = convertedBody
    go [predicate] = TyFun (liftedRepType env) bodyRep predicate convertedBody
    go (predicate : rest) = funType env predicate (go rest)

funType :: ConvertEnv -> Type -> Type -> Type
funType env = TyFun (liftedRepType env) (liftedRepType env)

liftedRepType :: ConvertEnv -> Type
liftedRepType env = TyCon (liftedRepName (cePrimPackage env))

tyVarBinder :: ConvertEnv -> TyVarId -> Either String Binder
tyVarBinder env tyVar = do
  kind <- convertKind (withTyVar tyVar env) (tvKind tyVar)
  pure (Binder (tyVarName tyVar) kind)

tyVarName :: TyVarId -> Name
tyVarName tyVar =
  Name (tvName tyVar) SortTypeVariable (OriginLocal (tvUnique tyVar))

tyVarType :: TyVarId -> Type
tyVarType tyVar = TyVar (tyVarName tyVar)

tyConNameFc :: ConvertEnv -> TyCon -> Name
tyConNameFc env tyCon =
  if Set.member (tyConKey tyCon) (ceClassTyCons env)
    then classDictTypeName tyCon
    else
      Name
        (tyConName tyCon)
        (namespaceSort (tyConNamespace tyCon))
        (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
  where
    namespaceSort ResolutionNamespaceTerm = SortDataConstructor
    namespaceSort ResolutionNamespaceType = SortTypeConstructor
    namespaceSort ResolutionNamespaceModule = SortTypeConstructor

-- | Invisible kind parameters that the type constructor quantifies before visible arguments.
extraKindVars :: ConvertEnv -> TyCon -> [TyVarId] -> Either String [TyVarId]
extraKindVars env tyCon visible = do
  ForAll vars _ _ <- kindScheme env tyCon
  let seen = map tvUnique visible
  pure (filter (\tyVar -> tvUnique tyVar `notElem` seen) vars)

invisibleKindArgs :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String [Type]
invisibleKindArgs env tyCon arguments expectedKind = do
  variables <- extraKindVars env tyCon []
  mapM (kindVarToType env tyCon arguments expectedKind) variables

-- | The kinds of the arguments give the invisible kind argument. A type
-- variable of the same identity in scope is the fallback: a default method
-- of a levity-polymorphic class quantifies the class kind variable while
-- its default signature fixes the kind of the class parameter.
kindVarToType :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> TyVarId -> Either String Type
kindVarToType env tyCon arguments expectedKind tyVar =
  case either (const Nothing) (Map.lookup (tvUnique tyVar)) (kindSubst env tyCon arguments expectedKind) of
    Just runtimeRep -> convertRep env runtimeRep
    Nothing ->
      case Map.lookup (tvUnique tyVar) (ceTyVars env) of
        Just found -> Right (tyVarType found)
        Nothing ->
          Left
            ( "cannot infer the invisible kind argument "
                <> show (tvUnique tyVar)
                <> " for "
                <> T.unpack (tyConName tyCon)
                <> " with arguments "
                <> show arguments
            )

kindSubst :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String (Map Unique TcType)
kindSubst env tyCon arguments expectedKind = do
  ForAll quantified _ resultKind <- kindScheme env tyCon
  let quantifiedUniques = map tvUnique quantified
      (argumentSubstitution, remainingKind) = go quantifiedUniques Map.empty resultKind arguments
      resultSubstitution =
        case expectedKind of
          Just expected -> matchKind quantifiedUniques remainingKind expected
          Nothing -> Map.empty
  pure (argumentSubstitution <> resultSubstitution)
  where
    go quantifiedUniques substitution (KFun formal result) (argument : rest) =
      case typeKindInEnv env argument of
        Right argumentKind ->
          let found = matchKind quantifiedUniques (applySubst substitution formal) argumentKind
           in go quantifiedUniques (substitution <> found) (applySubst found result) rest
        Left _ -> go quantifiedUniques substitution result rest
    go _ substitution kind _ = (substitution, applySubst substitution kind)

    matchKind quantifiedUniques (TcTyVar tyVar) actual
      | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) actual
    matchKind quantifiedUniques (KTYPE (TcTyVar tyVar)) (KTYPE runtimeRep)
      | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) runtimeRep
    matchKind quantifiedUniques (KFun left right) (KFun left' right') =
      matchKind quantifiedUniques left left' <> matchKind quantifiedUniques right right'
    matchKind quantifiedUniques (TcTyCon left formalArguments) (TcTyCon right actualArguments)
      | left == right,
        length formalArguments == length actualArguments =
          Map.unions (zipWith (matchKind quantifiedUniques) formalArguments actualArguments)
    matchKind _ _ _ = Map.empty

visibleArgumentKinds :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String [TcType]
visibleArgumentKinds env tyCon arguments expectedKind = do
  ForAll _ _ resultKind <- kindScheme env tyCon
  substitution <- kindSubst env tyCon arguments expectedKind
  pure (takeArgumentKinds (applySubst substitution resultKind))
  where
    takeArgumentKinds (KFun argument result) = argument : takeArgumentKinds result
    takeArgumentKinds _ = []

kindScheme :: ConvertEnv -> TyCon -> Either String TypeScheme
kindScheme env tyCon =
  case Map.lookup (tyConKey tyCon) (ceKindEnv env) of
    Just scheme -> Right scheme
    Nothing -> Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon))

-- | The predicates that carry runtime evidence. An equality is a fact of
-- the type checker only, so its evidence is erased.
dictionaryPredicates :: [Pred] -> [Pred]
dictionaryPredicates = filter (not . isEqualityPred)

isEqualityPred :: Pred -> Bool
isEqualityPred predicate =
  case predicate of
    EqPred {} -> True
    _ -> False
