{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core type representation for the type checker.
module Aihc.Tc.Types
  ( Unique (..),
    TyVarId (TyVarId, tvName, tvUnique),
    tvKind,
    setTyVarKind,
    TcType (..),
    TcTypeKey,
    TcAxiomKey (..),
    TcKindEnv,
    TyCon (TyCon, tyConName, tyConArity),
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    isArrowTyCon,
    isEqualityTyCon,
    mkAppTy,
    mkTyConApp,
    tyConNamespace,
    mkTyConWithOrigin,
    mkTyConWithNamespace,
    TypeScheme (..),
    typeKindInEnv,
    runtimeRepKind,
    liftedRep,
    typeKindType,
    runtimeRepFromKind,
    isFixedRuntimeRep,
    runtimeRepOfTypeInEnv,
    isUnliftedTypeInEnv,
    pattern KTYPE,
    pattern KConstraint,
    pattern KRuntimeRep,
    pattern KLevity,
    pattern KVecCount,
    pattern KVecElem,
    pattern KFun,
    pattern KMeta,
    pattern KType,
    pattern BoxedRep,
    pattern TupleRep,
    pattern SumRep,
    pattern VecRep,
    pattern Lifted,
    pattern Unlifted,
    pattern IntRep,
    pattern Int8Rep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern WordRep,
    pattern Word8Rep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern AddrRep,
    pattern FloatRep,
    pattern DoubleRep,
    typeSchemeBody,
    applySubst,
    applySubstPred,
    Pred (..),
    constraintTypeToPred,
    collectForAllTypes,
    collectTypeApplications,
    isImplicitParamTyConName,
    TcLevel (..),
    topTcLevel,
    pushLevel,
  )
where

import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Control.Monad (zipWithM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

newtype Unique = Unique Int
  deriving (Eq, Ord, Show, Read)

-- | A type variable and its type-level kind.
data TyVarId = TyVarIdInternal !Text !Unique !TcType
  deriving (Eq, Ord, Show, Read)

pattern TyVarId :: Text -> Unique -> TyVarId
pattern TyVarId {tvName, tvUnique} <- TyVarIdInternal tvName tvUnique _
  where
    TyVarId name unique = TyVarIdInternal name unique typeKindType

{-# COMPLETE TyVarId #-}

tvKind :: TyVarId -> TcType
tvKind (TyVarIdInternal _ _ kind) = kind

setTyVarKind :: TcType -> TyVarId -> TyVarId
setTyVarKind kind (TyVarIdInternal name unique _) = TyVarIdInternal name unique kind

-- | A type-constructor identity. Kind schemes live in the type-constructor environment.
data TyCon = TyConInternal !PackageId !Text !ResolutionNamespace !Text !Int
  deriving (Eq, Ord, Show, Read)

pattern TyCon :: Text -> Int -> TyCon
pattern TyCon {tyConName, tyConArity} <- TyConInternal _ _ _ tyConName tyConArity

{-# COMPLETE TyCon #-}

type TcTypeKey = (PackageId, Text, ResolutionNamespace, Text)

-- | Package, module, and axiom name. This identity is unique across modules.
data TcAxiomKey = TcAxiomKey
  { axiomKeyPackage :: !PackageId,
    axiomKeyModule :: !Text,
    axiomKeyName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

type TcKindEnv = Map TcTypeKey TypeScheme

tyConPackageId :: TyCon -> PackageId
tyConPackageId (TyConInternal packageId _ _ _ _) = packageId

tyConModuleName :: TyCon -> Text
tyConModuleName (TyConInternal _ moduleName _ _ _) = moduleName

-- | Whether a type constructor is the function arrow @(->)@.
isArrowTyCon :: TyCon -> Bool
isArrowTyCon tyCon =
  tyConName tyCon == "(->)" && tyConModuleName tyCon == "GHC.Types"

-- | Apply a type constructor to arguments. A saturated function arrow is
-- the function type, so the arrow constructor and the function type are
-- one form.
mkTyConApp :: TyCon -> [TcType] -> TcType
mkTyConApp tyCon arguments =
  case arguments of
    [argument, result] | isArrowTyCon tyCon -> TcFunTy argument result
    _ -> TcTyCon tyCon arguments

-- | Apply a type to an argument. An application of a type constructor
-- stays a constructor application, so a saturated arrow becomes the
-- function type.
mkAppTy :: TcType -> TcType -> TcType
mkAppTy function argument =
  case function of
    TcTyCon tyCon arguments -> mkTyConApp tyCon (arguments <> [argument])
    _ -> TcAppTy function argument

tyConNamespace :: TyCon -> ResolutionNamespace
tyConNamespace (TyConInternal _ _ namespace _ _) = namespace

tyConKey :: TyCon -> TcTypeKey
tyConKey tyCon = (tyConPackageId tyCon, tyConModuleName tyCon, tyConNamespace tyCon, tyConName tyCon)

mkTyConWithOrigin :: PackageId -> Text -> Text -> Int -> TyCon
mkTyConWithOrigin packageId moduleName =
  TyConInternal packageId moduleName ResolutionNamespaceType

mkTyConWithNamespace :: ResolutionNamespace -> PackageId -> Text -> Text -> Int -> TyCon
mkTyConWithNamespace namespace packageId moduleName =
  TyConInternal packageId moduleName namespace

-- | Internal types. Kinds use this same representation.
data TcType
  = TcTyVar !TyVarId
  | TcMetaTv !Unique
  | TcTyCon !TyCon ![TcType]
  | TcFunTy !TcType !TcType
  | TcForAllTy !TyVarId !TcType
  | TcQualTy ![Pred] !TcType
  | TcAppTy !TcType !TcType
  deriving (Eq, Ord, Show, Read)

data TypeScheme = ForAll ![TyVarId] ![Pred] !TcType
  deriving (Eq, Ord, Show, Read)

typeSchemeBody :: TypeScheme -> TcType
typeSchemeBody (ForAll _ _ body) = body

data Pred
  = ClassPred !TyCon ![TcType]
  | EqPred !TcType !TcType
  | QuantifiedPred ![TyVarId] ![Pred] !Pred
  | -- | An implicit parameter such as @?x :: Int@. The name keeps its @?@ prefix.
    IParamPred !Text !TcType
  deriving (Eq, Ord, Show, Read)

-- | Convert a constraint-kinded type to a predicate.
constraintTypeToPred :: TcType -> Maybe Pred
constraintTypeToPred ty =
  case collectForAllTypes ty of
    (variables@(_ : _), qualified) -> do
      let (antecedents, consequentType) =
            case qualified of
              TcQualTy predicates body -> (predicates, body)
              body -> ([], body)
      consequent <- atomicConstraintTypeToPred consequentType
      pure (QuantifiedPred variables antecedents consequent)
    ([], body) -> atomicConstraintTypeToPred body

atomicConstraintTypeToPred :: TcType -> Maybe Pred
atomicConstraintTypeToPred ty =
  case collectTypeApplications ty of
    (TcTyCon tyCon headArgs, arguments)
      | isEqualityTyCon tyCon,
        [left, right] <- headArgs <> arguments ->
          Just (EqPred left right)
    (TcTyCon tyCon [payload], [])
      | isImplicitParamTyConName (tyConName tyCon) -> Just (IParamPred (tyConName tyCon) payload)
    (TcTyCon tyCon headArgs, arguments) ->
      Just (ClassPred tyCon (headArgs <> arguments))
    _ -> Nothing

-- | The exported nominal equality type in the primitive module.
isEqualityTyCon :: TyCon -> Bool
isEqualityTyCon tyCon =
  tyConModuleName tyCon == "GHC.Types"
    && tyConNamespace tyCon == ResolutionNamespaceType
    && tyConName tyCon == "~"
    && tyConArity tyCon == 2

-- | The name of the constraint type constructor for one implicit parameter.
isImplicitParamTyConName :: Text -> Bool
isImplicitParamTyConName = T.isPrefixOf "?"

collectForAllTypes :: TcType -> ([TyVarId], TcType)
collectForAllTypes (TcForAllTy variable body) =
  let (variables, result) = collectForAllTypes body
   in (variable : variables, result)
collectForAllTypes ty = ([], ty)

collectTypeApplications :: TcType -> (TcType, [TcType])
collectTypeApplications ty =
  case ty of
    TcAppTy function argument ->
      let (headType, arguments) = collectTypeApplications function
       in (headType, arguments <> [argument])
    _ -> (ty, [])

-- These values identify source declarations. They do not contain kind data.
primTypeCon :: Text -> Int -> TyCon
primTypeCon = mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types"

primDataCon :: Text -> Int -> TyCon
primDataCon = mkTyConWithNamespace ResolutionNamespaceTerm (PackageId "aihc-prim") "GHC.Types"

constraintTyCon, runtimeRepTyCon :: TyCon
constraintTyCon = primTypeCon "Constraint" 0
runtimeRepTyCon = primTypeCon "RuntimeRep" 0

typeKindType, constraintKind, runtimeRepKind :: TcType
typeKindType = mkTYPEKind liftedRep
constraintKind = TcTyCon constraintTyCon []
runtimeRepKind = TcTyCon runtimeRepTyCon []

mkTYPEKind :: TcType -> TcType
mkTYPEKind representation = TcTyCon (primTypeCon "TYPE" 1) [representation]

nullaryRep :: Text -> TcType
nullaryRep name = TcTyCon (primDataCon name 0) []

liftedRep :: TcType
liftedRep = boxedRep (TcTyCon (primDataCon "Lifted" 0) [])

intRep, int8Rep, int16Rep, int32Rep, int64Rep :: TcType
wordRep, word8Rep, word16Rep, word32Rep, word64Rep, addrRep :: TcType
intRep = nullaryRep "IntRep"
int8Rep = nullaryRep "Int8Rep"
int16Rep = nullaryRep "Int16Rep"
int32Rep = nullaryRep "Int32Rep"
int64Rep = nullaryRep "Int64Rep"

wordRep = nullaryRep "WordRep"

word8Rep = nullaryRep "Word8Rep"

word16Rep = nullaryRep "Word16Rep"

word32Rep = nullaryRep "Word32Rep"

word64Rep = nullaryRep "Word64Rep"

addrRep = nullaryRep "AddrRep"

boxedRep :: TcType -> TcType
boxedRep levity = TcTyCon (primDataCon "BoxedRep" 1) [levity]

tupleRep :: [TcType] -> TcType
tupleRep fields = TcTyCon (primDataCon "TupleRep" 1) [dataConstructorList fields]

sumRep :: [TcType] -> TcType
sumRep fields = TcTyCon (primDataCon "SumRep" 1) [dataConstructorList fields]

dataConstructorList :: [TcType] -> TcType
dataConstructorList = foldr cons nil
  where
    nil = TcTyCon (primDataCon "[]" 0) []
    cons field rest = TcTyCon (primDataCon ":" 2) [field, rest]

-- | Get a type kind from the complete type-constructor identity table.
typeKindInEnv :: TcKindEnv -> TcType -> Either String TcType
typeKindInEnv kindEnv = go
  where
    go rawType =
      case configurePrimitiveType rawType of
        TcTyVar tyVar -> Right (configurePrimitiveType (tvKind tyVar))
        TcMetaTv {} -> Left "type still has a meta variable"
        TcTyCon tyCon arguments -> do
          scheme <-
            maybe
              (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
              Right
              (Map.lookup (tyConKey tyCon) kindEnv)
          applyArguments scheme arguments
        TcFunTy {} -> Right (configurePrimitiveType typeKindType)
        TcForAllTy _ body -> go body
        TcQualTy _ body -> go body
        TcAppTy function argument -> do
          functionKind <- go function
          applyKind functionKind argument

    applyArguments (ForAll quantified _ body) = applyMany (map tvUnique quantified) (configurePrimitiveType body)

    applyMany _ kind [] = Right kind
    applyMany quantified kind (argument : rest) = do
      kind' <- applyKindWith quantified kind argument
      applyMany quantified kind' rest

    applyKind = applyKindWith []

    applyKindWith quantified (TcFunTy formal result) argument = do
      actual <- go argument
      substitution <- matchKinds quantified formal actual
      Right (applySubst substitution result)
    applyKindWith _ kind _ = Left ("type application uses a non-function kind: " <> show kind)

    matchKinds quantified formal actual =
      case (formal, actual) of
        (TcTyVar tyVar, _)
          | tvUnique tyVar `elem` quantified -> Right (Map.singleton (tvUnique tyVar) actual)
        (KTYPE formalRep, KTYPE actualRep) -> matchKinds quantified formalRep actualRep
        (TcFunTy left right, TcFunTy left' right') ->
          Map.union <$> matchKinds quantified left left' <*> matchKinds quantified right right'
        (TcTyCon left leftArguments, TcTyCon right rightArguments)
          | left == right,
            length leftArguments == length rightArguments ->
              Map.unions <$> zipWithM (matchKinds quantified) leftArguments rightArguments
        _
          | equivalentKind formal actual -> Right Map.empty
          | otherwise -> Left ("kind mismatch: expected " <> show formal <> ", got " <> show actual)

    equivalentKind left right =
      configurePrimitiveType left == configurePrimitiveType right
        || case (left, right) of
          (KTYPE leftRep, KTYPE rightRep) -> leftRep == rightRep
          _ -> False

    configurePrimitiveType ty =
      case ty of
        TcTyVar tyVar -> TcTyVar (setTyVarKind (configurePrimitiveType (tvKind tyVar)) tyVar)
        TcMetaTv {} -> ty
        TcTyCon tyCon arguments -> TcTyCon (configurePrimitiveTyCon tyCon) (map configurePrimitiveType arguments)
        TcFunTy argument result -> TcFunTy (configurePrimitiveType argument) (configurePrimitiveType result)
        TcForAllTy tyVar body ->
          TcForAllTy
            (setTyVarKind (configurePrimitiveType (tvKind tyVar)) tyVar)
            (configurePrimitiveType body)
        TcQualTy predicates body -> TcQualTy (map configurePred predicates) (configurePrimitiveType body)
        TcAppTy function argument -> TcAppTy (configurePrimitiveType function) (configurePrimitiveType argument)

    configurePred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred (configurePrimitiveTyCon className) (map configurePrimitiveType arguments)
        EqPred left right -> EqPred (configurePrimitiveType left) (configurePrimitiveType right)
        IParamPred name payload -> IParamPred name (configurePrimitiveType payload)
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            (map (\variable -> setTyVarKind (configurePrimitiveType (tvKind variable)) variable) variables)
            (map configurePred antecedents)
            (configurePred consequent)

    configurePrimitiveTyCon tyCon
      | tyConPackageId tyCon == PackageId "aihc-prim",
        tyConModuleName tyCon == "GHC.Types" =
          mkTyConWithNamespace
            (tyConNamespace tyCon)
            primitivePackage
            "GHC.Types"
            (tyConName tyCon)
            (tyConArity tyCon)
      | otherwise = tyCon

    primitivePackage =
      case [ packageId
           | ((packageId, moduleName, namespace, name), _) <- Map.toList kindEnv,
             moduleName == "GHC.Types",
             namespace == ResolutionNamespaceType,
             name == "TYPE"
           ] of
        packageId : _ -> packageId
        [] -> PackageId "aihc-prim"

runtimeRepOfTypeInEnv :: TcKindEnv -> TcType -> Either String TcType
runtimeRepOfTypeInEnv kindEnv ty = typeKindInEnv kindEnv ty >>= runtimeRepFromKind

isUnliftedTypeInEnv :: TcKindEnv -> TcType -> Bool
isUnliftedTypeInEnv kindEnv ty =
  case runtimeRepOfTypeInEnv kindEnv ty of
    Right representation -> not (matchesLiftedRuntimeRep representation)
    Left _ -> False

-- | Apply a type-variable substitution to a type.
applySubst :: Map Unique TcType -> TcType -> TcType
applySubst substitution = go
  where
    go ty =
      case ty of
        TcTyVar tyVar -> Map.findWithDefault ty (tvUnique tyVar) substitution
        TcMetaTv {} -> ty
        TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments)
        TcFunTy argument result -> TcFunTy (go argument) (go result)
        TcForAllTy tyVar body ->
          TcForAllTy tyVar (applySubst (Map.delete (tvUnique tyVar) substitution) body)
        TcQualTy predicates body -> TcQualTy (map (applySubstPred substitution) predicates) (go body)
        TcAppTy function argument -> applyType (go function) (go argument)

    applyType = mkAppTy

-- | Apply a type-variable substitution to a predicate.
applySubstPred :: Map Unique TcType -> Pred -> Pred
applySubstPred substitution predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map (applySubst substitution) arguments)
    EqPred left right -> EqPred (applySubst substitution left) (applySubst substitution right)
    IParamPred name payload -> IParamPred name (applySubst substitution payload)
    QuantifiedPred variables antecedents consequent ->
      let scopedSubstitution = foldr (Map.delete . tvUnique) substitution variables
       in QuantifiedPred
            variables
            (map (applySubstPred scopedSubstitution) antecedents)
            (applySubstPred scopedSubstitution consequent)

pattern KTYPE :: TcType -> TcType
pattern KTYPE representation <- (matchTYPEKind -> Just representation)
  where
    KTYPE representation = mkTYPEKind representation

pattern KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KType :: TcType
pattern KConstraint <- (matchesNullary ResolutionNamespaceType "Constraint" -> True) where KConstraint = constraintKind
pattern KRuntimeRep <- (matchesNullary ResolutionNamespaceType "RuntimeRep" -> True)
pattern KLevity <- (matchesNullary ResolutionNamespaceType "Levity" -> True)
pattern KVecCount <- (matchesNullary ResolutionNamespaceType "VecCount" -> True)
pattern KVecElem <- (matchesNullary ResolutionNamespaceType "VecElem" -> True)
pattern KType <- (matchesLiftedTypeKind -> True) where KType = typeKindType

pattern KFun :: TcType -> TcType -> TcType
pattern KFun argument result = TcFunTy argument result

pattern KMeta :: Unique -> TcType
pattern KMeta unique = TcMetaTv unique

matchTYPEKind :: TcType -> Maybe TcType
matchTYPEKind kind =
  case kind of
    TcTyCon tyCon [representation]
      | tyConName tyCon == "TYPE" -> Just representation
    _ -> Nothing

matchesLiftedTypeKind :: TcType -> Bool
matchesLiftedTypeKind = maybe False matchesLiftedRuntimeRep . matchTYPEKind

matchesLiftedRuntimeRep :: TcType -> Bool
matchesLiftedRuntimeRep representation =
  case representation of
    TcTyCon boxed [TcTyCon levity []] ->
      tyConNamespace boxed == ResolutionNamespaceTerm
        && tyConName boxed == "BoxedRep"
        && tyConNamespace levity == ResolutionNamespaceTerm
        && tyConName levity == "Lifted"
    _ -> False

pattern BoxedRep :: TcType -> TcType
pattern BoxedRep levity <- (matchUnaryRep "BoxedRep" -> Just levity)

pattern TupleRep :: [TcType] -> TcType
pattern TupleRep fields <- (matchListRep "TupleRep" -> Just fields)
  where
    TupleRep fields = tupleRep fields

pattern SumRep :: [TcType] -> TcType
pattern SumRep fields <- (matchListRep "SumRep" -> Just fields)
  where
    SumRep fields = sumRep fields

pattern VecRep :: TcType -> TcType -> TcType
pattern VecRep count element <- (matchBinaryRep "VecRep" -> Just (count, element))

pattern Lifted, Unlifted :: TcType
pattern Lifted <- (matchesNullary ResolutionNamespaceTerm "Lifted" -> True)
pattern Unlifted <- (matchesNullary ResolutionNamespaceTerm "Unlifted" -> True)

pattern IntRep, Int8Rep, Int16Rep, Int32Rep, Int64Rep :: TcType

pattern WordRep, Word8Rep, Word16Rep, Word32Rep, Word64Rep :: TcType

pattern AddrRep, FloatRep, DoubleRep :: TcType

pattern IntRep <- (matchesNullary ResolutionNamespaceTerm "IntRep" -> True) where IntRep = intRep

pattern Int8Rep <- (matchesNullary ResolutionNamespaceTerm "Int8Rep" -> True) where Int8Rep = int8Rep

pattern Int16Rep <- (matchesNullary ResolutionNamespaceTerm "Int16Rep" -> True) where Int16Rep = int16Rep

pattern Int32Rep <- (matchesNullary ResolutionNamespaceTerm "Int32Rep" -> True) where Int32Rep = int32Rep

pattern Int64Rep <- (matchesNullary ResolutionNamespaceTerm "Int64Rep" -> True) where Int64Rep = int64Rep

pattern WordRep <- (matchesNullary ResolutionNamespaceTerm "WordRep" -> True) where WordRep = wordRep

pattern Word8Rep <- (matchesNullary ResolutionNamespaceTerm "Word8Rep" -> True) where Word8Rep = word8Rep

pattern Word16Rep <- (matchesNullary ResolutionNamespaceTerm "Word16Rep" -> True) where Word16Rep = word16Rep

pattern Word32Rep <- (matchesNullary ResolutionNamespaceTerm "Word32Rep" -> True) where Word32Rep = word32Rep

pattern Word64Rep <- (matchesNullary ResolutionNamespaceTerm "Word64Rep" -> True) where Word64Rep = word64Rep

pattern AddrRep <- (matchesNullary ResolutionNamespaceTerm "AddrRep" -> True) where AddrRep = addrRep

pattern FloatRep <- (matchesNullary ResolutionNamespaceTerm "FloatRep" -> True)

pattern DoubleRep <- (matchesNullary ResolutionNamespaceTerm "DoubleRep" -> True)

matchUnaryRep :: Text -> TcType -> Maybe TcType
matchUnaryRep expected (TcTyCon tyCon [argument])
  | tyConNamespace tyCon == ResolutionNamespaceTerm,
    tyConName tyCon == expected =
      Just argument
matchUnaryRep _ _ = Nothing

matchBinaryRep :: Text -> TcType -> Maybe (TcType, TcType)
matchBinaryRep expected (TcTyCon tyCon [left, right])
  | tyConNamespace tyCon == ResolutionNamespaceTerm,
    tyConName tyCon == expected =
      Just (left, right)
matchBinaryRep _ _ = Nothing

matchListRep :: Text -> TcType -> Maybe [TcType]
matchListRep expected (TcTyCon tyCon [listType])
  | tyConNamespace tyCon == ResolutionNamespaceTerm,
    tyConName tyCon == expected =
      decodeDataConstructorList listType
matchListRep _ _ = Nothing

decodeDataConstructorList :: TcType -> Maybe [TcType]
decodeDataConstructorList ty =
  case ty of
    TcTyCon tyCon []
      | tyConNamespace tyCon == ResolutionNamespaceTerm,
        tyConName tyCon == "[]" ->
          Just []
    TcTyCon tyCon [field, rest]
      | tyConNamespace tyCon == ResolutionNamespaceTerm,
        tyConName tyCon == ":" ->
          (field :) <$> decodeDataConstructorList rest
    _ -> Nothing

matchesNullary :: ResolutionNamespace -> Text -> TcType -> Bool
matchesNullary namespace expected (TcTyCon tyCon []) =
  tyConNamespace tyCon == namespace && tyConName tyCon == expected
matchesNullary _ _ _ = False

runtimeRepFromKind :: TcType -> Either String TcType
runtimeRepFromKind kind =
  case kind of
    TcTyCon tyCon [representation]
      | tyConName tyCon == "TYPE" -> Right representation
    _ -> Left ("type does not have a runtime representation: " <> show kind)

-- | Test whether a runtime representation has a fixed machine layout.
isFixedRuntimeRep :: TcType -> Bool
isFixedRuntimeRep representation =
  case representation of
    BoxedRep levity -> promotedConstructorIsOneOf ["Lifted", "Unlifted"] levity
    TupleRep fields -> all isFixedRuntimeRep fields
    SumRep fields -> all isFixedRuntimeRep fields
    VecRep count element ->
      promotedConstructorIsOneOf ["Vec2", "Vec4", "Vec8", "Vec16", "Vec32", "Vec64"] count
        && promotedConstructorIsOneOf
          [ "Int8ElemRep",
            "Int16ElemRep",
            "Int32ElemRep",
            "Int64ElemRep",
            "Word8ElemRep",
            "Word16ElemRep",
            "Word32ElemRep",
            "Word64ElemRep",
            "FloatElemRep",
            "DoubleElemRep"
          ]
          element
    IntRep -> True
    Int8Rep -> True
    Int16Rep -> True
    Int32Rep -> True
    Int64Rep -> True
    WordRep -> True
    Word8Rep -> True
    Word16Rep -> True
    Word32Rep -> True
    Word64Rep -> True
    AddrRep -> True
    FloatRep -> True
    DoubleRep -> True
    _ -> False
  where
    promotedConstructorIsOneOf names ty =
      case ty of
        TcTyCon tyCon [] ->
          tyConNamespace tyCon == ResolutionNamespaceTerm
            && tyConModuleName tyCon == "GHC.Types"
            && tyConName tyCon `elem` names
        _ -> False

newtype TcLevel = TcLevel Int
  deriving (Eq, Ord, Show, Read)

topTcLevel :: TcLevel
topTcLevel = TcLevel 0

pushLevel :: TcLevel -> TcLevel
pushLevel (TcLevel level) = TcLevel (level + 1)
