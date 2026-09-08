{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core type representation for the type checker.
module Aihc.Tc.Types
  ( Unique (..),
    TyVarId (TyVarId, tvName, tvUnique),
    mkTyVarId,
    tvKind,
    setTyVarKind,
    TcType (..),
    isPolyType,
    TcTypeKey,
    TcAxiomKey (..),
    TcKindEnv,
    TyCon (TyCon, tyConName, tyConArity),
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    TcKinds (..),
    isEqualityTyCon,
    mkAppTy,
    tyConNamespace,
    mkTyConWithOrigin,
    mkTyConWithNamespace,
    TypeScheme (..),
    typeKindInEnv,
    typeKind,
    constraintKind,
    runtimeRepKind,
    mkTYPEKind,
    liftedRep,
    tupleRep,
    sumRep,
    intRep,
    int8Rep,
    int16Rep,
    int32Rep,
    int64Rep,
    wordRep,
    word8Rep,
    word16Rep,
    word32Rep,
    word64Rep,
    addrRep,
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

-- | A type variable is matched by its name and its unique. Its kind is
-- read with 'tvKind' and given with 'mkTyVarId': the module knows no kind
-- vocabulary of its own, so it has no kind to default to.
pattern TyVarId :: Text -> Unique -> TyVarId
pattern TyVarId {tvName, tvUnique} <- TyVarIdInternal tvName tvUnique _

{-# COMPLETE TyVarId #-}

-- | A type variable of one name, unique and kind.
mkTyVarId :: Text -> Unique -> TcType -> TyVarId
mkTyVarId = TyVarIdInternal

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

-- | Apply a type to an argument.
--
-- An application of a type constructor stays a constructor application,
-- and a saturated arrow becomes the function type. The arrow is a form of
-- its own rather than a type constructor, so recognising it here is a
-- pattern match: this module needs to know no library to normalise.
mkAppTy :: TcType -> TcType -> TcType
mkAppTy function argument =
  case function of
    TcTyCon tyCon arguments -> TcTyCon tyCon (arguments <> [argument])
    TcAppTy TcArrowTy domain -> TcFunTy domain argument
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
  | -- | The function arrow @(->)@ itself, unapplied. It is a form of its
    -- own rather than a type constructor so that the module can recognise
    -- an arrow without being told which type constructor the arrow is: a
    -- saturated one is 'TcFunTy' and a partial one is @'TcAppTy'
    -- 'TcArrowTy' domain@, and nothing else may spell either.
    TcArrowTy
  | TcFunTy !TcType !TcType
  | TcForAllTy !TyVarId !TcType
  | TcQualTy ![Pred] !TcType
  | TcAppTy !TcType !TcType
  deriving (Eq, Ord, Show, Read)

data TypeScheme = ForAll ![TyVarId] ![Pred] !TcType
  deriving (Eq, Ord, Show, Read)

-- | Whether a type is a polytype: a leading quantifier or context. A
-- meta-variable never stands for a polytype, so an argument of such a
-- type is checked against it rather than inferred. A quantifier nested
-- under an arrow or a constructor does not make a type a polytype.
isPolyType :: TcType -> Bool
isPolyType TcForAllTy {} = True
isPolyType TcQualTy {} = True
isPolyType _ = False

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
constraintTypeToPred :: TcKinds -> TcType -> Maybe Pred
constraintTypeToPred kinds ty =
  case collectForAllTypes ty of
    (variables@(_ : _), qualified) -> do
      let (antecedents, consequentType) =
            case qualified of
              TcQualTy predicates body -> (predicates, body)
              body -> ([], body)
      consequent <- atomicConstraintTypeToPred kinds consequentType
      pure (QuantifiedPred variables antecedents consequent)
    ([], body) -> atomicConstraintTypeToPred kinds body

atomicConstraintTypeToPred :: TcKinds -> TcType -> Maybe Pred
atomicConstraintTypeToPred kinds ty =
  case collectTypeApplications ty of
    (TcTyCon tyCon headArgs, arguments)
      | isEqualityTyCon kinds tyCon,
        [left, right] <- headArgs <> arguments ->
          Just (EqPred left right)
    (TcTyCon tyCon [payload], [])
      | isImplicitParamTyConName (tyConName tyCon) -> Just (IParamPred (tyConName tyCon) payload)
    (TcTyCon tyCon headArgs, arguments) ->
      Just (ClassPred tyCon (headArgs <> arguments))
    _ -> Nothing

-- | Whether a type constructor is the nominal equality constraint @~@.
--
-- A source module may declare a class of that name -- @class a ~ b@ is
-- accepted with TypeOperators -- and such a class imposes no equality, so
-- this compares identities rather than names.
isEqualityTyCon :: TcKinds -> TyCon -> Bool
isEqualityTyCon kinds tyCon =
  tyCon == kindsEqualityTyCon kinds
    || tyCon == mkTyConWithOrigin (tyConPackageId equality) (tyConModuleName equality) "~~" 2
  where
    equality = kindsEqualityTyCon kinds

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

-- | The kind vocabulary, resolved to the module that declares it.
--
-- The type checker knows the names GHC gives these constructors --
-- @TYPE@, @Constraint@, @BoxedRep@, @IntRep@ and the rest -- but not the
-- package or module that declares them, and it must not guess: a kind it
-- built and a kind it resolved from an interface have to be the same
-- type. 'Aihc.Tc.Wiring.mkTcKinds' builds this table from the compiler's
-- wiring, and every kind below is built from it.
data TcKinds = TcKinds
  { -- | A kind constructor of one name and arity, such as @TYPE@.
    kindsTyCon :: Text -> Int -> TyCon,
    -- | A promoted constructor of the kind vocabulary of one name and
    -- arity, such as @BoxedRep@, @Lifted@ or @IntRep@.
    kindsDataCon :: Text -> Int -> TyCon,
    -- | The nominal equality constraint @~@.
    kindsEqualityTyCon :: TyCon,
    -- | The type constructor that 'TcArrowTy' denotes. Nothing inside the
    -- type checker needs it -- an arrow is recognised by its form -- but a
    -- partially applied arrow that leaves for the desugarer has to be
    -- named there like any other type constructor.
    kindsArrowTyCon :: TyCon
  }

-- | The tables are functions, so a table shows as its name alone, as
-- 'Aihc.Tc.Wiring.TcWiring' does.
instance Show TcKinds where
  show _ = "TcKinds"

-- | The kind of ordinary lifted types, @TYPE (BoxedRep Lifted)@.
typeKind :: TcKinds -> TcType
typeKind kinds = mkTYPEKind kinds (liftedRep kinds)

-- | The kind of constraints.
constraintKind :: TcKinds -> TcType
constraintKind kinds = TcTyCon (kindsTyCon kinds "Constraint" 0) []

-- | The kind of runtime representations.
runtimeRepKind :: TcKinds -> TcType
runtimeRepKind kinds = TcTyCon (kindsTyCon kinds "RuntimeRep" 0) []

-- | The kind of types of one runtime representation.
mkTYPEKind :: TcKinds -> TcType -> TcType
mkTYPEKind kinds representation =
  TcTyCon (kindsTyCon kinds "TYPE" 1) [representation]

nullaryRep :: TcKinds -> Text -> TcType
nullaryRep kinds name = TcTyCon (kindsDataCon kinds name 0) []

-- | The lifted runtime representation, @BoxedRep Lifted@.
liftedRep :: TcKinds -> TcType
liftedRep kinds = boxedRep kinds (nullaryRep kinds "Lifted")

intRep, int8Rep, int16Rep, int32Rep, int64Rep :: TcKinds -> TcType
wordRep, word8Rep, word16Rep, word32Rep, word64Rep, addrRep :: TcKinds -> TcType
intRep kinds = nullaryRep kinds "IntRep"
int8Rep kinds = nullaryRep kinds "Int8Rep"
int16Rep kinds = nullaryRep kinds "Int16Rep"
int32Rep kinds = nullaryRep kinds "Int32Rep"
int64Rep kinds = nullaryRep kinds "Int64Rep"

wordRep kinds = nullaryRep kinds "WordRep"

word8Rep kinds = nullaryRep kinds "Word8Rep"

word16Rep kinds = nullaryRep kinds "Word16Rep"

word32Rep kinds = nullaryRep kinds "Word32Rep"

word64Rep kinds = nullaryRep kinds "Word64Rep"

addrRep kinds = nullaryRep kinds "AddrRep"

boxedRep :: TcKinds -> TcType -> TcType
boxedRep kinds levity = TcTyCon (kindsDataCon kinds "BoxedRep" 1) [levity]

-- | The runtime representation of an unboxed tuple of these fields.
tupleRep :: TcKinds -> [TcType] -> TcType
tupleRep kinds fields =
  TcTyCon (kindsDataCon kinds "TupleRep" 1) [dataConstructorList kinds fields]

-- | The runtime representation of an unboxed sum of these fields.
sumRep :: TcKinds -> [TcType] -> TcType
sumRep kinds fields =
  TcTyCon (kindsDataCon kinds "SumRep" 1) [dataConstructorList kinds fields]

dataConstructorList :: TcKinds -> [TcType] -> TcType
dataConstructorList kinds = foldr cons nil
  where
    nil = TcTyCon (kindsDataCon kinds "[]" 0) []
    cons field rest = TcTyCon (kindsDataCon kinds ":" 2) [field, rest]

-- | Get a type kind from the complete type-constructor identity table.
typeKindInEnv :: TcKinds -> TcKindEnv -> TcType -> Either String TcType
typeKindInEnv kinds kindEnv = go
  where
    go rawType =
      case rawType of
        TcTyVar tyVar -> Right (tvKind tyVar)
        TcMetaTv {} -> Left "type still has a meta variable"
        TcTyCon tyCon arguments -> do
          scheme <-
            maybe
              (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
              Right
              (Map.lookup (tyConKey tyCon) kindEnv)
          applyArguments scheme arguments
        TcArrowTy -> Right (KFun (typeKind kinds) (KFun (typeKind kinds) (typeKind kinds)))
        TcFunTy {} -> Right (typeKind kinds)
        TcForAllTy _ body -> go body
        TcQualTy _ body -> go body
        TcAppTy function argument -> do
          functionKind <- go function
          applyKind functionKind argument

    applyArguments (ForAll quantified _ body) = applyMany (map tvUnique quantified) body

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
          | formal == actual -> Right Map.empty
          | otherwise -> Left ("kind mismatch: expected " <> show formal <> ", got " <> show actual)

runtimeRepOfTypeInEnv :: TcKinds -> TcKindEnv -> TcType -> Either String TcType
runtimeRepOfTypeInEnv kinds kindEnv ty = typeKindInEnv kinds kindEnv ty >>= runtimeRepFromKind

isUnliftedTypeInEnv :: TcKinds -> TcKindEnv -> TcType -> Bool
isUnliftedTypeInEnv kinds kindEnv ty =
  case runtimeRepOfTypeInEnv kinds kindEnv ty of
    Right representation -> not (matchesLiftedRuntimeRep representation)
    Left _ -> False

-- | Apply a type-variable substitution to a type.
applySubst :: Map Unique TcType -> TcType -> TcType
applySubst substitution = go
  where
    go ty =
      case ty of
        TcTyVar tyVar -> Map.findWithDefault (TcTyVar (setTyVarKind (go (tvKind tyVar)) tyVar)) (tvUnique tyVar) substitution
        TcMetaTv {} -> ty
        TcArrowTy -> ty
        TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments)
        TcFunTy argument result -> TcFunTy (go argument) (go result)
        TcForAllTy tyVar body ->
          TcForAllTy (setTyVarKind (go (tvKind tyVar)) tyVar) (applySubst (Map.delete (tvUnique tyVar) substitution) body)
        TcQualTy predicates body -> TcQualTy (map (applySubstPred substitution) predicates) (go body)
        TcAppTy function argument -> mkAppTy (go function) (go argument)

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
            [setTyVarKind (applySubst scopedSubstitution (tvKind variable)) variable | variable <- variables]
            (map (applySubstPred scopedSubstitution) antecedents)
            (applySubstPred scopedSubstitution consequent)

-- The kind patterns recognise a kind by its namespace and its name, so
-- that a kind built here and a kind resolved from an interface match each
-- other. They only match: a kind is built from a 'TcKinds'.
pattern KTYPE :: TcType -> TcType
pattern KTYPE representation <- (matchTYPEKind -> Just representation)

pattern KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KType :: TcType
pattern KConstraint <- (matchesNullary ResolutionNamespaceType "Constraint" -> True)
pattern KRuntimeRep <- (matchesNullary ResolutionNamespaceType "RuntimeRep" -> True)
pattern KLevity <- (matchesNullary ResolutionNamespaceType "Levity" -> True)
pattern KVecCount <- (matchesNullary ResolutionNamespaceType "VecCount" -> True)
pattern KVecElem <- (matchesNullary ResolutionNamespaceType "VecElem" -> True)
pattern KType <- (matchesLiftedTypeKind -> True)

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

pattern SumRep :: [TcType] -> TcType
pattern SumRep fields <- (matchListRep "SumRep" -> Just fields)

pattern VecRep :: TcType -> TcType -> TcType
pattern VecRep count element <- (matchBinaryRep "VecRep" -> Just (count, element))

pattern Lifted, Unlifted :: TcType
pattern Lifted <- (matchesNullary ResolutionNamespaceTerm "Lifted" -> True)
pattern Unlifted <- (matchesNullary ResolutionNamespaceTerm "Unlifted" -> True)

pattern IntRep, Int8Rep, Int16Rep, Int32Rep, Int64Rep :: TcType

pattern WordRep, Word8Rep, Word16Rep, Word32Rep, Word64Rep :: TcType

pattern AddrRep, FloatRep, DoubleRep :: TcType

pattern IntRep <- (matchesNullary ResolutionNamespaceTerm "IntRep" -> True)

pattern Int8Rep <- (matchesNullary ResolutionNamespaceTerm "Int8Rep" -> True)

pattern Int16Rep <- (matchesNullary ResolutionNamespaceTerm "Int16Rep" -> True)

pattern Int32Rep <- (matchesNullary ResolutionNamespaceTerm "Int32Rep" -> True)

pattern Int64Rep <- (matchesNullary ResolutionNamespaceTerm "Int64Rep" -> True)

pattern WordRep <- (matchesNullary ResolutionNamespaceTerm "WordRep" -> True)

pattern Word8Rep <- (matchesNullary ResolutionNamespaceTerm "Word8Rep" -> True)

pattern Word16Rep <- (matchesNullary ResolutionNamespaceTerm "Word16Rep" -> True)

pattern Word32Rep <- (matchesNullary ResolutionNamespaceTerm "Word32Rep" -> True)

pattern Word64Rep <- (matchesNullary ResolutionNamespaceTerm "Word64Rep" -> True)

pattern AddrRep <- (matchesNullary ResolutionNamespaceTerm "AddrRep" -> True)

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
    -- The representation matchers this function is built from compare a
    -- namespace and a name. Recognise a promoted constructor the same way,
    -- so that the whole function reads one identity the same way.
    promotedConstructorIsOneOf names ty =
      case ty of
        TcTyCon tyCon [] ->
          tyConNamespace tyCon == ResolutionNamespaceTerm
            && tyConName tyCon `elem` names
        _ -> False

newtype TcLevel = TcLevel Int
  deriving (Eq, Ord, Show, Read)

topTcLevel :: TcLevel
topTcLevel = TcLevel 0

pushLevel :: TcLevel -> TcLevel
pushLevel (TcLevel level) = TcLevel (level + 1)
