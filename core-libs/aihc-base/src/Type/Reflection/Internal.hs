{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Type.Reflection.Internal
  ( Typeable (..),
    TypeRep,
    (:~~:) (..),
    pattern Con,
    pattern Con',
    pattern App,
    pattern Fun,
    typeRepKind,
    someTypeRep,
    mkTrCon,
    mkTrApp,
    SomeTypeRep (..),
    TyCon (..),
    Module (..),
    eqTypeRep,
    typeOf,
    splitApps,
    typeRepTyCon,
    tyConPackage,
    tyConModule,
    tyConName,
    modulePackage,
    moduleName,
    rnfTypeRep,
    rnfSomeTypeRep,
    rnfTyCon,
    rnfModule,
  )
where

import Data.Bool (Bool (..), (&&))
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~~:) (..))
import GHC.Base (List (..), String, foldr, unpackCString#, (.))
import GHC.Classes qualified
import GHC.Internal.Classes (Eq (..), Ord (..), Ordering (..))
import GHC.Num qualified
import GHC.Prim (ord#, seq, (==#))
import GHC.Show (Show (..), ShowS, showChar, showParen, showString)
import GHC.Types (Char (..), KindRep (..), Levity (..), Module (..), RuntimeRep (..), TYPE, TrName (..), TyCon (..), Type, VecCount (..), VecElem (..), type (~~))
import GHC.Types qualified
import Unsafe.Coerce (unsafeCoerce)

type role TypeRep nominal

data TypeRep (a :: k) = TypeRep TyCon [SomeTypeRep] [SomeTypeRep]

data SomeTypeRep where
  SomeTypeRep :: forall k (a :: k). TypeRep a -> SomeTypeRep

class Typeable (a :: k) where
  typeRep :: TypeRep a

typeOf :: (Typeable a) => a -> TypeRep a
typeOf _ = typeRep

someTypeRep :: forall k proxy (a :: k). (Typeable a) => proxy a -> SomeTypeRep
someTypeRep _ = SomeTypeRep (typeRep :: TypeRep a)

typeRepTyCon :: forall k (a :: k). TypeRep a -> TyCon
typeRepTyCon (TypeRep tyCon _ _) = tyCon

-- | The type constructor of a type and its arguments.
splitApps :: forall k (a :: k). TypeRep a -> (TyCon, [SomeTypeRep])
splitApps (TypeRep tyCon _ arguments) = (tyCon, arguments)

trNameString :: TrName -> String
trNameString (TrNameS address) = unpackCString# address
trNameString (TrNameD name) = name

modulePackage :: Module -> String
modulePackage (Module package _) = trNameString package

moduleName :: Module -> String
moduleName (Module _ name) = trNameString name

tyConPackage :: TyCon -> String
tyConPackage (TyCon modul _ _ _) = modulePackage modul

tyConModule :: TyCon -> String
tyConModule (TyCon modul _ _ _) = moduleName modul

tyConName :: TyCon -> String
tyConName (TyCon _ name _ _) = trNameString name

eqTypeRep :: forall k1 k2 (a :: k1) (b :: k2). TypeRep a -> TypeRep b -> Maybe (a :~~: b)
eqTypeRep left right =
  if eqSomeTypeRep (SomeTypeRep left) (SomeTypeRep right)
    then Just (unsafeCoerce (HRefl :: () :~~: ()))
    else Nothing

eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Bool
eqSomeTypeRep (SomeTypeRep (TypeRep leftTyCon leftKinds leftArgs)) (SomeTypeRep (TypeRep rightTyCon rightKinds rightArgs)) =
  eqTyCon leftTyCon rightTyCon && sameTypeReps leftKinds rightKinds && sameTypeReps leftArgs rightArgs

-- | Type constructors are compared by their qualified name. GHC compares
-- fingerprints, which aihc does not build; the name, the module and the
-- package identify a constructor just as well.
eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon left right =
  sameString (tyConPackage left) (tyConPackage right)
    && sameString (tyConModule left) (tyConModule right)
    && sameString (tyConName left) (tyConName right)

sameTypeReps :: [SomeTypeRep] -> [SomeTypeRep] -> Bool
sameTypeReps [] [] = True
sameTypeReps [] (_ : _) = False
sameTypeReps (_ : _) [] = False
sameTypeReps (left : lefts) (right : rights) =
  eqSomeTypeRep left right && sameTypeReps lefts rights

sameString :: String -> String -> Bool
sameString [] [] = True
sameString [] (_ : _) = False
sameString (_ : _) [] = False
sameString (left : lefts) (right : rights) = sameChar left right && sameString lefts rights

sameChar :: Char -> Char -> Bool
sameChar (C# left) (C# right) =
  case (==#) (ord# left) (ord# right) of
    0# -> False
    _ -> True

rnfTyCon :: TyCon -> ()
rnfTyCon (TyCon modul name _ _) = rnfModule modul `seq` rnfTrName name

rnfModule :: Module -> ()
rnfModule (Module package name) = rnfTrName package `seq` rnfTrName name

rnfTrName :: TrName -> ()
rnfTrName name = rnfString (trNameString name)

rnfSomeTypeRep :: SomeTypeRep -> ()
rnfSomeTypeRep (SomeTypeRep representation) = rnfTypeRep representation

rnfTypeRep :: forall k (a :: k). TypeRep a -> ()
rnfTypeRep (TypeRep tyCon kinds arguments) = rnfTyCon tyCon `seq` rnfSomeTypeRepList kinds `seq` rnfSomeTypeRepList arguments

rnfSomeTypeRepList :: [SomeTypeRep] -> ()
rnfSomeTypeRepList = foldr (seq . rnfSomeTypeRep) ()

rnfString :: String -> ()
rnfString = foldr seq ()

mkTrCon :: forall k (a :: k). TyCon -> [SomeTypeRep] -> TypeRep a
mkTrCon con kinds = TypeRep con kinds []

mkTrApp :: forall k1 k2 (f :: k1 -> k2) (x :: k1). TypeRep f -> TypeRep x -> TypeRep (f x)
mkTrApp (TypeRep con kinds args) x = TypeRep con kinds (append args (SomeTypeRep x))

append :: [a] -> a -> [a]
append values x = foldr (:) [x] values

data AppView (a :: k) where
  IsCon :: forall k (a :: k). TyCon -> [SomeTypeRep] -> AppView a
  IsApp :: forall k2 (t :: k2) k1 (f :: k1 -> k2) (x :: k1). (t ~ f x) => TypeRep f -> TypeRep x -> AppView t

data Application (a :: Type)

splitApp :: forall k (a :: k). TypeRep a -> AppView a
splitApp (TypeRep con kinds []) = IsCon con kinds
splitApp (TypeRep con kinds args) = splitLast [] args
  where
    splitLast prefix [SomeTypeRep x] =
      unsafeCoerce (IsApp (TypeRep con kinds prefix :: TypeRep Application) (unsafeCoerce x :: TypeRep ()))
    splitLast prefix (x : xs) = splitLast (append prefix x) xs

pattern Con :: forall k (a :: k). TyCon -> TypeRep a
pattern Con con <- (splitApp -> IsCon con _)

pattern Con' :: forall k (a :: k). TyCon -> [SomeTypeRep] -> TypeRep a
pattern Con' con kinds <- (splitApp -> IsCon con kinds)

pattern App :: forall k2 (t :: k2). () => forall k1 (f :: k1 -> k2) (x :: k1). (t ~ f x) => TypeRep f -> TypeRep x -> TypeRep t
pattern App f x <- (splitApp -> IsApp f x)
  where
    App f x = mkTrApp f x

data FunView (a :: k) where
  IsFun :: forall k (t :: k) (a :: Type) (b :: Type). (k ~ Type, t ~~ (a -> b)) => TypeRep a -> TypeRep b -> FunView t
  NotFun :: forall k (a :: k). FunView a

splitFun :: forall k (a :: k). TypeRep a -> FunView a
splitFun (TypeRep con _ [SomeTypeRep a, SomeTypeRep b]) =
  if sameString (tyConModule con) "GHC.Types" && sameString (tyConName con) "(->)"
    then unsafeCoerce (IsFun (unsafeCoerce a :: TypeRep ()) (unsafeCoerce b :: TypeRep ()))
    else NotFun
splitFun _ = NotFun

pattern Fun :: forall k (t :: k). () => forall (a :: Type) (b :: Type). (k ~ Type, t ~~ (a -> b)) => TypeRep a -> TypeRep b -> TypeRep t
pattern Fun a b <- (splitFun -> IsFun a b)

typeRepKind :: forall k (a :: k). TypeRep a -> TypeRep k
typeRepKind (TypeRep (TyCon _ _ _ kind) kinds args) =
  case applyKinds (kindRep kinds kind) args of
    SomeTypeRep rep -> unsafeCoerce rep

applyKinds :: SomeTypeRep -> [SomeTypeRep] -> SomeTypeRep
applyKinds rep [] = rep
applyKinds (SomeTypeRep (TypeRep _ _ [_, result])) (_ : args) = applyKinds result args

kindRep :: [SomeTypeRep] -> KindRep -> SomeTypeRep
kindRep kinds (KindRepVar index) = indexKind kinds index
kindRep kinds (KindRepFun a b) =
  case (kindRep kinds a, kindRep kinds b) of
    (SomeTypeRep left, SomeTypeRep right) ->
      SomeTypeRep (TypeRep arrowCon [] [SomeTypeRep left, SomeTypeRep right] :: TypeRep ())
kindRep kinds (KindRepTyConApp con@(TyCon _ _ arity _) args) =
  case splitKindArguments arity (mapKinds kinds args) of
    (kindArguments, typeArguments) -> SomeTypeRep (TypeRep con kindArguments typeArguments :: TypeRep ())
kindRep kinds (KindRepApp f x) =
  case (kindRep kinds f, kindRep kinds x) of
    (SomeTypeRep (TypeRep con ks args), arg) -> SomeTypeRep (TypeRep con ks (append args arg) :: TypeRep ())
kindRep _ (KindRepTYPE representation) = representationNode (typeRep :: TypeRep TYPE) [runtimeRepresentation representation]

mapKinds :: [SomeTypeRep] -> [KindRep] -> [SomeTypeRep]
mapKinds _ [] = []
mapKinds kinds (x : xs) = kindRep kinds x : mapKinds kinds xs

indexKind :: [SomeTypeRep] -> GHC.Types.Int -> SomeTypeRep
indexKind (x : xs) index = if index GHC.Classes.== 0 then x else indexKind xs (index GHC.Num.- 1)

arrowCon :: TyCon
arrowCon = TyCon (Module (TrNameD "aihc-prim") (TrNameD "GHC.Types")) (TrNameD "(->)") 0 (KindRepFun lifted (KindRepFun lifted lifted))
  where
    lifted = KindRepTYPE (BoxedRep Lifted)

instance forall k (a :: k). Eq (TypeRep a) where
  left == right = eqSomeTypeRep (SomeTypeRep left) (SomeTypeRep right)

instance Eq SomeTypeRep where
  (==) = eqSomeTypeRep

instance Eq TyCon where
  (==) = eqTyCon

instance Ord TyCon where
  compare left right =
    case compareList (tyConPackage left) (tyConPackage right) of
      EQ -> case compareList (tyConModule left) (tyConModule right) of
        EQ -> compareList (tyConName left) (tyConName right)
        order -> order
      order -> order

instance Ord SomeTypeRep where
  compare (SomeTypeRep (TypeRep leftCon leftKinds leftArgs)) (SomeTypeRep (TypeRep rightCon rightKinds rightArgs)) =
    case compare leftCon rightCon of
      EQ -> case compareList leftKinds rightKinds of
        EQ -> compareList leftArgs rightArgs
        order -> order
      order -> order

instance forall k (a :: k). Ord (TypeRep a) where
  compare left right = compare (SomeTypeRep left) (SomeTypeRep right)

instance Show TyCon where
  showsPrec _ con = showString (tyConName con)

instance Show SomeTypeRep where
  showsPrec precedence (SomeTypeRep rep) = showsPrec precedence rep

instance forall k (a :: k). Show (TypeRep a) where
  showsPrec precedence (TypeRep con _ args) =
    case args of
      [] -> showString (tyConName con)
      _ -> showParen (precedence > 9) (showString (tyConName con) . showArguments args)

showArguments :: [SomeTypeRep] -> ShowS
showArguments [] = showString ""
showArguments (x : xs) = showChar ' ' . showsPrec 10 x . showArguments xs

compareList :: (Ord a) => [a] -> [a] -> Ordering
compareList [] [] = EQ
compareList [] (_ : _) = LT
compareList (_ : _) [] = GT
compareList (x : xs) (y : ys) =
  case compare x y of
    EQ -> compareList xs ys
    order -> order

splitKindArguments :: GHC.Types.Int -> [a] -> ([a], [a])
splitKindArguments count values =
  if count == 0
    then ([], values)
    else case values of
      [] -> ([], [])
      x : xs -> case splitKindArguments (count GHC.Num.- 1) xs of
        (prefix, suffix) -> (x : prefix, suffix)

representationNode :: forall k (a :: k). TypeRep a -> [SomeTypeRep] -> SomeTypeRep
representationNode (TypeRep con kinds _) arguments = SomeTypeRep (TypeRep con kinds arguments :: TypeRep ())

runtimeRepresentation :: RuntimeRep -> SomeTypeRep
runtimeRepresentation AddrRep = SomeTypeRep (typeRep :: TypeRep 'AddrRep)
runtimeRepresentation DoubleRep = SomeTypeRep (typeRep :: TypeRep 'DoubleRep)
runtimeRepresentation FloatRep = SomeTypeRep (typeRep :: TypeRep 'FloatRep)
runtimeRepresentation Int16Rep = SomeTypeRep (typeRep :: TypeRep 'Int16Rep)
runtimeRepresentation Int32Rep = SomeTypeRep (typeRep :: TypeRep 'Int32Rep)
runtimeRepresentation Int64Rep = SomeTypeRep (typeRep :: TypeRep 'Int64Rep)
runtimeRepresentation Int8Rep = SomeTypeRep (typeRep :: TypeRep 'Int8Rep)
runtimeRepresentation IntRep = SomeTypeRep (typeRep :: TypeRep 'IntRep)
runtimeRepresentation Word16Rep = SomeTypeRep (typeRep :: TypeRep 'Word16Rep)
runtimeRepresentation Word32Rep = SomeTypeRep (typeRep :: TypeRep 'Word32Rep)
runtimeRepresentation Word64Rep = SomeTypeRep (typeRep :: TypeRep 'Word64Rep)
runtimeRepresentation Word8Rep = SomeTypeRep (typeRep :: TypeRep 'Word8Rep)
runtimeRepresentation WordRep = SomeTypeRep (typeRep :: TypeRep 'WordRep)
runtimeRepresentation (BoxedRep Lifted) = SomeTypeRep (typeRep :: TypeRep ('BoxedRep 'Lifted))
runtimeRepresentation (BoxedRep Unlifted) = SomeTypeRep (typeRep :: TypeRep ('BoxedRep 'Unlifted))
runtimeRepresentation (TupleRep fields) = representationNode (typeRep :: TypeRep 'TupleRep) [runtimeRepresentationList fields]
runtimeRepresentation (SumRep fields) = representationNode (typeRep :: TypeRep 'SumRep) [runtimeRepresentationList fields]
runtimeRepresentation (VecRep count element) = representationNode (typeRep :: TypeRep 'VecRep) [vectorCount count, vectorElement element]

vectorCount :: VecCount -> SomeTypeRep
vectorCount Vec16 = SomeTypeRep (typeRep :: TypeRep 'Vec16)
vectorCount Vec2 = SomeTypeRep (typeRep :: TypeRep 'Vec2)
vectorCount Vec32 = SomeTypeRep (typeRep :: TypeRep 'Vec32)
vectorCount Vec4 = SomeTypeRep (typeRep :: TypeRep 'Vec4)
vectorCount Vec64 = SomeTypeRep (typeRep :: TypeRep 'Vec64)
vectorCount Vec8 = SomeTypeRep (typeRep :: TypeRep 'Vec8)

vectorElement :: VecElem -> SomeTypeRep
vectorElement DoubleElemRep = SomeTypeRep (typeRep :: TypeRep 'DoubleElemRep)
vectorElement FloatElemRep = SomeTypeRep (typeRep :: TypeRep 'FloatElemRep)
vectorElement Int16ElemRep = SomeTypeRep (typeRep :: TypeRep 'Int16ElemRep)
vectorElement Int32ElemRep = SomeTypeRep (typeRep :: TypeRep 'Int32ElemRep)
vectorElement Int64ElemRep = SomeTypeRep (typeRep :: TypeRep 'Int64ElemRep)
vectorElement Int8ElemRep = SomeTypeRep (typeRep :: TypeRep 'Int8ElemRep)
vectorElement Word16ElemRep = SomeTypeRep (typeRep :: TypeRep 'Word16ElemRep)
vectorElement Word32ElemRep = SomeTypeRep (typeRep :: TypeRep 'Word32ElemRep)
vectorElement Word64ElemRep = SomeTypeRep (typeRep :: TypeRep 'Word64ElemRep)
vectorElement Word8ElemRep = SomeTypeRep (typeRep :: TypeRep 'Word8ElemRep)

runtimeRepresentationList :: [RuntimeRep] -> SomeTypeRep
runtimeRepresentationList values =
  let list = typeRepTyCon (typeRep :: TypeRep [RuntimeRep])
      element = SomeTypeRep (typeRep :: TypeRep RuntimeRep)
      listKind = KindRepTyConApp list [KindRepVar 0]
      constructor name = TyCon (Module (TrNameD (tyConPackage list)) (TrNameD (tyConModule list))) (TrNameD name) 1
   in case values of
        [] -> SomeTypeRep (TypeRep (constructor "[]" listKind) [element] [] :: TypeRep ())
        x : xs -> SomeTypeRep (TypeRep (constructor ":" (KindRepFun (KindRepVar 0) (KindRepFun listKind listKind))) [element] [runtimeRepresentation x, runtimeRepresentationList xs] :: TypeRep ())
