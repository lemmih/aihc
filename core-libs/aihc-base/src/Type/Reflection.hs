{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Type.Reflection
  ( Typeable (..),
    TypeRep,
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
import Data.Proxy (Proxy (..))
import GHC.Base (List (..), String, foldr, unpackCString#, (.))
import GHC.Prim (ord#, seq, (==#))
import GHC.Types (Char (..), Module (..), TrName (..), TyCon (..))

data TypeRep a = TypeRep TyCon [SomeTypeRep]

data SomeTypeRep = forall a. SomeTypeRep (TypeRep a)

class Typeable a where
  -- Both projections are compiler supplied until imported class selectors
  -- retain enough metadata for typeOf to be an ordinary wrapper around typeRep.
  typeRep :: Proxy a -> TypeRep a
  typeOf :: a -> TypeRep a

typeRepTyCon :: TypeRep a -> TyCon
typeRepTyCon (TypeRep tyCon _) = tyCon

-- | The type constructor of a type and its arguments.
splitApps :: TypeRep a -> (TyCon, [SomeTypeRep])
splitApps (TypeRep tyCon arguments) = (tyCon, arguments)

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

eqTypeRep :: TypeRep a -> TypeRep b -> Bool
eqTypeRep (TypeRep leftTyCon leftArgs) (TypeRep rightTyCon rightArgs) =
  eqTyCon leftTyCon rightTyCon && sameTypeReps leftArgs rightArgs

eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Bool
eqSomeTypeRep (SomeTypeRep left) (SomeTypeRep right) = eqTypeRep left right

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

rnfTypeRep :: TypeRep a -> ()
rnfTypeRep (TypeRep tyCon arguments) = rnfTyCon tyCon `seq` rnfSomeTypeRepList arguments

rnfSomeTypeRepList :: [SomeTypeRep] -> ()
rnfSomeTypeRepList = foldr (seq . rnfSomeTypeRep) ()

rnfString :: String -> ()
rnfString = foldr seq ()
