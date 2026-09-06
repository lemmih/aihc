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
    tyConName,
    rnfTypeRep,
    rnfSomeTypeRep,
    rnfTyCon,
    rnfModule,
  )
where

import Data.Bool (Bool (..), (&&))
import Data.Proxy (Proxy (..))
import GHC.Base (List (..), String, foldr, (.))
import GHC.Prim (ord#, seq, (==#))
import GHC.Types (Char (..))

newtype TyCon = TyCon String

data Module = Module String String

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

tyConName :: TyCon -> String
tyConName (TyCon name) = name

eqTypeRep :: TypeRep a -> TypeRep b -> Bool
eqTypeRep (TypeRep leftTyCon leftArgs) (TypeRep rightTyCon rightArgs) =
  eqTyCon leftTyCon rightTyCon && sameTypeReps leftArgs rightArgs

eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Bool
eqSomeTypeRep (SomeTypeRep left) (SomeTypeRep right) = eqTypeRep left right

eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon (TyCon leftName) (TyCon rightName) = sameString leftName rightName

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
rnfTyCon (TyCon name) = rnfString name

rnfModule :: Module -> ()
rnfModule (Module package name) = rnfString package `seq` rnfString name

rnfSomeTypeRep :: SomeTypeRep -> ()
rnfSomeTypeRep (SomeTypeRep representation) = rnfTypeRep representation

rnfTypeRep :: TypeRep a -> ()
rnfTypeRep (TypeRep tyCon arguments) = rnfTyCon tyCon `seq` rnfSomeTypeRepList arguments

rnfSomeTypeRepList :: [SomeTypeRep] -> ()
rnfSomeTypeRepList = foldr (seq . rnfSomeTypeRep) ()

rnfString :: String -> ()
rnfString = foldr seq ()
