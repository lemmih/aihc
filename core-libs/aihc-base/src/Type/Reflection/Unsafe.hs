{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Type.Reflection.Unsafe
  ( TyCon,
    mkTyCon,
    mkTrCon,
    mkTrApp,
    tyConKindArgs,
    tyConKindRep,
    KindRep (..),
    KindBndr,
    TypeLitSort (..),
    pattern KindRepTypeLit,
  )
where

import Data.Maybe (Maybe (..))
import GHC.Base (String, unpackCString#)
import GHC.Types (Int, KindBndr, KindRep (..), Module (..), TrName (..), TyCon (..), TypeLitSort (..))
import Type.Reflection.Internal (mkTrApp, mkTrCon)

-- | Build a type constructor. The compiler builds the constructors that
-- 'Type.Reflection.Typeable' evidence carries; this is for a caller that
-- deserialises one.
mkTyCon :: String -> String -> String -> Int -> KindRep -> TyCon
mkTyCon package moduleName name =
  TyCon (Module (TrNameD package) (TrNameD moduleName)) (TrNameD name)

-- | The number of kind arguments that the kind representation of a type
-- constructor abstracts over.
tyConKindArgs :: TyCon -> Int
tyConKindArgs (TyCon _ _ kindArgs _) = kindArgs

tyConKindRep :: TyCon -> KindRep
tyConKindRep (TyCon _ _ _ kindRep) = kindRep

-- | A type literal in a kind, whichever way its text is stored. Building
-- one stores the text as a list, as a caller outside the compiler has no
-- string literal to point at.
pattern KindRepTypeLit :: TypeLitSort -> String -> KindRep
pattern KindRepTypeLit sort text <- (kindRepTypeLit -> Just (sort, text))
  where
    KindRepTypeLit sort text = KindRepTypeLitD sort text

kindRepTypeLit :: KindRep -> Maybe (TypeLitSort, String)
kindRepTypeLit (KindRepTypeLitS sort address) = Just (sort, unpackCString# address)
kindRepTypeLit (KindRepTypeLitD sort text) = Just (sort, text)
kindRepTypeLit _ = Nothing
