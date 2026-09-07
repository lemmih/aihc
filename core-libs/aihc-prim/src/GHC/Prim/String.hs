{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | The class that an overloaded string literal desugars to.
--
-- With OverloadedStrings a string literal becomes @fromString "..."@. The
-- compiler takes the method from the built-in scope, so the class must live
-- in the primitive package. @Data.String@ of @aihc-base@ exports it again.
module GHC.Prim.String
  ( IsString (..),
  )
where

import GHC.Prim.Base (String)
import GHC.Types (Char)

class IsString a where
  fromString :: String -> a

instance (a ~ Char) => IsString [a] where
  fromString string = string
