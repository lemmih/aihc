-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Exception (SomeException, try)
import Language.Haskell.TH qualified
import Language.Haskell.TH.CodeDo qualified as CodeDo
import Language.Haskell.TH.LanguageExtensions qualified
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Lib.Internal qualified
import Language.Haskell.TH.Ppr qualified
import Language.Haskell.TH.PprLib qualified
import Language.Haskell.TH.Quote qualified
import Language.Haskell.TH.Syntax

litInt :: (Quote m) => Integer -> m Exp
litInt n = litE (integerL n)

typedLitInt :: (Quote m) => Integer -> Code m Integer
typedLitInt = unsafeCodeCoerce . litInt

newtype TypedOnly = TypedOnly Integer

instance Lift TypedOnly where
  liftTyped (TypedOnly n) = unsafeCodeCoerce (pure (AppE (ConE (mkName "TypedOnly")) (LitE (IntegerL n))))

main :: IO ()
main = do
  lifted <- runQ (lift (TypedOnly 37))
  if lifted == AppE (ConE (mkName "TypedOnly")) (LitE (IntegerL 37))
    then pure ()
    else error "default lift returned an incorrect expression"
  expression <- runQ (litInt 42)
  if expression == LitE (IntegerL 42)
    then pure ()
    else error "integer literal combinators returned an incorrect expression"
  first <- runQ (newName "x")
  second <- runQ (newName "x")
  if first /= second
    then pure ()
    else error "newName returned the same name"
  result <- try (runQ (reify (mkName "x"))) :: IO (Either SomeException Info)
  case result of
    Left _ -> pure ()
    Right _ -> error "reify did not fail outside the compiler"
  let _ = typedLitInt 42 :: Code Q Integer
      _ = Language.Haskell.TH.Cpp
      _ = (CodeDo.>>) :: Q () -> Code Q Integer -> Code Q Integer
      _ = Language.Haskell.TH.LanguageExtensions.Cpp
      _ = Language.Haskell.TH.Lib.Internal.intPrimL
      _ = Language.Haskell.TH.Ppr.pprint :: Exp -> String
      _ = Language.Haskell.TH.PprLib.empty
      _ = Language.Haskell.TH.Quote.quoteFile
  pure ()
