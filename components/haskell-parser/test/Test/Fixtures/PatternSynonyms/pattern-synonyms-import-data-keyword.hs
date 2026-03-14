{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportDataKeyword where

import PatternSynonymsSource (data Zero, data Succ)

buildZero = Zero
buildOne = Succ Zero
