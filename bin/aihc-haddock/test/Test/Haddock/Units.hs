{-# LANGUAGE OverloadedStrings #-}

module Test.Haddock.Units (tests) where

import Aihc.Haddock.Compare (normalizeDoc)
import Aihc.Haddock.Markup (parseDocText, parseMetaDocText)
import Aihc.Haddock.Model
import Aihc.Haddock.Reference.Json (StableName (..), parseStableName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "units"
    [ testCase "stable name with operator containing $" $
        parseStableName "$main$Data.Functor$<$>" @?= Just (StableName "main" "Data.Functor" "<$>"),
      testCase "stable name of a versioned package" $
        parseStableName "$colour-2.3.7$Data.Colour.RGB$RGB" @?= Just (StableName "colour-2.3.7" "Data.Colour.RGB" "RGB"),
      testCase "malformed stable name" $
        parseStableName "Data.Colour.RGB" @?= Nothing,
      testCase "paragraphs append right-nested" $
        parseDocText "First.\n\nSecond."
          @?= DocAppend (DocParagraph (DocString "First.")) (DocParagraph (DocString "Second.")),
      testCase "@since is split off" $
        parseMetaDocText "@since 1.2.3" @?= MetaDoc (Just [1, 2, 3]) DocEmpty,
      testCase "inline identifiers and modules" $
        parseDocText "See 'area' in \"Doc.Fixture\"."
          @?= DocParagraph
            ( DocAppend
                (DocString "See ")
                (DocAppend (DocIdentifier "area") (DocAppend (DocString " in ") (DocAppend (DocModule "Doc.Fixture") (DocString "."))))
            ),
      testCase "normalization flattens appends and merges strings" $
        normalizeDoc id (DocAppend (DocAppend (DocString "a") (DocString "b")) (DocAppend DocEmpty (DocString "c")))
          @?= DocString "abc"
    ]
