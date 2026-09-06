{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Compiler
  ( tests,
  )
where

import Aihc.Native (NativeTarget (Llvm), backendCompiler, renderLinkedFunctionSymbol)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isDigit, isHexDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Hedgehog (Gen, Property, annotate, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "backend compiler"
    [ testCase "optimizes LLVM IR before object emission" $ do
        (compiler, arguments) <- backendCompiler Llvm
        assertEqual "LLVM compiler" "clang" compiler
        assertBool "LLVM optimization flag" ("-O2" `elem` arguments)
        assertEqual "module-warning flag count" 1 (length (filter (== "-Wno-override-module") arguments)),
      testCase "renders common linker identities readably" $ do
        assertEqual
          "library symbol"
          "aihc_base_4_21_2_0_dephash_Data_Foldable_toList"
          (renderLinkedFunctionSymbol (T.intercalate "\0" ["aihc", "base", "4", "21", "2", "0", "dephash", "Data", "Foldable", "toList"]))
        assertEqual "executable symbol" "exe_Main_main" (renderLinkedFunctionSymbol (T.intercalate "\0" ["exe", "Main", "main"])),
      testCase "escapes unsafe symbol bytes without collisions" $ do
        assertEqual "underscore escape" "aihc_entry_foo__ubar" (renderLinkedFunctionSymbol "foo_bar")
        assertEqual "coded escape" "aihc_entry_foo__dbar" (renderLinkedFunctionSymbol "foo.bar")
        assertEqual "low byte escape" "aihc_entry_a__x09b" (renderLinkedFunctionSymbol "a\tb")
        assertEqual
          "one escape per utf-8 byte"
          "aihc_entry_caf__xc3__xa9"
          (renderLinkedFunctionSymbol "caf\233")
        assertBool
          "component boundaries cannot imitate escapes"
          (renderLinkedFunctionSymbol (T.intercalate "\0" ["foo", "x2e", "bar"]) /= renderLinkedFunctionSymbol "foo.bar"),
      testCase "counts repeated escapes instead of repeating them" $ do
        -- A wide tuple is the reason the count exists: without it the symbol
        -- grows with the arity, and GHC.Tuple grows with its cube.
        assertEqual
          "tuple constructor"
          "aihc__mprim_GHC__dTuple___p__3c__q"
          (renderLinkedFunctionSymbol (T.intercalate "\0" ["aihc-prim", "GHC.Tuple", "(,,,)"]))
        assertEqual "single occurrence stays implicit" "aihc_entry___c" (renderLinkedFunctionSymbol ",")
        assertEqual "hex escapes count too" "aihc_entry___3x09" (renderLinkedFunctionSymbol "\t\t\t")
        assertBool
          "a count cannot swallow the digits after it"
          (renderLinkedFunctionSymbol ",,5" /= renderLinkedFunctionSymbol ",25"),
      testProperty "rendered symbols read back unchanged" prop_symbolRoundTrip
    ]

-- | Every logical name has exactly one rendering, and this reads it back. The
-- property is what makes the rendering safe as a linker identity: two names
-- that differ anywhere cannot land on the same symbol.
prop_symbolRoundTrip :: Property
prop_symbolRoundTrip = property $ do
  components <- forAll (Gen.list (Range.linear 2 4) genComponent)
  let logicalName = T.intercalate "\0" components
      rendered = renderLinkedFunctionSymbol logicalName
  annotate (T.unpack rendered)
  case decodeLinkedSymbol rendered of
    Left problem -> annotate problem >> failure
    Right decoded -> decoded === Text.encodeUtf8 logicalName

-- | Names built from the bytes that actually collide with symbol syntax, in
-- runs long enough to exercise the repeat count. Components are non-empty
-- because 'Aihc.Grin.Syntax.grinScopedName' always fills in all three.
genComponent :: Gen Text
genComponent =
  T.concat
    <$> Gen.list
      (Range.linear 1 6)
      ( Gen.choice
          [ Gen.text (Range.linear 1 4) Gen.alphaNum,
            T.replicate <$> Gen.int (Range.linear 1 70) <*> (T.singleton <$> Gen.element symbolPunctuation),
            Gen.text (Range.linear 1 3) (Gen.filter (/= '\0') Gen.unicode)
          ]
      )

-- | The bytes that collide with symbol syntax: every coded escape, one that
-- has to fall back to hex, and one that is not ASCII at all.
symbolPunctuation :: [Char]
symbolPunctuation = "_,.-()$#'<=>*:/+[]\t\200"

-- | The inverse of the rendering, spelled out independently so that the test
-- pins the on-disk encoding rather than restating the renderer.
decodeLinkedSymbol :: Text -> Either String BS.ByteString
decodeLinkedSymbol rendered = BS.pack <$> go (T.unpack rendered)
  where
    go [] = Right []
    -- A separator that runs into an escape gives @___@, so read the whole run
    -- of underscores at once: the last two open an escape, and any before them
    -- are separators. Components are never empty, so there are at most three.
    go ('_' : rest) =
      case span (== '_') rest of
        (extra, afterRun)
          | run < 2 -> (0 :) <$> go afterRun
          | otherwise ->
              let (digits, coded) = span isDigit afterRun
                  count = if null digits then 1 else read digits
               in do
                    (byte, remaining) <- readCode coded
                    ((replicate (run - 2) 0 <> replicate count byte) <>) <$> go remaining
          where
            run = 1 + length extra
    go (intact : rest) = (fromIntegral (ord intact) :) <$> go rest
    readCode ('x' : high : low : rest)
      | isHexDigit high && isHexDigit low =
          Right (fromIntegral (digitToInt high * 16 + digitToInt low), rest)
    readCode (code : rest)
      | Just source <- lookup code codeTable = Right (fromIntegral (ord source), rest)
    readCode rest = Left ("unreadable escape at " <> take 8 rest)

codeTable :: [(Char, Char)]
codeTable =
  [ ('u', '_'),
    ('c', ','),
    ('d', '.'),
    ('m', '-'),
    ('p', '('),
    ('q', ')'),
    ('s', '$'),
    ('h', '#'),
    ('r', '\''),
    ('g', '>'),
    ('e', '='),
    ('l', '<'),
    ('a', '*'),
    ('o', ':'),
    ('f', '/'),
    ('t', '+'),
    ('k', '['),
    ('j', ']')
  ]
