{-# LANGUAGE OverloadedStrings #-}

-- | The linear-scan register allocator.
--
-- The unit cases pin down what the allocator does with a few shapes. The
-- invariant case is the one that matters: over every evaluation fixture, two
-- values that share a register never have overlapping intervals.
module Test.Lir.RegAllocSpec
  ( tests,
  )
where

import Aihc.Lir (Item (..), Module (..), parseModule, renderParseError)
import Aihc.Lir.RegAlloc
  ( Allocation (..),
    Interval (..),
    Registers (..),
    allocateRegisters,
    allocateRegistersFor,
    functionIntervals,
  )
import Aihc.Lir.Syntax (ExternFunction (..), Function (..), Signature, Symbol, Var (..), functionSignature)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | A pool of made-up registers. The allocator never looks inside one.
pool :: Int -> [Text]
pool count = take count ["r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9"]

-- | A made-up target: four volatile registers that carry no argument, the
-- argument registers @a0@ to @a7@, and two preserved registers.
target :: Bool -> Registers Text
target preservedCost =
  Registers
    { registersVolatile = ["t0", "t1", "t2", "t3"] <> arguments,
      registersPreserved = ["s0", "s1"],
      registersPreservedCost = preservedCost,
      registersArgument = \index -> if index < length arguments then Just (arguments !! index) else Nothing
    }
  where
    arguments = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"]

-- | The signatures a module declares, as the backends pass them.
moduleSignatures :: Module -> Map.Map Symbol Signature
moduleSignatures (Module items) =
  Map.fromList
    ( [(functionName function, functionSignature function) | ItemFunction function <- items]
        <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
    )

tests :: FilePath -> IO TestTree
tests evalDirectory = do
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory evalDirectory
  pure
    ( testGroup
        "register allocation"
        [ testGroup "shapes" unitTests,
          testGroup "conventions" conventionTests,
          testGroup "no two live values share a register" (map (invariantTest evalDirectory) names)
        ]
    )

unitTests :: [TestTree]
unitTests =
  [ testCase "a value touched once does not earn a register" $ do
      function <- one chainSource
      let allocation = allocateRegisters (pool 4) function
      assertEqual
        "every value stays in a frame slot"
        [Var "x", Var "a", Var "b", Var "c"]
        (allocationSpills allocation)
      assertEqual "no register is saved" [] (allocationUsed allocation),
    testCase "a value touched often earns a register" $ do
      function <- one thriceSource
      let allocation = allocateRegisters (pool 4) function
      assertBool
        "the value three instructions read is in a register"
        (Map.member (Var "x") (allocationRegisters allocation))
      assertEqual
        "the values one instruction reads stay in frame slots"
        [Var "a", Var "b"]
        (allocationSpills allocation),
    testCase "more exits raise the bar" $ do
      -- The same three reads no longer pay for themselves once two exits
      -- each have to restore the register.
      function <- one twoExitSource
      let allocation = allocateRegisters (pool 4) function
      assertEqual "no register is saved" [] (allocationUsed allocation),
    testCase "a loop keeps its carried values in registers" $ do
      function <- one loopSource
      let registers = allocationRegisters (allocateRegisters (pool 10) function)
      mapM_
        (\name -> assertBool (show name <> " is in a register") (Map.member (Var name) registers))
        ["i", "acc", "j", "total", "next", "sum"]
      assertBool
        "the counter and the accumulator do not share a register"
        (Map.lookup (Var "i") registers /= Map.lookup (Var "acc") registers),
    testCase "an empty pool spills every value in definition order" $ do
      function <- one loopSource
      let allocation = allocateRegisters (pool 0) function
      assertEqual "nothing is in a register" Map.empty (allocationRegisters allocation)
      assertEqual
        "every value spills, in the order the function defines them"
        [Var "n", Var "i", Var "acc", Var "done", Var "j", Var "total", Var "next", Var "sum", Var "result"]
        (allocationSpills allocation),
    testCase "a small pool spills the rest of a loop" $ do
      function <- one loopSource
      let allocation = allocateRegisters (pool 2) function
      assertBool "some values spill" (not (null (allocationSpills allocation)))
      assertBool
        "no more registers than the pool holds"
        (length (allocationUsed allocation) <= 2),
    testCase "the values a loop carries have overlapping intervals" $ do
      function <- one loopSource
      let intervals = Map.fromList [(intervalVar interval, interval) | interval <- functionIntervals function]
      counter <- lookupInterval intervals "i"
      accumulator <- lookupInterval intervals "acc"
      assertBool
        "the counter and the accumulator are live at the same time"
        (overlaps counter accumulator)
  ]

conventionTests :: [TestTree]
conventionTests =
  [ testCase "a parameter keeps its argument register" $ do
      function <- one chainSource
      let registers = allocationRegisters (allocateRegistersFor (target False) Map.empty function)
      assertEqual "the parameter is in the first argument register" (Just "a0") (Map.lookup (Var "x") registers),
    testCase "a returned value takes the result register" $ do
      function <- one chainSource
      let registers = allocationRegisters (allocateRegistersFor (target False) Map.empty function)
      assertEqual "the result is in the first argument register" (Just "a0") (Map.lookup (Var "c") registers),
    testCase "a loop carries each value in one register" $ do
      function <- one loopSource
      let registers = allocationRegisters (allocateRegistersFor (target False) Map.empty function)
          register name = Map.lookup (Var name) registers
      assertEqual "the counter keeps the register of the parameter" (Just "a0") (register "i")
      assertEqual "the counter keeps one register around the loop" (register "i") (register "j")
      assertEqual "the accumulator keeps one register around the loop" (register "acc") (register "total")
      assertEqual "the next accumulator takes the register the old one gave up" (register "acc") (register "sum")
      assertBool "the counter and the accumulator do not share a register" (register "i" /= register "acc"),
    testCase "a block parameter is live from its own block" $ do
      function <- one loopSource
      let intervals = Map.fromList [(intervalVar interval, interval) | interval <- functionIntervals function]
      parameter <- lookupInterval intervals "n"
      counter <- lookupInterval intervals "i"
      assertBool "the parameter has died before the counter is born" (intervalEnd parameter < intervalStart counter),
    testCase "a value live across a C call takes a preserved register" $ do
      (signatures, function) <- oneWith cCallSource
      let registers = allocationRegisters (allocateRegistersFor (target False) signatures function)
      assertBool "the value that lives across the call is preserved" (Map.lookup (Var "x") registers `elem` [Just "s0", Just "s1"])
      assertEqual "the result of the call arrives in the result register" (Just "a0") (Map.lookup (Var "a") registers),
    testCase "a value live across a C call earns its preserved register" $ do
      (signatures, function) <- oneWith cCallOnceSource
      let free = allocateRegistersFor (target False) signatures function
          charged = allocateRegistersFor (target True) signatures function
      assertBool "a free preserved register is taken" (Map.member (Var "x") (allocationRegisters free))
      assertEqual "one use after the call does not pay for a save" [Var "x"] (allocationSpills charged),
    testCase "a value live across an aihc call spills" $ do
      (signatures, function) <- oneWith aihcCallSource
      let allocation = allocateRegistersFor (target False) signatures function
      assertEqual "no register survives an aihc call" [Var "x"] (allocationSpills allocation)
  ]

-- | Every value that shares a register with another has an interval disjoint
-- from it. This is the property the backends depend on.
invariantTest :: FilePath -> FilePath -> TestTree
invariantTest directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  lirModule <- either (assertFailure . renderParseError) pure (parseModule source)
  let signatures = moduleSignatures lirModule
  sequence_
    [ check description (allocate function) function
    | ItemFunction function <- moduleItems lirModule,
      (description, allocate) <-
        [ ("a preserved pool", allocateRegisters (pool 5)),
          ("a target", allocateRegistersFor (target False) signatures),
          ("a target that charges for preserved registers", allocateRegistersFor (target True) signatures)
        ]
    ]
  where
    check description allocation function = do
      let registers = allocationRegisters allocation
          intervals = Map.fromList [(intervalVar interval, interval) | interval <- functionIntervals function]
      sequence_
        [ assertBool
            ( "in "
                <> name
                <> " with "
                <> description
                <> ": "
                <> show (unVar left)
                <> " and "
                <> show (unVar right)
                <> " share a register while both are live"
            )
            (not (overlaps leftInterval rightInterval))
        | (left, leftRegister) <- Map.toList registers,
          (right, rightRegister) <- Map.toList registers,
          left < right,
          leftRegister == rightRegister,
          Just leftInterval <- [Map.lookup left intervals],
          Just rightInterval <- [Map.lookup right intervals]
        ]

-- | Whether two intervals are live at the same time. Two values may share a
-- register when one ends exactly where the other begins, since the
-- instruction that consumes the one defines the other, unless the one never
-- lived past its own definition.
overlaps :: Interval -> Interval -> Bool
overlaps left right =
  intervalStart left < intervalEnd right
    && intervalStart right < intervalEnd left
    || intervalStart left == intervalStart right
    || meets left right
    || meets right left
  where
    meets earlier later = intervalEnd earlier == intervalStart later && intervalStart earlier == intervalEnd earlier

lookupInterval :: Map.Map Var Interval -> Text -> IO Interval
lookupInterval intervals name =
  maybe (assertFailure ("no interval for " <> show name)) pure (Map.lookup (Var name) intervals)

-- | The single function of a source that defines exactly one.
one :: Text -> IO Function
one = fmap snd . oneWith

-- | The single function of a source, with the signatures the source
-- declares.
oneWith :: Text -> IO (Map.Map Symbol Signature, Function)
oneWith source = do
  lirModule <- either (assertFailure . renderParseError) pure (parseModule source)
  case [function | ItemFunction function <- moduleItems lirModule] of
    [function] -> pure (moduleSignatures lirModule, function)
    functions -> assertFailure ("expected one function, found " <> show (length functions))

-- | A value that lives across a C call and one use of it afterwards.
cCallSource :: Text
cCallSource =
  "extern func @helper(i64) -> i64 cc c\n\
  \func @across(%x: i64) -> i64 {\n\
  \entry:\n\
  \  %a = call @helper(%x)\n\
  \  %b = add i64 %a, %x\n\
  \  return %b\n\
  \}\n"

-- | A value that lives across a C call with only one use in the function.
cCallOnceSource :: Text
cCallOnceSource =
  "extern func @helper(i64) -> i64 cc c\n\
  \func @across(%x: i64) -> i64 {\n\
  \entry:\n\
  \  %a = call @helper(1)\n\
  \  %b = add i64 %a, %x\n\
  \  return %b\n\
  \}\n"

-- | The same shape across an aihc call.
aihcCallSource :: Text
aihcCallSource =
  "extern func @helper(i64) -> i64\n\
  \func @across(%x: i64) -> i64 {\n\
  \entry:\n\
  \  %a = call @helper(%x)\n\
  \  %b = add i64 %a, %x\n\
  \  return %b\n\
  \}\n"

chainSource :: Text
chainSource =
  "func @chain(%x: i64) -> i64 {\n\
  \entry:\n\
  \  %a = add i64 %x, 1\n\
  \  %b = add i64 %a, 1\n\
  \  %c = add i64 %b, 1\n\
  \  return %c\n\
  \}\n"

-- | Three instructions read @%x@, and the function has one exit, so the
-- register it would take pays for itself.
thriceSource :: Text
thriceSource =
  "func @thrice(%x: i64) -> i64 {\n\
  \entry:\n\
  \  %a = add i64 %x, %x\n\
  \  %b = add i64 %a, %x\n\
  \  return %b\n\
  \}\n"

-- | The same reads of @%x@, but two exits each restore what the prologue
-- saved.
twoExitSource :: Text
twoExitSource =
  "func @branchy(%x: i64, %c: i1) -> i64 {\n\
  \entry:\n\
  \  br %c, left, right\n\
  \\n\
  \left:\n\
  \  %p = add i64 %x, 1\n\
  \  return %p\n\
  \\n\
  \right:\n\
  \  %q = add i64 %x, 2\n\
  \  return %q\n\
  \}\n"

loopSource :: Text
loopSource =
  "func @count(%n: i64) -> i64 {\n\
  \entry:\n\
  \  jump loop(%n, 0)\n\
  \\n\
  \loop(%i: i64, %acc: i64):\n\
  \  %done = eq i64 %i, 0\n\
  \  br %done, exit(%acc), step(%i, %acc)\n\
  \\n\
  \step(%j: i64, %total: i64):\n\
  \  %next = sub i64 %j, 1\n\
  \  %sum = add i64 %total, %j\n\
  \  jump loop(%next, %sum)\n\
  \\n\
  \exit(%result: i64):\n\
  \  return %result\n\
  \}\n"
