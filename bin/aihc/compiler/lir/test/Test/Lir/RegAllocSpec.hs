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
    allocateRegisters,
    functionIntervals,
  )
import Aihc.Lir.Syntax (Function, Var (..))
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

tests :: FilePath -> IO TestTree
tests evalDirectory = do
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory evalDirectory
  pure
    ( testGroup
        "register allocation"
        [ testGroup "shapes" unitTests,
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

-- | Every value that shares a register with another has an interval disjoint
-- from it. This is the property the backends depend on.
invariantTest :: FilePath -> FilePath -> TestTree
invariantTest directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  lirModule <- either (assertFailure . renderParseError) pure (parseModule source)
  mapM_ check [function | ItemFunction function <- moduleItems lirModule]
  where
    check function = do
      let allocation = allocateRegisters (pool 5) function
          registers = allocationRegisters allocation
          intervals = Map.fromList [(intervalVar interval, interval) | interval <- functionIntervals function]
      sequence_
        [ assertBool
            ( "in "
                <> name
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

overlaps :: Interval -> Interval -> Bool
overlaps left right =
  intervalStart left <= intervalEnd right && intervalStart right <= intervalEnd left

lookupInterval :: Map.Map Var Interval -> Text -> IO Interval
lookupInterval intervals name =
  maybe (assertFailure ("no interval for " <> show name)) pure (Map.lookup (Var name) intervals)

-- | The single function of a source that defines exactly one.
one :: Text -> IO Function
one source = do
  lirModule <- either (assertFailure . renderParseError) pure (parseModule source)
  case [function | ItemFunction function <- moduleItems lirModule] of
    [function] -> pure function
    functions -> assertFailure ("expected one function, found " <> show (length functions))

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
