-- | Progress reporting for the type checker.
--
-- Runs the whole @aihc-tc@ test tree -- the annotated fixtures, the
-- annotation traversal check, and the hand-written unit and property tests --
-- and outputs a summary in the standard PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE
-- format. Fixtures marked @xfail@ or @xpass@ are green in the test tree but
-- describe unimplemented behaviour, so they are reported as XFAIL/XPASS rather
-- than as passes.
module Main (main) where

import Control.Concurrent.STM (TVar, atomically, readTVar, retry)
import Data.IntMap.Strict qualified as IntMap
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import TcAnnotatedGolden (ExpectedStatus (..), TcAnnotatedCase (..), loadTcAnnotatedCases)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Runners
  ( Result,
    Status (Done),
    TreeFold (..),
    foldTestTree,
    launchTestTree,
    resultSuccessful,
    trivialFold,
  )
import Test.Tc.Interface (tcInterfaceTests)
import Test.Tc.Properties (tcProperties)
import Test.Tc.Suite (tcAnnotatedGoldenTests)
import Test.Tc.Traverse (tcTraverseTests)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args

  cases <- loadTcAnnotatedCases
  tree <- suiteTree
  results <- runSuite tree

  let failures = [name | (name, result) <- zip (testNames tree) results, not (resultSuccessful result)]
      totalN = length results
      xfailN = countStatus StatusXFail cases
      xpassN = countStatus StatusXPass cases
      failN = length failures
      passN = max 0 (totalN - failN - xfailN - xpassN)
      completion = pct (passN + xpassN) totalN

  putStrLn "Type checker progress"
  putStrLn "====================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  mapM_ (\name -> putStrLn ("FAIL " <> name)) failures

  if null failures && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

-- | The same test tree the @spec@ test suite runs.
suiteTree :: IO TestTree
suiteTree = do
  annotatedGoldenTests <- tcAnnotatedGoldenTests
  traverseTests <- tcTraverseTests
  pure
    ( testGroup
        "aihc-tc"
        [ annotatedGoldenTests,
          traverseTests,
          tcInterfaceTests,
          tcProperties
        ]
    )

-- | Run every test in the tree and collect the results in test index order.
runSuite :: TestTree -> IO [Result]
runSuite tree = launchTestTree mempty tree $ \statusMap -> do
  results <- traverse awaitResult (IntMap.elems statusMap)
  pure (\_time -> pure results)

awaitResult :: TVar Status -> IO Result
awaitResult var = atomically $ do
  status <- readTVar var
  case status of
    Done result -> pure result
    _ -> retry

-- | Fully qualified test names, in the same order as the status map.
testNames :: TestTree -> [String]
testNames = foldTestTree namesFold mempty
  where
    namesFold =
      trivialFold
        { foldSingle = \_opts name _test -> [name],
          foldGroup = \_opts name inner -> map ((name <> ".") <>) (concat inner)
        }

countStatus :: ExpectedStatus -> [TcAnnotatedCase] -> Int
countStatus wanted cases = length [() | tc <- cases, caseStatus tc == wanted]

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
