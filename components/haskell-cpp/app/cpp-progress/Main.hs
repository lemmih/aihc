module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest, progressSummary)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
  cases <- loadManifest
  outcomes <- mapM evaluateCase cases
  let (passN, xfailN, xpassN, failN) = progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN
  putStrLn "Haskell CPP preprocessor progress"
  putStrLn "==============================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  mapM_ printFail [(m, d) | (m, OutcomeFail, d) <- outcomes]
  mapM_ printXPass [(m, d) | (m, OutcomeXPass, d) <- outcomes]

  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

printFail :: (CaseMeta, String) -> IO ()
printFail (meta, details) =
  putStrLn ("FAIL " <> caseId meta <> " [" <> caseCategory meta <> "] " <> details)

printXPass :: (CaseMeta, String) -> IO ()
printXPass (meta, details) =
  putStrLn ("XPASS " <> caseId meta <> " [" <> caseCategory meta <> "] " <> details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
