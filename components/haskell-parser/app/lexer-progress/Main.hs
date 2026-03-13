module Main (main) where

import LexerGolden
  ( LexerCase (..),
    Outcome (..),
    evaluateLexerCase,
    loadLexerCases,
    progressSummary,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
  cases <- loadLexerCases
  let outcomes = map evaluateCase cases
      (passN, xfailN, xpassN, failN) = progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  putStrLn "Haskell lexer progress"
  putStrLn "====================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  mapM_ printFail [(meta, details) | (meta, OutcomeFail, details) <- outcomes]
  mapM_ printXPass [(meta, details) | (meta, OutcomeXPass, details) <- outcomes]

  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

evaluateCase :: LexerCase -> (LexerCase, Outcome, String)
evaluateCase meta =
  let (outcome, details) = evaluateLexerCase meta
   in (meta, outcome, details)

printFail :: (LexerCase, String) -> IO ()
printFail (meta, details) =
  putStrLn ("FAIL " <> caseId meta <> " [" <> caseCategory meta <> "] " <> details)

printXPass :: (LexerCase, String) -> IO ()
printXPass (meta, details) =
  putStrLn ("XPASS " <> caseId meta <> " [" <> caseCategory meta <> "] " <> details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
