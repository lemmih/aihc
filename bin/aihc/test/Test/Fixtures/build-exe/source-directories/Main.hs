module Main where

import PrimitiveChecks (primitiveChecks)
import Message
import System.Environment (getArgs)
import System.IO ()

main :: IO ()
main = if primitiveChecks then run else error "primitive check failed"

run :: IO ()
run = do
  arguments <- getArgs
  case arguments of
    [] -> putStrLn message
    [first, second] -> do
      putStrLn first
      putStrLn second
    _ -> putStrLn "unexpected arguments"
