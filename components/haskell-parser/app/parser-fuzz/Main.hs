module Main (main) where

import ParserFuzz.CLI (parseOptions)
import ParserFuzz.Testing (runWithOptions)

main :: IO ()
main = parseOptions >>= runWithOptions
