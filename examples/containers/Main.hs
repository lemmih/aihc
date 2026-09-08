-- Show the four main structures of the containers package:
-- maps, sets, trees, and sequences.
module Main where

import Data.Foldable (toList)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree

sampleWords :: [String]
sampleWords = words "the quick brown fox jumps over the lazy dog the fox sleeps"

-- A map counts how many times each word is present.
wordCounts :: Map String Int
wordCounts = Map.fromListWith (+) (map (\word -> (word, 1)) sampleWords)

showCount :: (String, Int) -> String
showCount (word, count) = word ++ "=" ++ show count

vowels :: Set Char
vowels = Set.fromList "aeiou"

letters :: Set Char
letters = Set.fromList (concat sampleWords)

-- A tree holds a small directory layout.
fileTree :: Tree String
fileTree =
  Node
    "src"
    [ Node "Main.hs" [],
      Node
        "Data"
        [ Node "Map.hs" [],
          Node "Set.hs" []
        ],
      Node "Tree.hs" []
    ]

queue :: Seq Int
queue = 0 <| (Seq.fromList [1, 2, 3, 4] |> 5)

section :: String -> IO ()
section title = putStrLn ("== " ++ title ++ " ==")

main :: IO ()
main = do
  section "Map"
  putStrLn ("size: " ++ show (Map.size wordCounts))
  putStrLn ("counts: " ++ unwords (map showCount (Map.toList wordCounts)))
  putStrLn ("the: " ++ show (Map.lookup "the" wordCounts))
  putStrLn ("cat: " ++ show (Map.lookup "cat" wordCounts))
  putStrLn ("repeated: " ++ unwords (Map.keys (Map.filter (> 1) wordCounts)))
  putStrLn ("total: " ++ show (sum (Map.elems wordCounts)))

  section "Set"
  putStrLn ("letters: " ++ Set.toList letters)
  putStrLn ("vowels used: " ++ Set.toList (Set.intersection letters vowels))
  putStrLn ("no vowels: " ++ Set.toList (Set.difference letters vowels))
  putStrLn ("has q: " ++ show (Set.member 'q' letters))
  putStrLn ("has y: " ++ show (Set.member 'y' letters))

  section "Tree"
  putStr (Tree.drawTree fileTree)
  putStrLn ("nodes: " ++ show (length (Tree.flatten fileTree)))
  putStrLn ("depth: " ++ show (length (Tree.levels fileTree)))
  putStrLn ("sorted: " ++ unwords (sort (Tree.flatten fileTree)))

  section "Seq"
  putStrLn ("queue: " ++ show (toList queue))
  putStrLn ("length: " ++ show (Seq.length queue))
  putStrLn ("index 2: " ++ show (Seq.index queue 2))
  putStrLn ("reverse: " ++ show (toList (Seq.reverse queue)))
  putStrLn ("doubled: " ++ show (toList (fmap (* 2) queue)))
  let (front, back) = Seq.splitAt 3 queue
  putStrLn ("front: " ++ show (toList front))
  putStrLn ("back: " ++ show (toList back))
