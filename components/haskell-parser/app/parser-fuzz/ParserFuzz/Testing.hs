module ParserFuzz.Testing (runWithOptions) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate)
import Data.Text qualified as T
import Language.Haskell.Exts qualified as HSE
import Parser (defaultConfig, parseModule)
import Parser.Types (ParseResult (..))
import ParserFuzz.Arbitrary (generateCandidate, normalizeCandidateAst, qcGenStream, shrinkGeneratedModule)
import ParserFuzz.CLI (Options (..))
import ParserFuzz.Types (Candidate (..), SearchResult (..))
import System.Random (randomIO)
import Test.QuickCheck (shrink)
import Test.QuickCheck.Random (QCGen)

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  seed <- resolveSeed (optSeed opts)
  found <- findFirstFailure opts seed
  case found of
    Nothing -> do
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (optMaxTests opts))
      putStrLn "No failing module found."
    Just result -> do
      let (minimized, shrinksAccepted) = shrinkCandidate opts (srCandidate result)
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (srTestsTried result))
      putStrLn ("Shrink passes accepted: " <> show shrinksAccepted)
      putStrLn "Minimized source:"
      putStrLn "---8<---"
      putStrLn (candSource minimized)
      putStrLn "--->8---"
      case optOutput opts of
        Nothing -> pure ()
        Just path -> do
          writeFile path (candSource minimized)
          putStrLn ("Wrote minimized source to " <> path)

resolveSeed :: Maybe Int -> IO Int
resolveSeed (Just seed) = pure seed
resolveSeed Nothing = randomIO

findFirstFailure :: Options -> Int -> IO (Maybe SearchResult)
findFirstFailure opts seed = go 1 (qcGenStream seed)
  where
    go :: Int -> [QCGen] -> IO (Maybe SearchResult)
    go _ [] = pure Nothing
    go idx (g : gs)
      | idx > optMaxTests opts = pure Nothing
      | otherwise =
          let candidate = generateCandidate (optSize opts) g
           in do
                printGeneratedModule opts idx candidate
                if oursFails (candSource candidate)
                  then pure (Just (SearchResult idx candidate))
                  else go (idx + 1) gs

printGeneratedModule :: Options -> Int -> Candidate -> IO ()
printGeneratedModule opts idx candidate
  | not (optPrintGeneratedModules opts) = pure ()
  | otherwise = do
      putStrLn ("Generated module #" <> show idx <> ":")
      putStrLn "---8<---"
      putStrLn (candSource candidate)
      putStrLn "--->8---"

shrinkCandidate :: Options -> Candidate -> (Candidate, Int)
shrinkCandidate opts = go 0
  where
    go :: Int -> Candidate -> (Candidate, Int)
    go accepted candidate
      | accepted >= optMaxShrinkPasses opts = (candidate, accepted)
      | otherwise =
          case firstSuccessfulShrink candidate of
            Nothing -> (candidate, accepted)
            Just nextCandidate -> go (accepted + 1) nextCandidate

firstSuccessfulShrink :: Candidate -> Maybe Candidate
firstSuccessfulShrink candidate = tryCandidates (candidateTransforms candidate)
  where
    tryCandidates :: [HSE.Module HSE.SrcSpanInfo] -> Maybe Candidate
    tryCandidates [] = Nothing
    tryCandidates (ast' : rest) =
      case normalizeCandidateAst "firstSuccessfulShrink" ast' of
        Left msg -> error msg
        Right candidate' ->
          if oursFails (candSource candidate')
            then Just candidate'
            else tryCandidates rest

candidateTransforms :: Candidate -> [HSE.Module HSE.SrcSpanInfo]
candidateTransforms candidate =
  removeModuleHead (candAst candidate)
    <> shrinkModuleHeadName (candAst candidate)
    <> removeModuleHeadExports (candAst candidate)
    <> shrinkModuleHeadExports (candAst candidate)
    <> shrinkGeneratedModule (candAst candidate)
    <> []

-- TODO: Add leaf-pruning transforms for exports, imports, and decl lists.
-- TODO: Add recursive subtree-pruning transforms across declaration/expr/type AST nodes.

removeModuleHead :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleHead modu =
  case modu of
    HSE.Module loc (Just _) pragmas imports decls ->
      [HSE.Module loc Nothing pragmas imports decls]
    _ -> []

shrinkModuleHeadName :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleHeadName modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc (HSE.ModuleName nLoc name) warning exports)) pragmas imports decls ->
      [ HSE.Module
          loc
          (Just (HSE.ModuleHead hLoc (HSE.ModuleName nLoc name') warning exports))
          pragmas
          imports
          decls
      | name' <- shrinkModuleName name
      ]
    _ -> []

removeModuleHeadExports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleHeadExports modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning (Just _))) pragmas imports decls ->
      [HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning Nothing)) pragmas imports decls]
    _ -> []

shrinkModuleHeadExports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleHeadExports modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning (Just (HSE.ExportSpecList eLoc specs)))) pragmas imports decls ->
      let removeAt idx = take idx specs <> drop (idx + 1) specs
          shrunkLists = [removeAt idx | idx <- [0 .. length specs - 1]] <> [[], specs]
       in [ HSE.Module
              loc
              (Just (HSE.ModuleHead hLoc modName warning (Just (HSE.ExportSpecList eLoc specs'))))
              pragmas
              imports
              decls
          | specs' <- unique shrunkLists,
            specs' /= specs
          ]
    _ -> []

shrinkModuleName :: String -> [String]
shrinkModuleName name =
  let parts = splitModuleName name
      joinedShrinks =
        [ intercalate "." parts'
        | parts' <- shrinkModuleNameParts parts,
          isValidModuleName parts'
        ]
      rawShrinks =
        [ raw
        | raw <- shrink name,
          isValidModuleName (splitModuleName raw)
        ]
   in unique (joinedShrinks <> rawShrinks)

shrinkModuleNameParts :: [String] -> [[String]]
shrinkModuleNameParts parts =
  dropSegmentShrinks parts <> shrinkSegmentShrinks parts

-- Remove one segment at a time (if at least one segment remains).
dropSegmentShrinks :: [String] -> [[String]]
dropSegmentShrinks parts =
  [ before <> after
  | idx <- [0 .. length parts - 1],
    let (before, rest) = splitAt idx parts,
    (_ : after) <- [rest],
    not (null (before <> after))
  ]

-- Shrink each segment independently.
shrinkSegmentShrinks :: [String] -> [[String]]
shrinkSegmentShrinks parts =
  [ replaceAt idx segment' parts
  | (idx, segment) <- zip [0 ..] parts,
    segment' <- shrink segment,
    isValidModuleSegment segment'
  ]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx value xs =
  let (before, rest) = splitAt idx xs
   in case rest of
        [] -> xs
        (_ : after) -> before <> (value : after)

unique :: (Eq a) => [a] -> [a]
unique = foldr keep []
  where
    keep x acc
      | x `elem` acc = acc
      | otherwise = x : acc

oursFails :: String -> Bool
oursFails source =
  case parseModule defaultConfig (T.pack source) of
    ParseErr _ -> True
    ParseOk _ -> False

splitModuleName :: String -> [String]
splitModuleName raw =
  case raw of
    "" -> []
    _ -> splitOnDot raw

splitOnDot :: String -> [String]
splitOnDot s =
  case break (== '.') s of
    (prefix, []) -> [prefix]
    (prefix, _ : rest) -> prefix : splitOnDot rest

isValidModuleName :: [String] -> Bool
isValidModuleName segments = not (null segments) && all isValidModuleSegment segments

isValidModuleSegment :: String -> Bool
isValidModuleSegment segment =
  case segment of
    first : rest -> isUpper first && all isSegmentRestChar rest
    [] -> False

isSegmentRestChar :: Char -> Bool
isSegmentRestChar ch = isAlphaNum ch || ch == '\''
