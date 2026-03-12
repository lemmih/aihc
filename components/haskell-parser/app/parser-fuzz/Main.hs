{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Language.Haskell.Exts as HSE
import Parser (defaultConfig, parseModule)
import Parser.Types (ParseResult (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (QCGen, mkQCGen)
#if MIN_VERSION_random(1,3,0)
import System.Random (splitGen)
#else
import System.Random (split)
#endif

data Options = Options
  { optSeed :: Maybe Int,
    optMaxTests :: Int,
    optSize :: Int,
    optMaxShrinkPasses :: Int,
    optOutput :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optSeed = Nothing,
      optMaxTests = 10000,
      optSize = 10,
      optMaxShrinkPasses = 1000,
      optOutput = Nothing
    }

data SearchResult = SearchResult
  { srTestsTried :: Int,
    srCandidate :: Candidate
  }

data Candidate = Candidate
  { candAst :: HSE.Module HSE.SrcSpanInfo,
    candComments :: [HSE.Comment],
    candSource :: String
  }

newtype GenModule = GenModule {unGenModule :: HSE.Module HSE.SrcSpanInfo}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    moduName <- genModuleName
    let header = HSE.ModuleHead HSE.noSrcSpan (HSE.ModuleName HSE.noSrcSpan moduName) Nothing Nothing
    pure (GenModule (HSE.Module HSE.noSrcSpan (Just header) [] [] []))

-- TODO: Add generators for module pragmas/imports/declarations.
-- TODO: Extend generator to cover additional Haskell syntax supported by HSE.

main :: IO ()
main = do
  args <- getArgs
  case parseArgs defaultOptions args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr ""
      hPutStrLn stderr usage
      exitFailure
    Right (opts, showHelp)
      | showHelp -> do
          putStrLn usage
          exitSuccess
      | otherwise -> runWithOptions opts

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  seed <- resolveSeed (optSeed opts)
  mResult <- findFirstFailure opts seed
  case mResult of
    Nothing -> do
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (optMaxTests opts))
      putStrLn "No failing module found."
      exitSuccess
    Just found -> do
      (minimized, shrinksAccepted) <- shrinkCandidate opts (srCandidate found)
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (srTestsTried found))
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
resolveSeed Nothing = floor <$> getPOSIXTime

findFirstFailure :: Options -> Int -> IO (Maybe SearchResult)
findFirstFailure opts seed = go 1 (qcGenStream (mkQCGen seed))
  where
    go :: Int -> [QCGen] -> IO (Maybe SearchResult)
    go _ [] = pure Nothing
    go idx (g : gs)
      | idx > optMaxTests opts = pure Nothing
      | otherwise = do
          let generated = unGen (arbitrary :: Gen GenModule) g (optSize opts)
          mCandidate <- materializeCandidate generated
          case mCandidate of
            Just candidate
              | oursFails (candSource candidate) ->
                  pure (Just (SearchResult idx candidate))
            _ -> go (idx + 1) gs

materializeCandidate :: GenModule -> IO (Maybe Candidate)
materializeCandidate (GenModule modu0) = do
  let source0 = HSE.prettyPrint modu0
      mode = hseParseMode
  case HSE.parseFileContentsWithComments mode source0 of
    HSE.ParseFailed _ _ -> pure Nothing
    HSE.ParseOk (modu1, comments) -> do
      mSource <- safeExactPrint modu1 comments
      pure $ do
        source <- mSource
        pure
          Candidate
            { candAst = modu1,
              candComments = comments,
              candSource = source
            }

shrinkCandidate :: Options -> Candidate -> IO (Candidate, Int)
shrinkCandidate opts = go 0
  where
    go :: Int -> Candidate -> IO (Candidate, Int)
    go accepted candidate
      | accepted >= optMaxShrinkPasses opts = pure (candidate, accepted)
      | otherwise = do
          mNext <- firstSuccessfulShrink candidate
          case mNext of
            Nothing -> pure (candidate, accepted)
            Just nextCandidate -> go (accepted + 1) nextCandidate

firstSuccessfulShrink :: Candidate -> IO (Maybe Candidate)
firstSuccessfulShrink candidate = tryCandidates (candidateTransforms candidate)
  where
    tryCandidates :: [HSE.Module HSE.SrcSpanInfo] -> IO (Maybe Candidate)
    tryCandidates [] = pure Nothing
    tryCandidates (ast' : rest) = do
      mSource <- safeExactPrint ast' (candComments candidate)
      case mSource of
        Nothing -> tryCandidates rest
        Just source'
          | isMeaningfulSource source' && oursFails source' ->
              pure
                ( Just
                    Candidate
                      { candAst = ast',
                        candComments = candComments candidate,
                        candSource = source'
                      }
                )
          | otherwise -> tryCandidates rest

isMeaningfulSource :: String -> Bool
isMeaningfulSource = any (not . isSpace)

candidateTransforms :: Candidate -> [HSE.Module HSE.SrcSpanInfo]
candidateTransforms candidate =
  removeModuleHead (candAst candidate)
    <> shrinkModuleHeadName (candAst candidate)
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

safeExactPrint :: HSE.Module HSE.SrcSpanInfo -> [HSE.Comment] -> IO (Maybe String)
safeExactPrint ast comments = do
  result <- try (evaluate (force (HSE.exactPrint ast comments))) :: IO (Either SomeException String)
  case result of
    Left _ -> pure Nothing
    Right rendered -> pure (Just rendered)

oursFails :: String -> Bool
oursFails source =
  case parseModule defaultConfig (T.pack source) of
    ParseErr _ -> True
    ParseOk _ -> False

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<generated>",
      HSE.extensions = HSE.glasgowExts
    }

genModuleName :: Gen String
genModuleName = do
  segmentCount <- chooseInt (1, 4)
  segments <- vectorOf segmentCount genModuleSegment
  pure (intercalate "." segments)

genModuleSegment :: Gen String
genModuleSegment = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 10)
  rest <- vectorOf restLen (elements moduleSegmentChars)
  pure (first : rest)

moduleSegmentChars :: [Char]
moduleSegmentChars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "'"

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

qcGenStream :: QCGen -> [QCGen]
qcGenStream seed =
  let (g1, g2) = splitQcGen seed
   in g1 : qcGenStream g2

splitQcGen :: QCGen -> (QCGen, QCGen)
#if MIN_VERSION_random(1,3,0)
splitQcGen = splitGen
#else
splitQcGen = split
#endif

parseArgs :: Options -> [String] -> Either String (Options, Bool)
parseArgs opts = go opts False
  where
    go :: Options -> Bool -> [String] -> Either String (Options, Bool)
    go current showHelp rest =
      case rest of
        [] -> Right (current, showHelp)
        "--help" : xs -> go current True xs
        "--seed" : value : xs -> do
          seed <- readIntFlag "--seed" value
          go current {optSeed = Just seed} showHelp xs
        "--max-tests" : value : xs -> do
          n <- readPositiveIntFlag "--max-tests" value
          go current {optMaxTests = n} showHelp xs
        "--size" : value : xs -> do
          n <- readPositiveIntFlag "--size" value
          go current {optSize = n} showHelp xs
        "--max-shrink-passes" : value : xs -> do
          n <- readPositiveIntFlag "--max-shrink-passes" value
          go current {optMaxShrinkPasses = n} showHelp xs
        "--output" : value : xs ->
          go current {optOutput = Just value} showHelp xs
        flag : _
          | flag `elem` ["--seed", "--max-tests", "--size", "--max-shrink-passes", "--output"] ->
              Left ("Missing value for " <> flag)
          | otherwise -> Left ("Unknown flag: " <> flag)

readIntFlag :: String -> String -> Either String Int
readIntFlag flag raw =
  case reads raw of
    [(n, "")] -> Right n
    _ -> Left ("Invalid integer for " <> flag <> ": " <> raw)

readPositiveIntFlag :: String -> String -> Either String Int
readPositiveIntFlag flag raw = do
  n <- readIntFlag flag raw
  if n > 0
    then Right n
    else Left ("Expected a positive integer for " <> flag <> ", got: " <> raw)

usage :: String
usage =
  unlines
    [ "Usage: parser-fuzz [--seed N] [--max-tests N] [--size N] [--max-shrink-passes N] [--output PATH] [--help]",
      "",
      "Generate HSE modules, find a source that fails aihc-parser, and minimize it with exactPrint.",
      "",
      "Flags:",
      "  --seed N                Deterministic random seed (default: current POSIX time)",
      "  --max-tests N           Number of generated modules to try (default: 10000)",
      "  --size N                QuickCheck generation size (default: 10)",
      "  --max-shrink-passes N   Maximum accepted shrink steps (default: 1000)",
      "  --output PATH           Write minimized source to PATH",
      "  --help                  Show this help"
    ]
