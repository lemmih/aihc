module Main (main) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate)
import Data.Text qualified as T
import Language.Haskell.Exts qualified as HSE
import Options.Applicative qualified as OA
import Parser (defaultConfig, parseModule)
import Parser.Types (ParseResult (..))
import System.Random (randomIO)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (QCGen, mkQCGen)

data Options = Options
  { optSeed :: Maybe Int,
    optMaxTests :: Int,
    optSize :: Int,
    optMaxShrinkPasses :: Int,
    optOutput :: Maybe FilePath
  }

data SearchResult = SearchResult
  { srTestsTried :: Int,
    srCandidate :: Candidate
  }

data Candidate = Candidate
  { candAst :: HSE.Module HSE.SrcSpanInfo,
    candSource :: String
  }

newtype GenModule = GenModule {unGenModule :: HSE.Module HSE.SrcSpanInfo}
  deriving (Show)

newtype GenModuleHead = GenModuleHead {unGenModuleHead :: Maybe (HSE.ModuleHead HSE.SrcSpanInfo)}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    GenModuleHead header <- arbitrary
    pure (GenModule (HSE.Module HSE.noSrcSpan header [] [] []))

instance Arbitrary GenModuleHead where
  arbitrary =
    oneof
      [ pure (GenModuleHead Nothing),
        do
          moduName <- genModuleName
          let header = HSE.ModuleHead HSE.noSrcSpan (HSE.ModuleName HSE.noSrcSpan moduName) Nothing Nothing
          pure (GenModuleHead (Just header))
      ]

-- TODO: Add generators for module pragmas/imports/declarations.
-- TODO: Extend generator to cover additional Haskell syntax supported by HSE.

main :: IO ()
main = do
  opts <- OA.execParser parserInfo
  runWithOptions opts

parserInfo :: OA.ParserInfo Options
parserInfo =
  OA.info
    (optionsParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Generate HSE modules, find parser failures, and shrink with exactPrint"
        <> OA.header "parser-fuzz"
    )

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> OA.optional
      ( OA.option
          OA.auto
          ( OA.long "seed"
              <> OA.metavar "N"
              <> OA.help "Deterministic random seed (default: randomIO)"
          )
      )
    <*> OA.option
      OA.auto
      ( OA.long "max-tests"
          <> OA.metavar "N"
          <> OA.value 10000
          <> OA.showDefault
          <> OA.help "Number of generated modules to try"
      )
    <*> OA.option
      OA.auto
      ( OA.long "size"
          <> OA.metavar "N"
          <> OA.value 10
          <> OA.showDefault
          <> OA.help "QuickCheck generation size"
      )
    <*> OA.option
      OA.auto
      ( OA.long "max-shrink-passes"
          <> OA.metavar "N"
          <> OA.value 1000
          <> OA.showDefault
          <> OA.help "Maximum accepted shrink steps"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "output"
              <> OA.metavar "PATH"
              <> OA.help "Write minimized source to PATH"
          )
      )

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  seed <- resolveSeed (optSeed opts)
  case findFirstFailure opts seed of
    Nothing -> do
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (optMaxTests opts))
      putStrLn "No failing module found."
    Just found -> do
      let (minimized, shrinksAccepted) = shrinkCandidate opts (srCandidate found)
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
resolveSeed Nothing = randomIO

findFirstFailure :: Options -> Int -> Maybe SearchResult
findFirstFailure opts seed = go 1 (qcGenStream seed)
  where
    go :: Int -> [QCGen] -> Maybe SearchResult
    go _ [] = Nothing
    go idx (g : gs)
      | idx > optMaxTests opts = Nothing
      | otherwise =
          let generated = unGen (arbitrary :: Gen GenModule) g (optSize opts)
              candidate = materializeCandidate generated
           in if oursFails (candSource candidate)
                then Just (SearchResult idx candidate)
                else go (idx + 1) gs

materializeCandidate :: GenModule -> Candidate
materializeCandidate (GenModule modu0) =
  let source0 = HSE.prettyPrint modu0
      mode = hseParseMode
   in case HSE.parseFileContentsWithMode mode source0 of
        HSE.ParseFailed loc err ->
          error
            ( "materializeCandidate: generated AST failed to parse at "
                <> show loc
                <> " with error: "
                <> err
                <> "\nsource:\n"
                <> source0
            )
        HSE.ParseOk modu1 ->
          let source1 = HSE.exactPrint modu1 []
           in Candidate
                { candAst = modu1,
                  candSource = source1
                }

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
      let source' = HSE.exactPrint ast' []
       in if oursFails source'
            then
              Just
                Candidate
                  { candAst = ast',
                    candSource = source'
                  }
            else tryCandidates rest

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

qcGenStream :: Int -> [QCGen]
qcGenStream seed = map mkQCGen [seed ..]
