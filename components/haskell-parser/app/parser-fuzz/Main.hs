module Main (main) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate)
import Data.Text qualified as T
import Language.Haskell.Exts qualified as HSE
import Language.Haskell.Exts.Build qualified as HSEB
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

newtype GenModule = GenModule {unGenModule :: HSE.Module ()}
  deriving (Show)

newtype GenModuleHead = GenModuleHead {unGenModuleHead :: Maybe (HSE.ModuleHead ())}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    GenModuleHead header <- arbitrary
    pure (GenModule (HSE.Module () header [] [] []))

instance Arbitrary GenModuleHead where
  arbitrary =
    oneof
      [ pure (GenModuleHead Nothing),
        do
          moduName <- genModuleName
          exports <- genMaybeExportSpecList
          let header = HSE.ModuleHead () (HSE.ModuleName () moduName) Nothing exports
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
  case normalizeCandidateSource "materializeCandidate" (HSE.prettyPrint modu0) of
    Left msg -> error msg
    Right candidate -> candidate

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

genMaybeExportSpecList :: Gen (Maybe (HSE.ExportSpecList ()))
genMaybeExportSpecList =
  frequency
    [ (3, pure Nothing),
      (7, Just <$> genExportSpecList)
    ]

genExportSpecList :: Gen (HSE.ExportSpecList ())
genExportSpecList = do
  n <- chooseInt (0, 4)
  specs <- vectorOf n genExportSpec
  pure (HSE.ExportSpecList () specs)

genExportSpec :: Gen (HSE.ExportSpec ())
genExportSpec =
  frequency
    [ (4, HSE.EVar () <$> genVarQName),
      (3, HSE.EAbs () (HSE.NoNamespace ()) <$> genTypeQName),
      (3, genEThingWith),
      (2, HSE.EModuleContents () <$> genModuleNameNode)
    ]

genEThingWith :: Gen (HSE.ExportSpec ())
genEThingWith = do
  qtycon <- genTypeQName
  n <- chooseInt (0, 3)
  cnames <- vectorOf n genCName
  let wildcard = HSE.NoWildcard ()
  pure (HSE.EThingWith () wildcard qtycon cnames)

genVarQName :: Gen (HSE.QName ())
genVarQName =
  frequency
    [ (4, HSE.UnQual () . HSEB.name <$> genVarIdent),
      ( 1,
        do
          modu <- genModuleNameNode
          HSE.Qual () modu . HSEB.name <$> genVarIdent
      )
    ]

genTypeQName :: Gen (HSE.QName ())
genTypeQName =
  frequency
    [ (4, HSE.UnQual () . HSEB.name <$> genConIdent),
      ( 1,
        do
          modu <- genModuleNameNode
          HSE.Qual () modu . HSEB.name <$> genConIdent
      )
    ]

genCName :: Gen (HSE.CName ())
genCName =
  frequency
    [ (1, HSE.VarName () . HSEB.name <$> genVarIdent),
      (2, HSE.ConName () . HSEB.name <$> genConIdent)
    ]

genModuleNameNode :: Gen (HSE.ModuleName ())
genModuleNameNode = HSE.ModuleName () <$> genModuleName

genVarIdent :: Gen String
genVarIdent = do
  first <- elements ['a' .. 'z']
  restLen <- chooseInt (0, 10)
  rest <- vectorOf restLen (elements moduleSegmentChars)
  let ident = first : rest
  if ident `elem` reservedWords
    then genVarIdent
    else pure ident

genConIdent :: Gen String
genConIdent = genModuleSegment

reservedWords :: [String]
reservedWords =
  [ "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "foreign",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]

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

normalizeCandidateAst :: String -> HSE.Module HSE.SrcSpanInfo -> Either String Candidate
normalizeCandidateAst context modu0 =
  normalizeCandidateSource context (HSE.prettyPrint modu0)

normalizeCandidateSource :: String -> String -> Either String Candidate
normalizeCandidateSource context source0 =
  let mode = hseParseMode
   in case HSE.parseFileContentsWithMode mode source0 of
        HSE.ParseFailed loc err ->
          Left
            ( context
                <> ": internal bug: candidate AST failed to parse after prettyPrint at "
                <> show loc
                <> " with error: "
                <> err
                <> "\nsource:\n"
                <> source0
            )
        HSE.ParseOk modu1 ->
          let source1 = HSE.exactPrint modu1 []
           in case HSE.parseFileContentsWithMode mode source1 of
                HSE.ParseFailed loc err ->
                  Left
                    ( context
                        <> ": internal bug: normalized exactPrint failed to parse at "
                        <> show loc
                        <> " with error: "
                        <> err
                        <> "\nsource:\n"
                        <> source1
                    )
                HSE.ParseOk modu2 ->
                  Right
                    Candidate
                      { candAst = modu2,
                        candSource = HSE.exactPrint modu2 []
                      }
