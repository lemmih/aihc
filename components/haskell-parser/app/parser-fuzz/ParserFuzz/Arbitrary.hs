module ParserFuzz.Arbitrary
  ( generateCandidate,
    normalizeCandidateAst,
    qcGenStream,
  )
where

import Data.List (intercalate)
import Language.Haskell.Exts qualified as HSE
import Language.Haskell.Exts.Build qualified as HSEB
import ParserFuzz.Arbitrary.Helpers (eThingWith, moduleNameNode, noNamespace, noWildcard)
import ParserFuzz.Types (Candidate (..))
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (QCGen, mkQCGen)

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
          let header = HSE.ModuleHead () (moduleNameNode moduName) Nothing exports
          pure (GenModuleHead (Just header))
      ]

-- TODO: Add generators for module pragmas/imports/declarations.
-- TODO: Extend generator to cover additional Haskell syntax supported by HSE.

generateCandidate :: Int -> QCGen -> Candidate
generateCandidate size seed =
  let generated = unGen (arbitrary :: Gen GenModule) seed size
   in materializeCandidate generated

materializeCandidate :: GenModule -> Candidate
materializeCandidate (GenModule modu0) =
  case normalizeCandidateSource "materializeCandidate" (HSE.prettyPrint modu0) of
    Left msg -> error msg
    Right candidate -> candidate

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<generated>",
      HSE.extensions = HSE.glasgowExts
    }

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
      (3, HSE.EAbs () noNamespace <$> genTypeQName),
      (3, genEThingWith),
      (2, HSE.EModuleContents () <$> genModuleNameNode)
    ]

genEThingWith :: Gen (HSE.ExportSpec ())
genEThingWith = do
  qtycon <- genTypeQName
  n <- chooseInt (0, 3)
  cnames <- vectorOf n genCName
  pure (eThingWith noWildcard qtycon cnames)

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
genModuleNameNode = moduleNameNode <$> genModuleName

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

qcGenStream :: Int -> [QCGen]
qcGenStream seed = map mkQCGen [seed ..]
