{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Model
-- Description : The versioned machine-readable documentation model
--
-- The model is what @aihc-haddock@ writes out and what the comparison harness
-- reads back. It is independent of System FC and GRIN and records only what
-- documentation needs: declaration identities, namespaces, rendered
-- signatures, attached documentation, argument positions, export order,
-- sections, fixities, instances, warnings and source locations.
--
-- 'Doc' mirrors mainline Haddock's @DocH@ and its JSON encoding uses the same
-- @tag@ names and field names as Haddock's @--show-interface@ output, so the
-- same decoder reads both an @aihc-haddock@ model and a Haddock reference.
module Aihc.Haddock.Model
  ( docModelFormatVersion,
    Doc (..),
    Example (..),
    TableCell (..),
    Table (..),
    MetaDoc (..),
    plainMetaDoc,
    Namespace (..),
    DeclKind (..),
    Fixity (..),
    FixityDirection (..),
    Location (..),
    locationFromSpan,
    DeclDoc (..),
    ExportSubordinates (..),
    ExportItem (..),
    InstanceDoc (..),
    ModuleInfo (..),
    emptyModuleInfo,
    ModuleDoc (..),
    PackageDoc (..),
    encodePackageDoc,
    decodePackageDoc,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Bump when the JSON layout of 'PackageDoc' changes. Cached artifacts are
-- keyed by this version, so a bump invalidates every cached package.
docModelFormatVersion :: Int
docModelFormatVersion = 1

-- | Documentation markup. Constructors and their JSON tags follow Haddock's
-- @DocH@.
data Doc
  = DocEmpty
  | DocAppend Doc Doc
  | DocString Text
  | DocParagraph Doc
  | -- | An identifier written as @'name'@. The text is the name as written;
    -- resolution against the module scope is a later step.
    DocIdentifier Text
  | -- | A qualified identifier Haddock could not check, keyed by module name
    -- and the occurrence name.
    DocIdentifierUnchecked Text Text
  | DocModule Text
  | DocWarning Doc
  | DocEmphasis Doc
  | DocMonospaced Doc
  | DocBold Doc
  | DocUnorderedList [Doc]
  | DocOrderedList [(Int, Doc)]
  | DocDefList [(Doc, Doc)]
  | DocCodeBlock Doc
  | DocHyperlink Text (Maybe Doc)
  | DocPic Text (Maybe Text)
  | DocMathInline Text
  | DocMathDisplay Text
  | DocAName Text
  | DocProperty Text
  | DocExamples [Example]
  | DocHeader Int Doc
  | DocTable Table
  deriving (Eq, Show, Generic)

data Example = Example
  { exampleExpression :: Text,
    exampleResult :: [Text]
  }
  deriving (Eq, Show, Generic)

data TableCell = TableCell
  { tableCellColspan :: Int,
    tableCellRowspan :: Int,
    tableCellContents :: Doc
  }
  deriving (Eq, Show, Generic)

data Table = Table
  { tableHeaderRows :: [[TableCell]],
    tableBodyRows :: [[TableCell]]
  }
  deriving (Eq, Show, Generic)

-- | Documentation with its @\@since@ version, as Haddock's @MetaDoc@.
data MetaDoc = MetaDoc
  { metaSince :: Maybe [Int],
    metaDoc :: Doc
  }
  deriving (Eq, Show, Generic)

plainMetaDoc :: Doc -> MetaDoc
plainMetaDoc = MetaDoc Nothing

data Namespace
  = NamespaceType
  | NamespaceValue
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

data DeclKind
  = DeclKindData
  | DeclKindNewtype
  | DeclKindTypeSynonym
  | DeclKindClass
  | DeclKindTypeFamily
  | DeclKindDataFamily
  | DeclKindFunction
  | DeclKindPatternSynonym
  | DeclKindForeignImport
  | DeclKindConstructor
  | DeclKindField
  | DeclKindMethod
  | DeclKindAssociatedType
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

data FixityDirection
  = FixityInfix
  | FixityInfixL
  | FixityInfixR
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

data Fixity = Fixity
  { fixityPrecedence :: Int,
    fixityDirection :: FixityDirection
  }
  deriving (Eq, Ord, Show, Generic)

-- | A source location relative to the package root.
data Location = Location
  { locationFile :: FilePath,
    locationStartLine :: Int,
    locationStartColumn :: Int,
    locationEndLine :: Int,
    locationEndColumn :: Int
  }
  deriving (Eq, Ord, Show, Generic)

locationFromSpan :: (FilePath -> FilePath) -> SourceSpan -> Maybe Location
locationFromSpan relativize sp =
  case sp of
    NoSourceSpan -> Nothing
    SourceSpan file startLine startCol endLine endCol _ _ ->
      Just (Location (relativize file) startLine startCol endLine endCol)

-- | One documented declaration or subordinate (constructor, field, method).
data DeclDoc = DeclDoc
  { declName :: Text,
    declNamespace :: Namespace,
    declKind :: DeclKind,
    -- | The rendered signature or head, for example @Shape -> Double@ for a
    -- function or @Shape a@ for a data type. Missing when the source has no
    -- signature and type inference is not available.
    declSignature :: Maybe Text,
    declDoc :: Maybe MetaDoc,
    -- | Argument documentation by argument position, the result being the
    -- position after the last argument.
    declArgDocs :: Map Int MetaDoc,
    declSubordinates :: [DeclDoc],
    declFixity :: Maybe Fixity,
    declWarning :: Maybe Text,
    declLocation :: Maybe Location
  }
  deriving (Eq, Show, Generic)

data ExportSubordinates
  = ExportNoSubordinates
  | ExportAllSubordinates
  | ExportSomeSubordinates [Text]
  deriving (Eq, Show, Generic)

-- | One item of the export list in source order. Sections and documentation
-- chunks appear between the declarations they precede.
data ExportItem
  = ExportSectionItem Int Doc
  | -- | A documentation chunk. Named chunks record the @$name@ they came from.
    ExportDocItem (Maybe Text) Doc
  | ExportDeclItem Text Namespace ExportSubordinates
  | ExportModuleItem Text
  deriving (Eq, Show, Generic)

data InstanceDoc = InstanceDoc
  { instanceClass :: Text,
    -- | The rendered instance head including its context, for example
    -- @Eq a => Eq (Shape a)@.
    instanceHead :: Text,
    instanceDoc :: Maybe MetaDoc,
    instanceOverlap :: Maybe Text,
    -- | Instances generated from a @deriving@ clause.
    instanceDerived :: Bool,
    instanceLocation :: Maybe Location
  }
  deriving (Eq, Show, Generic)

-- | The module header fields Haddock reads from the leading comment.
data ModuleInfo = ModuleInfo
  { moduleInfoDescription :: Maybe Doc,
    moduleInfoCopyright :: Maybe Text,
    moduleInfoLicense :: Maybe Text,
    moduleInfoMaintainer :: Maybe Text,
    moduleInfoStability :: Maybe Text,
    moduleInfoPortability :: Maybe Text
  }
  deriving (Eq, Show, Generic)

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo Nothing Nothing Nothing Nothing Nothing Nothing

data ModuleDoc = ModuleDoc
  { moduleDocName :: Text,
    moduleDocExposed :: Bool,
    moduleDocFile :: FilePath,
    moduleDocDescription :: Maybe MetaDoc,
    moduleDocInfo :: ModuleInfo,
    moduleDocWarning :: Maybe Text,
    -- | 'Nothing' when the module has no export list.
    moduleDocExports :: Maybe [ExportItem],
    -- | Every top-level declaration in source order, exported or not.
    moduleDocDecls :: [DeclDoc],
    moduleDocInstances :: [InstanceDoc],
    -- | Documentation chunks declared as @-- $name@.
    moduleDocNamedChunks :: Map Text Doc,
    moduleDocExtensions :: [Text],
    -- | Problems found while building the model, such as documentation
    -- comments that attach to nothing.
    moduleDocDiagnostics :: [Text]
  }
  deriving (Eq, Show, Generic)

data PackageDoc = PackageDoc
  { packageDocFormatVersion :: Int,
    packageDocName :: Text,
    packageDocVersion :: Text,
    packageDocDependencies :: [Text],
    packageDocModules :: [ModuleDoc]
  }
  deriving (Eq, Show, Generic)

-- Doc JSON, following Haddock.Interface.Json --------------------------------

instance ToJSON Doc where
  toJSON doc =
    case doc of
      DocEmpty -> tagged "DocEmpty" []
      DocAppend first second -> tagged "DocAppend" ["first" .= first, "second" .= second]
      DocString text -> tagged "DocString" ["string" .= text]
      DocParagraph inner -> tagged "DocParagraph" ["document" .= inner]
      DocIdentifier name -> tagged "DocIdentifier" ["name" .= name]
      DocIdentifierUnchecked modName name -> tagged "DocIdentifierUnchecked" ["modName" .= modName, "name" .= name]
      DocModule name -> tagged "DocModule" ["string" .= name]
      DocWarning inner -> tagged "DocWarning" ["document" .= inner]
      DocEmphasis inner -> tagged "DocEmphasis" ["document" .= inner]
      DocMonospaced inner -> tagged "DocMonospaced" ["document" .= inner]
      DocBold inner -> tagged "DocBold" ["document" .= inner]
      DocUnorderedList items -> tagged "DocUnorderedList" ["documents" .= items]
      DocOrderedList items ->
        tagged "DocOrderedList" ["items" .= [object ["seq" .= n, "document" .= item] | (n, item) <- items]]
      DocDefList items ->
        tagged "DocDefList" ["definitions" .= [object ["document" .= term, "y" .= def] | (term, def) <- items]]
      DocCodeBlock inner -> tagged "DocCodeBlock" ["document" .= inner]
      DocHyperlink url label ->
        tagged "DocHyperlink" ["hyperlink" .= object ["hyperlinkUrl" .= url, "hyperlinkLabel" .= label]]
      DocPic url label ->
        tagged "DocPic" ["picture" .= object ["pictureUrl" .= url, "pictureLabel" .= label]]
      DocMathInline text -> tagged "DocMathInline" ["string" .= text]
      DocMathDisplay text -> tagged "DocMathDisplay" ["string" .= text]
      DocAName text -> tagged "DocAName" ["string" .= text]
      DocProperty text -> tagged "DocProperty" ["string" .= text]
      DocExamples examples -> tagged "DocExamples" ["examples" .= examples]
      DocHeader level title ->
        tagged "DocHeader" ["header" .= object ["headerLevel" .= level, "headerTitle" .= title]]
      DocTable table -> tagged "DocTable" ["table" .= table]
    where
      tagged :: Text -> [Pair] -> Aeson.Value
      tagged tag fields = object (("tag" .= tag) : fields)

instance FromJSON Doc where
  parseJSON = withObject "Doc" $ \obj -> do
    tag <- obj .: "tag"
    case (tag :: Text) of
      "DocEmpty" -> pure DocEmpty
      "DocAppend" -> DocAppend <$> obj .: "first" <*> obj .: "second"
      "DocString" -> DocString <$> obj .: "string"
      "DocParagraph" -> DocParagraph <$> obj .: "document"
      "DocIdentifier" -> DocIdentifier <$> obj .: "name"
      "DocIdentifierUnchecked" -> DocIdentifierUnchecked <$> obj .: "modName" <*> obj .:? "name" .!= ""
      "DocModule" -> DocModule <$> obj .: "string"
      "DocWarning" -> DocWarning <$> obj .: "document"
      "DocEmphasis" -> DocEmphasis <$> obj .: "document"
      "DocMonospaced" -> DocMonospaced <$> obj .: "document"
      "DocBold" -> DocBold <$> obj .: "document"
      "DocUnorderedList" -> DocUnorderedList <$> obj .: "documents"
      "DocOrderedList" -> do
        items <- obj .: "items"
        DocOrderedList <$> mapM (withObject "ordered item" (\item -> (,) <$> item .: "seq" <*> item .: "document")) items
      "DocDefList" -> do
        items <- obj .: "definitions"
        DocDefList <$> mapM (withObject "definition" (\item -> (,) <$> item .: "document" <*> item .: "y")) items
      "DocCodeBlock" -> DocCodeBlock <$> obj .: "document"
      "DocHyperlink" -> do
        link <- obj .: "hyperlink"
        DocHyperlink <$> link .: "hyperlinkUrl" <*> link .:? "hyperlinkLabel"
      "DocPic" -> do
        picture <- obj .: "picture"
        DocPic <$> picture .: "pictureUrl" <*> picture .:? "pictureLabel"
      "DocMathInline" -> DocMathInline <$> obj .: "string"
      "DocMathDisplay" -> DocMathDisplay <$> obj .: "string"
      "DocAName" -> DocAName <$> obj .: "string"
      "DocProperty" -> DocProperty <$> obj .: "string"
      "DocExamples" -> DocExamples <$> obj .: "examples"
      "DocHeader" -> do
        header <- obj .: "header"
        DocHeader <$> header .: "headerLevel" <*> header .: "headerTitle"
      "DocTable" -> DocTable <$> obj .: "table"
      other -> fail ("unknown documentation tag " <> T.unpack other)

instance ToJSON Example where
  toJSON (Example expression result) =
    object ["exampleExpression" .= expression, "exampleResult" .= result]

instance FromJSON Example where
  parseJSON = withObject "Example" $ \obj ->
    Example <$> obj .: "exampleExpression" <*> obj .: "exampleResult"

instance ToJSON TableCell where
  toJSON (TableCell colspan rowspan contents) =
    object ["tableCellColspan" .= colspan, "tableCellRowspan" .= rowspan, "tableCellContents" .= contents]

instance FromJSON TableCell where
  parseJSON = withObject "TableCell" $ \obj ->
    TableCell <$> obj .: "tableCellColspan" <*> obj .: "tableCellRowspan" <*> obj .: "tableCellContents"

instance ToJSON Table where
  toJSON (Table headerRows bodyRows) =
    object ["tableHeaderRows" .= headerRows, "tableBodyRows" .= bodyRows]

instance FromJSON Table where
  parseJSON = withObject "Table" $ \obj ->
    Table <$> obj .: "tableHeaderRows" <*> obj .: "tableBodyRows"

-- | Haddock encodes the @\@since@ version as the 'show' of an @[Int]@.
instance ToJSON MetaDoc where
  toJSON (MetaDoc since doc) =
    object
      [ "meta" .= object ["version" .= fmap (T.pack . show) since],
        "document" .= doc
      ]

instance FromJSON MetaDoc where
  parseJSON = withObject "MetaDoc" $ \obj -> do
    meta <- obj .:? "meta"
    version <- maybe (pure Nothing) (.:? "version") meta
    since <- traverse parseVersion version
    MetaDoc since <$> obj .: "document"
    where
      parseVersion :: Aeson.Value -> Parser [Int]
      parseVersion value =
        case value of
          Aeson.String text ->
            case reads (T.unpack text) of
              [(numbers, "")] -> pure numbers
              _ -> fail ("unreadable @since version " <> T.unpack text)
          _ -> parseJSON value

-- Model JSON -----------------------------------------------------------------

modelOptions :: String -> Aeson.Options
modelOptions prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . dropPrefix prefix,
      Aeson.constructorTagModifier = Aeson.camelTo2 '_' . dropPrefix prefix,
      Aeson.omitNothingFields = False,
      Aeson.sumEncoding = Aeson.TaggedObject "tag" "contents"
    }
  where
    dropPrefix p name = fromMaybe name (stripPrefixString p name)
    stripPrefixString [] rest = Just rest
    stripPrefixString (c : cs) (d : ds) | c == d = stripPrefixString cs ds
    stripPrefixString _ _ = Nothing

instance ToJSON Namespace where
  toJSON = Aeson.genericToJSON (modelOptions "Namespace")

instance FromJSON Namespace where
  parseJSON = Aeson.genericParseJSON (modelOptions "Namespace")

instance ToJSON DeclKind where
  toJSON = Aeson.genericToJSON (modelOptions "DeclKind")

instance FromJSON DeclKind where
  parseJSON = Aeson.genericParseJSON (modelOptions "DeclKind")

instance ToJSON FixityDirection where
  toJSON direction =
    Aeson.String $ case direction of
      FixityInfix -> "infix"
      FixityInfixL -> "infixl"
      FixityInfixR -> "infixr"

instance FromJSON FixityDirection where
  parseJSON = Aeson.withText "FixityDirection" $ \case
    "infix" -> pure FixityInfix
    "infixl" -> pure FixityInfixL
    "infixr" -> pure FixityInfixR
    other -> fail ("unknown fixity direction " <> T.unpack other)

-- | Haddock's @fix_map@ entries are @{"prec": n, "direction": "infixl"}@.
instance ToJSON Fixity where
  toJSON (Fixity prec direction) = object ["prec" .= prec, "direction" .= direction]

instance FromJSON Fixity where
  parseJSON = withObject "Fixity" $ \obj -> Fixity <$> obj .: "prec" <*> obj .: "direction"

instance ToJSON Location where
  toJSON = Aeson.genericToJSON (modelOptions "location")

instance FromJSON Location where
  parseJSON = Aeson.genericParseJSON (modelOptions "location")

instance ToJSON DeclDoc where
  toJSON = Aeson.genericToJSON (modelOptions "decl")

instance FromJSON DeclDoc where
  parseJSON = Aeson.genericParseJSON (modelOptions "decl")

instance ToJSON ExportSubordinates where
  toJSON = Aeson.genericToJSON (modelOptions "Export")

instance FromJSON ExportSubordinates where
  parseJSON = Aeson.genericParseJSON (modelOptions "Export")

instance ToJSON ExportItem where
  toJSON = Aeson.genericToJSON (modelOptions "Export")

instance FromJSON ExportItem where
  parseJSON = Aeson.genericParseJSON (modelOptions "Export")

instance ToJSON InstanceDoc where
  toJSON = Aeson.genericToJSON (modelOptions "instance")

instance FromJSON InstanceDoc where
  parseJSON = Aeson.genericParseJSON (modelOptions "instance")

instance ToJSON ModuleInfo where
  toJSON = Aeson.genericToJSON (modelOptions "moduleInfo")

instance FromJSON ModuleInfo where
  parseJSON = Aeson.genericParseJSON (modelOptions "moduleInfo")

instance ToJSON ModuleDoc where
  toJSON = Aeson.genericToJSON (modelOptions "moduleDoc")

instance FromJSON ModuleDoc where
  parseJSON = Aeson.genericParseJSON (modelOptions "moduleDoc")

instance ToJSON PackageDoc where
  toJSON = Aeson.genericToJSON (modelOptions "packageDoc")

instance FromJSON PackageDoc where
  parseJSON = Aeson.genericParseJSON (modelOptions "packageDoc")

-- | Pretty JSON with sorted keys, so the output is stable across runs and
-- readable in fixtures.
encodePackageDoc :: PackageDoc -> BL.ByteString
encodePackageDoc =
  AesonPretty.encodePretty' AesonPretty.defConfig {AesonPretty.confCompare = compare, AesonPretty.confIndent = AesonPretty.Spaces 2}

decodePackageDoc :: BL.ByteString -> Either String PackageDoc
decodePackageDoc bytes = do
  doc <- Aeson.eitherDecode bytes
  if packageDocFormatVersion doc == docModelFormatVersion
    then Right doc
    else
      Left
        ( "documentation model format "
            <> show (packageDocFormatVersion doc)
            <> " is not the supported format "
            <> show docModelFormatVersion
        )
