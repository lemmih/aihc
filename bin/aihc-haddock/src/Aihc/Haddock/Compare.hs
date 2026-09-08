{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Compare
-- Description : Compare an aihc-haddock model with mainline Haddock output
--
-- Both sides are converted to one comparison model under one normalization
-- specification, then compared field by field. The report lists every
-- difference with the field it belongs to, the fields the comparison omits,
-- and the names it excluded, so that a passing comparison states exactly
-- what it proved.
--
-- Normalization rules in this version:
--
-- * Identities are compared by module and occurrence name. The package part
--   of a Haddock stable name is dropped, because Haddock reports @main@ for
--   packages documented from source. Module names can be aliased through
--   'normalizationModuleAliases'; a collision between two distinct raw
--   identities fails the comparison.
-- * Generated names (starting with @$@, such as @$fEqShape@ and @$dmname@)
--   are excluded and reported.
-- * Documentation trees are flattened: nested 'DocAppend' nodes become one
--   sequence, adjacent strings merge, empty documents disappear.
--   Identifier links are compared by occurrence name until name resolution is
--   wired in.
-- * @exports@ has set semantics; @visible_exports@ keeps its order.
module Aihc.Haddock.Compare
  ( Identity (..),
    NormalizationConfig (..),
    defaultNormalization,
    ComparisonModule (..),
    comparisonFromModel,
    comparisonFromReference,
    Difference (..),
    Verdict (..),
    Report (..),
    compareInterface,
    compareHoogle,
    renderReport,
    normalizeDoc,
  )
where

import Aihc.Haddock.Model
import Aihc.Haddock.Reference.Hoogle (HoogleEntry (..), HoogleFile (..))
import Aihc.Haddock.Reference.Json
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isUpper)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | A canonical name: module and occurrence name.
data Identity = Identity
  { identityModule :: Text,
    identityName :: Text
  }
  deriving (Eq, Ord, Show)

renderIdentity :: Identity -> Text
renderIdentity (Identity modName name) = modName <> "." <> name

newtype NormalizationConfig = NormalizationConfig
  { -- | Reference module names mapped to the aihc module that provides the
    -- same declarations, for example @GHC.Internal.Read@ to @GHC.Read@.
    normalizationModuleAliases :: Map Text Text
  }

defaultNormalization :: NormalizationConfig
defaultNormalization = NormalizationConfig Map.empty

canonicalModule :: NormalizationConfig -> Text -> Text
canonicalModule config modName = Map.findWithDefault modName modName (normalizationModuleAliases config)

-- | A normalized document: the flattened sequence and the @\@since@ version.
data NormalizedDoc = NormalizedDoc
  { normalizedSince :: Maybe [Int],
    normalizedDoc :: Doc
  }
  deriving (Eq, Show)

data ComparisonModule = ComparisonModule
  { comparisonModuleName :: Text,
    comparisonDocMap :: Map Identity NormalizedDoc,
    comparisonArgMap :: Map Identity (Map Int NormalizedDoc),
    comparisonExports :: Set Identity,
    comparisonVisibleExports :: [Identity],
    comparisonFixities :: Map Identity Fixity,
    -- | The top-level names, whose order in @visible_exports@ is compared.
    -- Subordinate order is not: GHC lists record fields alphabetically.
    comparisonTopLevel :: Set Identity,
    -- | Raw names dropped by normalization, with the reason.
    comparisonExcluded :: [Text],
    -- | Normalization failures, such as identity collisions.
    comparisonErrors :: [Text]
  }
  deriving (Eq, Show)

-- Normalizing documents -----------------------------------------------------

-- | Flatten a document so that only content differences remain.
normalizeDoc :: (Text -> Text) -> Doc -> Doc
normalizeDoc identifierName doc =
  case mergeStrings (flatten doc) of
    [] -> DocEmpty
    [single] -> single
    parts -> foldr1 DocAppend parts
  where
    flatten d =
      case d of
        DocEmpty -> []
        DocAppend a b -> flatten a <> flatten b
        _ -> [descend d]
    descend d =
      case d of
        DocParagraph inner -> DocParagraph (normalize inner)
        DocIdentifier name -> DocIdentifier (identifierName name)
        DocWarning inner -> DocWarning (normalize inner)
        DocEmphasis inner -> DocEmphasis (normalize inner)
        DocMonospaced inner -> DocMonospaced (normalize inner)
        DocBold inner -> DocBold (normalize inner)
        DocUnorderedList items -> DocUnorderedList (map normalize items)
        DocOrderedList items -> DocOrderedList [(n, normalize item) | (n, item) <- items]
        DocDefList items -> DocDefList [(normalize term, normalize def) | (term, def) <- items]
        DocCodeBlock inner -> DocCodeBlock (normalize inner)
        DocHyperlink url label -> DocHyperlink url (normalize <$> label)
        DocHeader level title -> DocHeader level (normalize title)
        DocTable (Table headerRows bodyRows) -> DocTable (Table (map (map normalizeCell) headerRows) (map (map normalizeCell) bodyRows))
        _ -> d
    normalizeCell cell = cell {tableCellContents = normalize (tableCellContents cell)}
    normalize = normalizeDoc identifierName
    mergeStrings parts =
      case parts of
        DocString a : DocString b : rest -> mergeStrings (DocString (a <> b) : rest)
        part : rest -> part : mergeStrings rest
        [] -> []

normalizeMeta :: (Text -> Text) -> MetaDoc -> NormalizedDoc
normalizeMeta identifierName meta = NormalizedDoc (metaSince meta) (normalizeDoc identifierName (metaDoc meta))

-- From the aihc model ------------------------------------------------------------

comparisonFromModel :: NormalizationConfig -> ModuleDoc -> ComparisonModule
comparisonFromModel config modu =
  ComparisonModule
    { comparisonModuleName = modName,
      comparisonDocMap = keepFirst [(identity decl, normalizeMeta id doc) | decl <- typesFirst, Just doc <- [declDoc decl]],
      comparisonArgMap =
        keepFirst
          [ (identity decl, Map.map (normalizeMeta id) (declArgDocs decl))
          | decl <- typesFirst,
            not (Map.null (declArgDocs decl))
          ],
      comparisonExports = Set.fromList exported,
      comparisonVisibleExports = exported,
      comparisonFixities = Map.fromList [(identity decl, fixity) | decl <- allDecls, Just fixity <- [declFixity decl]],
      comparisonTopLevel = Set.fromList (map identity (moduleDocDecls modu)),
      comparisonExcluded = [],
      comparisonErrors = []
    }
  where
    modName = canonicalModule config (moduleDocName modu)
    identity decl = Identity modName (declName decl)
    allDecls = concatMap withSubordinates (moduleDocDecls modu)
    withSubordinates decl = decl : concatMap withSubordinates (declSubordinates decl)
    -- A type and its constructor of the same name share one Haddock stable
    -- name, and Haddock's doc_map keeps the type's documentation.
    typesFirst = filter ((== NamespaceType) . declNamespace) allDecls <> filter ((/= NamespaceType) . declNamespace) allDecls
    keepFirst :: [(Identity, a)] -> Map Identity a
    keepFirst = Map.fromListWith (\_new old -> old)
    exported =
      case moduleDocExports modu of
        Nothing -> map identity allDecls
        Just items -> concatMap exportedIdentities items
    exportedIdentities item =
      case item of
        ExportDeclItem name ns subs ->
          case [decl | decl <- moduleDocDecls modu, declName decl == name, declNamespace decl == ns] of
            decl : _ ->
              identity decl
                : case subs of
                  ExportNoSubordinates -> []
                  ExportAllSubordinates -> map identity (concatMap withSubordinates (declSubordinates decl))
                  ExportSomeSubordinates names ->
                    map identity (concatMap (filter ((`elem` names) . declName) . withSubordinates) (declSubordinates decl))
            [] -> []
        _ -> []

-- From the Haddock reference ---------------------------------------------------

comparisonFromReference :: NormalizationConfig -> ReferenceModule -> ComparisonModule
comparisonFromReference config modu =
  ComparisonModule
    { comparisonModuleName = canonicalModule config (referenceModuleShortName modu),
      comparisonDocMap = keyed (Map.map (normalizeMeta identifierName) (referenceDocMap modu)),
      comparisonArgMap = keyed (Map.map (Map.map (normalizeMeta identifierName)) (referenceArgMap modu)),
      comparisonExports = Set.fromList (mapMaybe canonical (referenceExports modu)),
      comparisonVisibleExports = mapMaybe canonical (referenceVisibleExports modu),
      comparisonFixities = keyed (referenceFixities modu),
      comparisonTopLevel = Set.empty,
      comparisonExcluded = excluded,
      comparisonErrors = collisions
    }
  where
    rawNames =
      Map.keys (referenceDocMap modu)
        <> Map.keys (referenceArgMap modu)
        <> referenceExports modu
        <> referenceVisibleExports modu
        <> Map.keys (referenceFixities modu)
    excluded =
      Set.toList . Set.fromList $
        [ raw <> " (generated name)"
        | raw <- rawNames,
          Just stable <- [parseStableName raw],
          T.isPrefixOf "$" (stableName stable)
        ]
          <> [raw <> " (not a stable name)" | raw <- rawNames, Nothing <- [parseStableName raw]]
    canonical raw = do
      stable <- parseStableName raw
      if T.isPrefixOf "$" (stableName stable)
        then Nothing
        else Just (Identity (canonicalModule config (stableModule stable)) (stableName stable))
    keyed :: Map Text a -> Map Identity a
    keyed = Map.fromList . mapMaybe (\(raw, value) -> (,value) <$> canonical raw) . Map.toList
    collisions =
      [ "normalization collision: " <> T.intercalate ", " raws <> " all map to " <> renderIdentity ident
      | (ident, raws) <- Map.toList (Map.fromListWith (<>) [(ident, [raw]) | raw <- Set.toList (Set.fromList rawNames), Just ident <- [canonical raw]]),
        length raws > 1
      ]
    identifierName name = maybe name stableName (parseStableName name)

-- Comparison ----------------------------------------------------------------------

data Difference = Difference
  { differenceField :: Text,
    differenceKey :: Text,
    differenceExpected :: Text,
    differenceActual :: Text
  }
  deriving (Eq, Show)

data Verdict = Pass | Fail
  deriving (Eq, Show)

data Report = Report
  { reportVerdict :: Verdict,
    reportDifferences :: [Difference],
    -- | What the comparison did not cover.
    reportNotes :: [Text]
  }
  deriving (Eq, Show)

mkReport :: [Difference] -> [Text] -> Report
mkReport differences =
  Report (if null differences then Pass else Fail) differences

-- | Compare the modules of an aihc model with a Haddock reference interface.
compareInterface :: NormalizationConfig -> PackageDoc -> ReferenceInterface -> Report
compareInterface config package reference =
  mkReport (moduleDifferences <> concatMap (uncurry compareModules) paired) notes
  where
    ours = Map.fromList [(comparisonModuleName c, c) | modu <- packageDocModules package, moduleDocExposed modu, let c = comparisonFromModel config modu]
    theirs = Map.fromList [(comparisonModuleName c, c) | modu <- referenceModules reference, let c = comparisonFromReference config modu]
    paired = [(theirs Map.! name, ours Map.! name) | name <- Map.keys (Map.intersection theirs ours)]
    moduleDifferences =
      [Difference "modules" name "module present" "module missing" | name <- Map.keys (Map.difference theirs ours)]
        <> [Difference "modules" name "module absent" "module present" | name <- Map.keys (Map.difference ours theirs)]
    notes =
      [ "omitted fields: package_info, link_env, is_sig, info, options (environment metadata, not documentation content)",
        "visible_exports order compared for top-level names only; GHC orders record fields alphabetically",
        "a type and a constructor with the same name share one stable name; the type's documentation is compared",
        "identifier links compared by occurrence name; module aliases: "
          <> (if Map.null (normalizationModuleAliases config) then "none" else T.intercalate ", " [k <> " -> " <> v | (k, v) <- Map.toList (normalizationModuleAliases config)])
      ]
        <> ["excluded from " <> comparisonModuleName c <> ": " <> raw | c <- Map.elems theirs, raw <- comparisonExcluded c]
        <> concatMap comparisonErrors (Map.elems theirs)

compareModules :: ComparisonModule -> ComparisonModule -> [Difference]
compareModules expected actual =
  [Difference "normalization" (comparisonModuleName expected) "" err | err <- comparisonErrors expected <> comparisonErrors actual]
    <> compareMaps "doc_map" renderDoc (comparisonDocMap expected) (comparisonDocMap actual)
    <> compareMaps "arg_map" renderArgs (comparisonArgMap expected) (comparisonArgMap actual)
    <> compareSets "exports" (comparisonExports expected) (comparisonExports actual)
    <> compareOrdered "visible_exports" (comparisonVisibleExports expected) (comparisonVisibleExports actual)
    <> compareOrdered "visible_exports (top-level order)" (topLevelOnly (comparisonVisibleExports expected)) (topLevelOnly (comparisonVisibleExports actual))
    <> compareMaps "fix_map" renderFixity (comparisonFixities expected) (comparisonFixities actual)
  where
    prefix field = field <> "@" <> comparisonModuleName expected
    compareMaps :: (Eq a) => Text -> (a -> Text) -> Map Identity a -> Map Identity a -> [Difference]
    compareMaps field render left right =
      [Difference (prefix field) (renderIdentity k) (render v) "missing" | (k, v) <- Map.toList (Map.difference left right)]
        <> [Difference (prefix field) (renderIdentity k) "missing" (render v) | (k, v) <- Map.toList (Map.difference right left)]
        <> [ Difference (prefix field) (renderIdentity k) (render l) (render r)
           | (k, (l, r)) <- Map.toList (Map.intersectionWith (,) left right),
             l /= r
           ]
    compareSets field left right =
      [Difference (prefix field) (renderIdentity k) "exported" "missing" | k <- Set.toList (Set.difference left right)]
        <> [Difference (prefix field) (renderIdentity k) "missing" "exported" | k <- Set.toList (Set.difference right left)]
    topLevel = Set.union (comparisonTopLevel expected) (comparisonTopLevel actual)
    topLevelOnly = filter (`Set.member` topLevel)
    compareOrdered field left right
      | Set.fromList left /= Set.fromList right = compareSets field (Set.fromList left) (Set.fromList right)
      | left == right || field == "visible_exports" = []
      | otherwise = [Difference (prefix field) "order" (T.unwords (map renderIdentity left)) (T.unwords (map renderIdentity right))]
    renderDoc (NormalizedDoc since doc) =
      maybe "" (\v -> "@since " <> T.pack (show v) <> " ") since <> TE.decodeUtf8 (BL.toStrict (Aeson.encode doc))
    renderArgs args = T.intercalate "; " [T.pack (show n) <> ": " <> renderDoc d | (n, d) <- Map.toList args]
    renderFixity (Fixity prec direction) = T.pack (show direction) <> " " <> T.pack (show prec)

-- Hoogle -----------------------------------------------------------------------------

-- | Compare two parsed Hoogle files. Declarations are compared after
-- collapsing whitespace and dropping module qualifiers from instance heads,
-- because Haddock qualifies instance heads with the defining module while
-- aihc-haddock does not resolve names yet. Documentation is compared as
-- re-flowed text.
compareHoogle :: HoogleFile -> HoogleFile -> Report
compareHoogle expected actual =
  mkReport differences notes
  where
    entries file = Map.fromListWith (\_ first -> first) [(normalizeDecl (hoogleEntryDecl e), normalizeDocLines (hoogleEntryDoc e)) | e <- hoogleEntries file]
    left = entries expected
    right = entries actual
    differences =
      [Difference "hoogle.package" "" (fromMaybe "" (hooglePackage expected)) (fromMaybe "" (hooglePackage actual)) | hooglePackage expected /= hooglePackage actual]
        <> [Difference "hoogle.version" "" (fromMaybe "" (hoogleVersion expected)) (fromMaybe "" (hoogleVersion actual)) | hoogleVersion expected /= hoogleVersion actual]
        <> [Difference "hoogle.decl" decl "present" "missing" | decl <- Map.keys (Map.difference left right)]
        <> [Difference "hoogle.decl" decl "missing" "present" | decl <- Map.keys (Map.difference right left)]
        <> [Difference "hoogle.doc" decl l r | (decl, (l, r)) <- Map.toList (Map.intersectionWith (,) left right), l /= r]
        <> orderDifference
    orderDifference =
      let common = Set.fromList (Map.keys (Map.intersection left right))
          order file = filter (`Set.member` common) (map (normalizeDecl . hoogleEntryDecl) (hoogleEntries file))
       in [Difference "hoogle.order" "" (T.intercalate " | " (order expected)) (T.intercalate " | " (order actual)) | order expected /= order actual]
    notes =
      [ "instance heads compared without module qualifiers",
        "documentation compared as re-flowed text; line wrapping is not significant"
      ]

normalizeDocLines :: [Text] -> Text
normalizeDocLines = T.unwords . T.words . T.unlines

normalizeDecl :: Text -> Text
normalizeDecl decl
  | T.isPrefixOf "instance " collapsed = T.unwords (map stripQualifier (T.words collapsed))
  | otherwise = collapsed
  where
    collapsed = T.unwords (T.words decl)

-- | @GHC.Classes.Eq@ becomes @Eq@; @(Data.Colour.RGB.RGB@ keeps its paren.
stripQualifier :: Text -> Text
stripQualifier word =
  let (open, rest) = T.span (`elem` ("([" :: String)) word
      segments = T.splitOn "." rest
      isModuleSegment segment =
        case T.uncons segment of
          Just (c, more) -> isUpper c && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') more
          Nothing -> False
      qualifiers = takeWhile isModuleSegment (init' segments)
      unqualified = T.intercalate "." (drop (length qualifiers) segments)
   in if null qualifiers then word else open <> unqualified
  where
    init' xs = if null xs then [] else init xs

renderReport :: Report -> Text
renderReport report =
  T.unlines $
    [verdictLine]
      <> concatMap renderDifference (sortOn (\d -> (differenceField d, differenceKey d)) (reportDifferences report))
      <> map ("note: " <>) (reportNotes report)
  where
    verdictLine =
      case reportVerdict report of
        Pass -> "PASS"
        Fail -> "FAIL " <> T.pack (show (length (reportDifferences report))) <> " differences"
    renderDifference d =
      [ differenceField d <> " " <> differenceKey d,
        "  expected: " <> differenceExpected d,
        "  actual:   " <> differenceActual d
      ]
