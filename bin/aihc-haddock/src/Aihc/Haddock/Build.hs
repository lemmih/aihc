{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Build
-- Description : Build the documentation model of one module
--
-- Walks the parsed module, attaches documentation comments by position (see
-- "Aihc.Haddock.Comment") and renders the declaration heads and signatures.
-- Parents claim @-- |@ comments before their children and @-- ^@ comments
-- after them, which gives Haddock's outermost-next, innermost-previous rule.
--
-- Name resolution and inferred types are not wired in yet. Functions without a
-- signature get no signature and a diagnostic.
module Aihc.Haddock.Build
  ( BuildInput (..),
    buildModuleDoc,
  )
where

import Aihc.Haddock.Comment
import Aihc.Haddock.Markup (parseDocText, parseInline, parseMetaDocText)
import Aihc.Haddock.Model
import Aihc.Haddock.Render
import Aihc.Parser.Syntax
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

data BuildInput = BuildInput
  { -- | The path the module was parsed from, as it appears in its spans.
    buildFile :: FilePath,
    -- | Turns a span's file name into the path recorded in the model.
    buildRelativize :: FilePath -> FilePath,
    buildModule :: Module,
    -- | The source text the parser saw, after preprocessing.
    buildSource :: Text,
    buildExtensions :: [Extension],
    buildExposed :: Bool,
    -- | Module name to use when the source has no header.
    buildFallbackName :: Text,
    -- | Diagnostics from earlier stages, such as parse errors.
    buildDiagnostics :: [Text]
  }

data BuildState = BuildState
  { stIndex :: CommentIndex,
    stInstances :: [InstanceDoc],
    stFixities :: Map.Map Text Fixity,
    stWarnings :: Map.Map Text Text,
    stDiagnostics :: [Text],
    stRelativize :: FilePath -> FilePath
  }

type Build = State BuildState

buildModuleDoc :: BuildInput -> ModuleDoc
buildModuleDoc input =
  ModuleDoc
    { moduleDocName = name,
      moduleDocExposed = buildExposed input,
      moduleDocFile = buildRelativize input (buildFile input),
      moduleDocDescription = description,
      moduleDocInfo = info,
      moduleDocWarning = moduleWarningPragma modu >>= pragmaMessage,
      moduleDocExports = exports,
      moduleDocDecls = decls,
      moduleDocInstances = reverse (stInstances finalState),
      moduleDocNamedChunks = chunks,
      moduleDocExtensions = map (T.pack . show) (buildExtensions input),
      moduleDocDiagnostics = buildDiagnostics input <> reverse (stDiagnostics finalState) <> unattached
    }
  where
    modu = buildModule input
    name = fromMaybe (buildFallbackName input) (moduleName modu)
    comments = collectDocComments (buildFile input) (buildExtensions input) (buildSource input)
    initialState =
      BuildState
        { stIndex = buildCommentIndex comments,
          stInstances = [],
          stFixities = Map.empty,
          stWarnings = Map.empty,
          stDiagnostics = [],
          stRelativize = buildRelativize input
        }
    ((description, info, exports, decls, chunks), finalState) = runState build initialState
    unattached =
      [ "unattached documentation comment at " <> renderSpanStart (docCommentSpan c)
      | c <- unclaimedComments (stIndex finalState)
      ]

    build = do
      let headSpan = moduleHead modu >>= listToMaybe . mapMaybe annotationSpan . moduleHeadAnns
      headerComments <- claimNextRaw headSpan
      let (info', description') = moduleHeader headerComments
      exportItems <- traverse (buildExports headSpan) (moduleExports modu)
      declDocs <- concat <$> mapM buildTopDecl (moduleDecls modu)
      chunkComments <- gets (namedChunkComments . stIndex)
      let chunks' = Map.fromList [(chunkName, parseDocText (docCommentText c)) | (chunkName, c) <- chunkComments]
      exportItems' <- traverse (mapM (resolveChunk chunks')) exportItems
      fixities <- gets stFixities
      warnings <- gets stWarnings
      let decls' = map (applyFixities fixities . applyWarnings warnings) (mergeValueDecls declDocs)
      forM_ decls' $ \decl ->
        case (declKind decl, declSignature decl) of
          (DeclKindFunction, Nothing) ->
            diagnostic ("no type signature for " <> declName decl <> "; inferred types need the type checker")
          _ -> pure ()
      pure (description', info', exportItems', decls', chunks')

    resolveChunk chunks' item =
      case item of
        ExportDocItem (Just chunkName) DocEmpty ->
          case Map.lookup chunkName chunks' of
            Just doc -> pure (ExportDocItem (Just chunkName) doc)
            Nothing -> do
              diagnostic ("export list refers to unknown documentation chunk $" <> chunkName)
              pure item
        _ -> pure item

-- Comment claiming -------------------------------------------------------------

claimNextRaw :: Maybe SourceSpan -> Build [DocComment]
claimNextRaw msp =
  case msp >>= spanOffsets of
    Nothing -> pure []
    Just (start, _) -> do
      index <- gets stIndex
      let (claimed, index') = claimNextAt start index
      modify' (\st -> st {stIndex = index'})
      pure claimed

claimNext :: Maybe SourceSpan -> Build (Maybe MetaDoc)
claimNext msp = commentsDoc <$> claimNextRaw msp

claimPrev :: Int -> Maybe SourceSpan -> Build (Maybe MetaDoc)
claimPrev parentColumn msp =
  case msp of
    Nothing -> pure Nothing
    Just sp -> do
      index <- gets stIndex
      let (claimed, index') = claimPrevWithin parentColumn sp index
      modify' (\st -> st {stIndex = index'})
      pure (commentsDoc claimed)

-- | Drop the @-- ^@ comments GHC would reject at this node, with a diagnostic.
discardPrev :: Int -> Maybe SourceSpan -> Text -> Build ()
discardPrev parentColumn msp reason = do
  dropped <- claimPrev parentColumn msp
  case dropped of
    Nothing -> pure ()
    Just _ -> diagnostic (reason <> " (" <> maybe "<unknown>" renderSpanStart msp <> ")")

-- | Documentation of a node: the @-- |@ comments before it, then the children,
-- then the @-- ^@ comments after it. A node with both gets both, in that
-- order, as GHC produces for a declaration followed by a column-one @-- ^@.
documented :: Int -> Maybe SourceSpan -> Build a -> Build (Maybe MetaDoc, a)
documented parentColumn msp children = do
  next <- claimNext msp
  result <- children
  prev <- claimPrev parentColumn msp
  pure (mergeDocs next prev, result)

commentsDoc :: [DocComment] -> Maybe MetaDoc
commentsDoc comments =
  case comments of
    [] -> Nothing
    _ -> Just (parseMetaDocText (T.intercalate "\n\n" (map docCommentText comments)))

mergeDocs :: Maybe MetaDoc -> Maybe MetaDoc -> Maybe MetaDoc
mergeDocs first second =
  case (first, second) of
    (Nothing, other) -> other
    (other, Nothing) -> other
    (Just a, Just b) -> Just (MetaDoc (metaSince a <|> metaSince b) (appendDoc (metaDoc a) (metaDoc b)))

appendDoc :: Doc -> Doc -> Doc
appendDoc a b =
  case (a, b) of
    (DocEmpty, other) -> other
    (other, DocEmpty) -> other
    _ -> DocAppend a b

diagnostic :: Text -> Build ()
diagnostic message = modify' (\st -> st {stDiagnostics = message : stDiagnostics st})

location :: Maybe SourceSpan -> Build (Maybe Location)
location msp = do
  relativize <- gets stRelativize
  pure (msp >>= locationFromSpan relativize)

renderSpanStart :: SourceSpan -> Text
renderSpanStart sp =
  case sp of
    SourceSpan {sourceSpanStartLine = line, sourceSpanStartCol = col} ->
      T.pack (show line) <> ":" <> T.pack (show col)
    NoSourceSpan -> "<unknown>"

-- Module header ----------------------------------------------------------------

-- | Split the module comment into Haddock's header fields and the description.
moduleHeader :: [DocComment] -> (ModuleInfo, Maybe MetaDoc)
moduleHeader comments =
  case comments of
    [] -> (emptyModuleInfo, Nothing)
    _ ->
      let text = T.intercalate "\n" (map docCommentText comments)
          (fields, bodyLines) = takeFields (dropWhile (T.all isSpace) (T.lines text))
          body = T.strip (T.unlines bodyLines)
          field key = listToMaybe [value | (k, value) <- fields, k == key]
          info =
            ModuleInfo
              { moduleInfoDescription = parseInline <$> field "Description",
                moduleInfoCopyright = field "Copyright",
                moduleInfoLicense = field "License",
                moduleInfoMaintainer = field "Maintainer",
                moduleInfoStability = field "Stability",
                moduleInfoPortability = field "Portability"
              }
          description = if T.null body then Nothing else Just (parseMetaDocText body)
       in (info, description)
  where
    takeFields lines' =
      case lines' of
        line : rest
          | Just (key, value) <- headerField line ->
              let (continuation, rest') = span isContinuation rest
                  value' = T.strip (T.unwords (value : map T.strip continuation))
                  (more, body) = takeFields rest'
               in ((key, value') : more, body)
        _ -> ([], lines')

    isContinuation line =
      not (T.all isSpace line) && T.isPrefixOf " " line && isNothing (headerField line)

    headerField line = do
      let (key, rest) = T.breakOn ":" line
          key' = T.strip key
      value <- T.stripPrefix ":" rest
      if key' `elem` ["Module", "Description", "Copyright", "License", "Maintainer", "Stability", "Portability"]
        then Just (key', T.strip value)
        else Nothing

-- Export list ------------------------------------------------------------------

buildExports :: Maybe SourceSpan -> [ExportSpec] -> Build [ExportItem]
buildExports headSpan specs = do
  between <-
    case headSpan of
      Nothing -> pure []
      Just sp -> do
        index <- gets stIndex
        let (claimed, index') = claimBetween sp index
        modify' (\st -> st {stIndex = index'})
        pure claimed
  let commentItems = [(offsetOf (docCommentSpan c), commentExportItem c) | c <- between]
      specItems = [(fromMaybe 0 (exportSpecSpan spec >>= fmap fst . spanOffsets), exportSpecItem spec) | spec <- specs]
  pure (map snd (sortOn fst (commentItems <> specItems)))
  where
    offsetOf sp = maybe 0 fst (spanOffsets sp)

commentExportItem :: DocComment -> ExportItem
commentExportItem comment =
  case docCommentKind comment of
    DocCommentSection level -> ExportSectionItem level (parseInline (docCommentText comment))
    DocCommentNamed chunkName -> ExportDocItem (Just chunkName) DocEmpty
    _ -> ExportDocItem Nothing (parseDocText (docCommentText comment))

exportSpecSpan :: ExportSpec -> Maybe SourceSpan
exportSpecSpan spec =
  case spec of
    ExportAnn ann _ -> annotationSpan ann
    _ -> Nothing

exportSpecItem :: ExportSpec -> ExportItem
exportSpecItem spec =
  case spec of
    ExportAnn _ inner -> exportSpecItem inner
    ExportModule _ modName -> ExportModuleItem modName
    ExportVar _ ns name -> ExportDeclItem (nameText name) (exportNamespace ns name) ExportNoSubordinates
    ExportAbs _ ns name -> ExportDeclItem (nameText name) (exportNamespace ns name) ExportNoSubordinates
    ExportAll _ ns name -> ExportDeclItem (nameText name) (exportNamespace ns name) ExportAllSubordinates
    ExportWith _ ns name members ->
      ExportDeclItem (nameText name) (exportNamespace ns name) (ExportSomeSubordinates (map (nameText . ieBundledMemberName) members))
    ExportWithAll _ ns name _ _ -> ExportDeclItem (nameText name) (exportNamespace ns name) ExportAllSubordinates

exportNamespace :: Maybe IEEntityNamespace -> Name -> Namespace
exportNamespace ns name =
  case ns of
    Just IEEntityNamespaceType -> NamespaceType
    Just IEEntityNamespacePattern -> NamespaceValue
    Just IEEntityNamespaceData -> NamespaceValue
    Nothing ->
      case nameType name of
        NameConId -> NamespaceType
        NameConSym -> NamespaceType
        _ -> NamespaceValue

-- Declarations -----------------------------------------------------------------

mkDecl :: Text -> Namespace -> DeclKind -> Maybe Text -> Maybe MetaDoc -> Map.Map Int MetaDoc -> [DeclDoc] -> Maybe Location -> DeclDoc
mkDecl name ns kind sig doc args subs loc =
  DeclDoc
    { declName = name,
      declNamespace = ns,
      declKind = kind,
      declSignature = sig,
      declDoc = doc,
      declArgDocs = args,
      declSubordinates = subs,
      declFixity = Nothing,
      declWarning = Nothing,
      declLocation = loc
    }

peelDecl :: Decl -> (Maybe SourceSpan, Decl)
peelDecl decl =
  case decl of
    DeclAnn ann inner ->
      let (sp, inner') = peelDecl inner
       in (sp <|> annotationSpan ann, inner')
    _ -> (Nothing, decl)

buildTopDecl :: Decl -> Build [DeclDoc]
buildTopDecl decl = do
  let (msp, inner) = peelDecl decl
      col = maybe 0 spanStartColumn msp
  loc <- location msp
  case inner of
    DeclTypeSig names ty -> signatureDecls DeclKindFunction 0 col msp loc names ty
    DeclPatSynSig names ty -> signatureDecls DeclKindPatternSynonym 0 col msp loc names ty
    DeclValue valueDecl -> bindingDecl DeclKindFunction 0 msp loc valueDecl
    DeclPatSyn patSyn -> do
      (doc, ()) <- documented 0 msp (pure ())
      pure [mkDecl (renderOccurrence (patSynDeclName patSyn)) NamespaceValue DeclKindPatternSynonym Nothing doc Map.empty [] loc]
    DeclForeign foreign'
      | foreignDirection foreign' == ForeignImport -> do
          (doc, args) <- documented 0 msp (argDocs col (foreignType foreign'))
          pure [mkDecl (renderOccurrence (foreignName foreign')) NamespaceValue DeclKindForeignImport (Just (renderType (foreignType foreign'))) doc args [] loc]
      | otherwise -> pure []
    DeclFixity assoc _ prec ops -> do
      recordFixity assoc prec ops
      pure []
    DeclTypeSyn synonym -> do
      (doc, ()) <- documented 0 msp (pure ())
      let head' = typeSynHead synonym
          sig = renderBinderHead head' <> " = " <> renderType (typeSynBody synonym)
      pure [mkDecl (renderOccurrence (binderHeadName head')) NamespaceType DeclKindTypeSynonym (Just sig) doc Map.empty [] loc]
    DeclData dataDecl -> dataLikeDecl DeclKindData msp col loc (dataDeclHead dataDecl) (dataDeclConstructors dataDecl) (dataDeclDeriving dataDecl)
    DeclTypeData dataDecl -> dataLikeDecl DeclKindData msp col loc (dataDeclHead dataDecl) (dataDeclConstructors dataDecl) (dataDeclDeriving dataDecl)
    DeclNewtype newtypeDecl ->
      dataLikeDecl DeclKindNewtype msp col loc (newtypeDeclHead newtypeDecl) (maybeToList (newtypeDeclConstructor newtypeDecl)) (newtypeDeclDeriving newtypeDecl)
    DeclClass classDecl -> classDeclDoc msp col loc classDecl
    DeclInstance instanceDecl -> do
      (doc, ()) <- documented 0 msp (pure ())
      let overlap = listToMaybe [T.pack (show o) | PragmaInstanceOverlap o <- map pragmaType (instanceDeclPragmas instanceDecl)]
      recordInstance
        InstanceDoc
          { instanceClass = fromMaybe "?" (typeHeadName (instanceDeclHead instanceDecl)),
            instanceHead =
              renderForall (instanceDeclForall instanceDecl)
                <> renderContext (instanceDeclContext instanceDecl)
                <> renderType (instanceDeclHead instanceDecl),
            instanceDoc = doc,
            instanceOverlap = overlap,
            instanceDerived = False,
            instanceLocation = loc
          }
      pure []
    DeclStandaloneDeriving deriving' -> do
      (doc, ()) <- documented 0 msp (pure ())
      recordInstance
        InstanceDoc
          { instanceClass = fromMaybe "?" (typeHeadName (standaloneDerivingHead deriving')),
            instanceHead =
              renderForall (standaloneDerivingForall deriving')
                <> renderContext (standaloneDerivingContext deriving')
                <> renderType (standaloneDerivingHead deriving'),
            instanceDoc = doc,
            instanceOverlap = Nothing,
            instanceDerived = True,
            instanceLocation = loc
          }
      pure []
    DeclTypeFamilyDecl family' -> do
      (doc, ()) <- documented 0 msp (pure ())
      pure (maybeToList (typeFamilyDeclDoc loc doc family'))
    DeclDataFamilyDecl family' -> do
      (doc, ()) <- documented 0 msp (pure ())
      let head' = dataFamilyDeclHead family'
          sig = renderBinderHead head' <> maybe "" ((" :: " <>) . renderType) (dataFamilyDeclKind family')
      pure [mkDecl (renderOccurrence (binderHeadName head')) NamespaceType DeclKindDataFamily (Just sig) doc Map.empty [] loc]
    DeclPragma pragma -> do
      recordWarningPragma pragma
      pure []
    _ -> pure []

signatureDecls :: DeclKind -> Int -> Int -> Maybe SourceSpan -> Maybe Location -> [BinderName] -> Type -> Build [DeclDoc]
signatureDecls kind parentColumn col msp loc names ty = do
  (doc, args) <- documented parentColumn msp (argDocs col ty)
  pure [mkDecl (renderOccurrence name) NamespaceValue kind (Just (renderType ty)) doc args [] loc | name <- names]

bindingDecl :: DeclKind -> Int -> Maybe SourceSpan -> Maybe Location -> ValueDecl -> Build [DeclDoc]
bindingDecl kind parentColumn msp loc valueDecl =
  case valueDeclName valueDecl of
    Nothing -> pure []
    Just name -> do
      (doc, ()) <- documented parentColumn msp (pure ())
      pure [mkDecl (renderOccurrence name) NamespaceValue kind Nothing doc Map.empty [] loc]

valueDeclName :: ValueDecl -> Maybe UnqualifiedName
valueDeclName valueDecl =
  case valueDecl of
    FunctionBind name _ -> Just name
    PatternBind _ pat _ -> patternVar pat
  where
    patternVar pat =
      case pat of
        PAnn _ inner -> patternVar inner
        PVar name -> Just name
        _ -> Nothing

-- | Argument documentation by position for a signature type.
argDocs :: Int -> Type -> Build (Map.Map Int MetaDoc)
argDocs col ty = do
  docs <- forM (zip [0 ..] (functionTypeArms ty)) $ \(position, arm) -> do
    (doc, ()) <- documented col (typeSpan arm) (pure ())
    pure ((position,) <$> doc)
  pure (Map.fromList (catMaybes docs))

dataLikeDecl :: DeclKind -> Maybe SourceSpan -> Int -> Maybe Location -> BinderHead UnqualifiedName -> [DataConDecl] -> [DerivingClause] -> Build [DeclDoc]
dataLikeDecl kind msp col loc head' constructors derivings = do
  let name = binderHeadName head'
      tyVars = binderHeadTyVars head'
      resultText = T.unwords (renderBinder name : map tyVarBinderName tyVars)
  (doc, subs) <- documented 0 msp (concat <$> mapM (constructorDecl col resultText) constructors)
  forM_ derivings $ \clause ->
    forM_ (derivedClassNames clause) $ \className ->
      recordInstance
        InstanceDoc
          { instanceClass = className,
            instanceHead = derivedHead className resultText tyVars,
            instanceDoc = Nothing,
            instanceOverlap = Nothing,
            instanceDerived = True,
            instanceLocation = loc
          }
  pure [mkDecl (renderOccurrence name) NamespaceType kind (Just (renderBinderHead head')) doc Map.empty subs loc]

derivedClassNames :: DerivingClause -> [Text]
derivedClassNames clause =
  case derivingClasses clause of
    Left name -> [nameText name]
    Right types -> mapMaybe typeHeadName types

-- | The head of a derived instance. Every type variable is assumed to need the
-- class, which is what the standard deriving mechanism produces.
derivedHead :: Text -> Text -> [TyVarBinder] -> Text
derivedHead className resultText tyVars =
  renderContext' <> className <> " " <> typeText
  where
    typeText = if null tyVars then resultText else "(" <> resultText <> ")"
    renderContext' =
      case tyVars of
        [] -> ""
        [single] -> className <> " " <> tyVarBinderName single <> " => "
        many -> "(" <> T.intercalate ", " [className <> " " <> tyVarBinderName v | v <- many] <> ") => "

-- | Constructors, their arguments and their fields are not layout items, so
-- a @-- ^@ at any column inside the declaration can document them. The column
-- check therefore uses the declaration's column throughout. GHC ignores a
-- @-- ^@ after a record constructor's closing brace, so a record constructor
-- takes only a @-- |@ comment.
constructorDecl :: Int -> Text -> DataConDecl -> Build [DeclDoc]
constructorDecl declColumn resultText con = do
  let (msp, inner) = peelCon con
  loc <- location msp
  case inner of
    PrefixCon tyVars ctx name bangs -> do
      (doc, args) <- documented declColumn msp (constructorArgDocs declColumn bangs)
      let sig = renderForall tyVars <> renderContext ctx <> arrows (map (renderType . bangType) bangs)
      pure [mkDecl (renderOccurrence name) NamespaceValue DeclKindConstructor (Just sig) doc args [] loc]
    InfixCon tyVars ctx left name right -> do
      (doc, args) <- documented declColumn msp (constructorArgDocs declColumn [left, right])
      let sig = renderForall tyVars <> renderContext ctx <> arrows (map (renderType . bangType) [left, right])
      pure [mkDecl (renderOccurrence name) NamespaceValue DeclKindConstructor (Just sig) doc args [] loc]
    RecordCon tyVars ctx name fields -> do
      doc <- claimNext msp
      fieldDocs <- concat <$> mapM (fieldDecl declColumn resultText) fields
      discardPrev declColumn msp "documentation comment after a record constructor's closing brace is ignored, as GHC does"
      let fieldTypes = concat [replicate (length (fieldNames field)) (renderType (bangType (fieldType field))) | field <- fields]
          sig = renderForall tyVars <> renderContext ctx <> arrows fieldTypes
      pure [mkDecl (renderOccurrence name) NamespaceValue DeclKindConstructor (Just sig) doc Map.empty fieldDocs loc]
    GadtCon telescopes ctx names body -> do
      (doc, (fieldDocs, args)) <-
        documented declColumn msp $
          case body of
            GadtRecordBody fields _ -> (,Map.empty) . concat <$> mapM (fieldDecl declColumn resultText) fields
            GadtPrefixBody arms _ -> ([],) <$> constructorArgDocs declColumn (map fst arms)
      let quantifier = T.concat [renderForall (forallTelescopeBinders telescope) | telescope <- telescopes]
          bodyText =
            case body of
              GadtPrefixBody arms result -> T.intercalate " -> " (map (renderType . bangType . fst) arms <> [renderType result])
              GadtRecordBody fields result ->
                T.intercalate " -> " (concat [replicate (length (fieldNames field)) (renderType (bangType (fieldType field))) | field <- fields] <> [renderType result])
          sig = quantifier <> renderContext ctx <> bodyText
      pure [mkDecl (renderOccurrence name) NamespaceValue DeclKindConstructor (Just sig) doc args fieldDocs loc | name <- names]
    _ -> do
      -- Tuple, list and unboxed sum constructors only occur in aihc-prim.
      _ <- documented declColumn msp (pure ())
      pure []
  where
    arrows args = T.intercalate " -> " (args <> [resultText])

-- | Documentation on the arguments of a plain constructor, by position.
constructorArgDocs :: Int -> [BangType] -> Build (Map.Map Int MetaDoc)
constructorArgDocs col bangs = do
  docs <- forM (zip [0 ..] bangs) $ \(position, bang) -> do
    let msp = listToMaybe (mapMaybe annotationSpan (bangAnns bang)) <|> typeSpan (bangType bang)
    (doc, ()) <- documented col msp (pure ())
    pure ((position,) <$> doc)
  pure (Map.fromList (catMaybes docs))

peelCon :: DataConDecl -> (Maybe SourceSpan, DataConDecl)
peelCon con =
  case con of
    DataConAnn ann inner ->
      let (sp, inner') = peelCon inner
       in (sp <|> annotationSpan ann, inner')
    _ -> (Nothing, con)

fieldDecl :: Int -> Text -> FieldDecl -> Build [DeclDoc]
fieldDecl parentColumn resultText field = do
  let msp = listToMaybe (mapMaybe annotationSpan (fieldAnns field))
  loc <- location msp
  (doc, ()) <- documented parentColumn msp (pure ())
  let sig = resultText <> " -> " <> renderType (bangType (fieldType field))
  pure [mkDecl (renderOccurrence name) NamespaceValue DeclKindField (Just sig) doc Map.empty [] loc | name <- fieldNames field]

classDeclDoc :: Maybe SourceSpan -> Int -> Maybe Location -> ClassDecl -> Build [DeclDoc]
classDeclDoc msp col loc classDecl = do
  let head' = classDeclHead classDecl
      sig = renderContext (fromMaybe [] (classDeclContext classDecl)) <> renderBinderHead head'
  (doc, items) <- documented 0 msp (concat <$> mapM (classItem col) (classDeclItems classDecl))
  pure [mkDecl (renderOccurrence (binderHeadName head')) NamespaceType DeclKindClass (Just sig) doc Map.empty (mergeValueDecls items) loc]

classItem :: Int -> ClassDeclItem -> Build [DeclDoc]
classItem classColumn item = do
  let (msp, inner) = peelClassItem item
      col = maybe 0 spanStartColumn msp
  loc <- location msp
  case inner of
    ClassItemTypeSig names ty -> signatureDecls DeclKindMethod classColumn col msp loc names ty
    ClassItemDefault valueDecl -> bindingDecl DeclKindMethod classColumn msp loc valueDecl
    ClassItemFixity assoc _ prec ops -> do
      recordFixity assoc prec ops
      pure []
    ClassItemTypeFamilyDecl family' -> do
      (doc, ()) <- documented classColumn msp (pure ())
      pure (maybeToList ((\d -> d {declKind = DeclKindAssociatedType}) <$> typeFamilyDeclDoc loc doc family'))
    ClassItemDataFamilyDecl family' -> do
      (doc, ()) <- documented classColumn msp (pure ())
      let head' = dataFamilyDeclHead family'
      pure [mkDecl (renderOccurrence (binderHeadName head')) NamespaceType DeclKindAssociatedType (Just (renderBinderHead head')) doc Map.empty [] loc]
    _ -> pure []

peelClassItem :: ClassDeclItem -> (Maybe SourceSpan, ClassDeclItem)
peelClassItem item =
  case item of
    ClassItemAnn ann inner ->
      let (sp, inner') = peelClassItem inner
       in (sp <|> annotationSpan ann, inner')
    _ -> (Nothing, item)

typeFamilyDeclDoc :: Maybe Location -> Maybe MetaDoc -> TypeFamilyDecl -> Maybe DeclDoc
typeFamilyDeclDoc loc doc family' = do
  name <- typeHeadName (typeFamilyDeclHead family')
  let params = map renderTyVarBinder (typeFamilyDeclParams family')
      resultSig =
        case typeFamilyDeclResultSig family' of
          Nothing -> ""
          Just (TypeFamilyKindSig kind) -> " :: " <> renderType kind
          Just (TypeFamilyTyVarSig binder) -> " = " <> renderTyVarBinder binder
          Just (TypeFamilyInjectiveSig binder injectivity) ->
            " = "
              <> renderTyVarBinder binder
              <> " | "
              <> typeFamilyInjectivityResult injectivity
              <> " -> "
              <> T.unwords (typeFamilyInjectivityDetermined injectivity)
      sig = T.unwords (renderType (typeFamilyDeclHead family') : params) <> resultSig
  pure (mkDecl name NamespaceType DeclKindTypeFamily (Just sig) doc Map.empty [] loc)

-- Fixities, warnings, instances ------------------------------------------------

recordFixity :: FixityAssoc -> Maybe Int -> [OperatorName] -> Build ()
recordFixity assoc prec ops =
  modify' $ \st ->
    st {stFixities = foldl' (\m op -> Map.insert (renderOccurrence op) fixity m) (stFixities st) ops}
  where
    fixity =
      Fixity
        { fixityPrecedence = fromMaybe 9 prec,
          fixityDirection = case assoc of
            Infix -> FixityInfix
            InfixL -> FixityInfixL
            InfixR -> FixityInfixR
        }

recordInstance :: InstanceDoc -> Build ()
recordInstance instance' = modify' (\st -> st {stInstances = instance' : stInstances st})

-- | @{-\# DEPRECATED f, g "message" \#-}@ and @{-\# WARNING ... \#-}@ pragmas.
recordWarningPragma :: Pragma -> Build ()
recordWarningPragma pragma =
  case pragmaType pragma of
    PragmaDeprecated _ -> record
    PragmaWarning _ -> record
    _ -> pure ()
  where
    record =
      case pragmaMessage pragma of
        Nothing -> pure ()
        Just message ->
          modify' $ \st ->
            st {stWarnings = foldl' (\m name -> Map.insert name message m) (stWarnings st) (pragmaNames pragma)}

-- | The names between the pragma keyword and the first quote.
pragmaNames :: Pragma -> [Text]
pragmaNames pragma =
  let inner = T.strip (T.dropEnd 3 (T.drop 3 (pragmaRawText pragma)))
      afterKeyword = T.stripStart (T.dropWhile (not . isSpace) inner)
      (namesText, _) = T.breakOn "\"" afterKeyword
   in filter (not . T.null) (map (T.strip . stripParens) (T.splitOn "," namesText))
  where
    stripParens text = fromMaybe text (T.stripPrefix "(" (T.strip text) >>= T.stripSuffix ")")

-- | The quoted message of a warning pragma, with list brackets and quotes
-- removed.
pragmaMessage :: Pragma -> Maybe Text
pragmaMessage pragma =
  let (_, quoted) = T.breakOn "\"" (pragmaRawText pragma)
      parts = [T.strip p | p <- T.splitOn "\"" quoted, not (T.all isSpace p), p /= ",", p /= "[", p /= "]"]
      message = T.intercalate " " (filter (\p -> p /= "#-}" && not (T.isPrefixOf "#-}" p)) parts)
   in if T.null quoted then Nothing else Just (T.strip message)

applyFixities :: Map.Map Text Fixity -> DeclDoc -> DeclDoc
applyFixities fixities decl =
  decl
    { declFixity = Map.lookup (declName decl) fixities <|> declFixity decl,
      declSubordinates = map (applyFixities fixities) (declSubordinates decl)
    }

applyWarnings :: Map.Map Text Text -> DeclDoc -> DeclDoc
applyWarnings warnings decl =
  decl
    { declWarning = Map.lookup (declName decl) warnings <|> declWarning decl,
      declSubordinates = map (applyWarnings warnings) (declSubordinates decl)
    }

-- | A signature and the binding it types produce two entries for one name.
-- Merge them, keeping the position of the first and the documentation of
-- both.
mergeValueDecls :: [DeclDoc] -> [DeclDoc]
mergeValueDecls = foldl' insert []
  where
    insert acc decl
      | mergeable decl,
        (before, existing : after) <- break (sameBinder decl) acc =
          before <> (merge existing decl : after)
      | otherwise = acc <> [decl]

    mergeable decl = declKind decl `elem` [DeclKindFunction, DeclKindMethod, DeclKindPatternSynonym]
    sameBinder decl other =
      mergeable other
        && declName other == declName decl
        && declNamespace other == declNamespace decl
        && declKind other == declKind decl
    merge existing decl =
      existing
        { declSignature = declSignature existing <|> declSignature decl,
          declDoc = mergeDocs (declDoc existing) (declDoc decl),
          declArgDocs = Map.union (declArgDocs existing) (declArgDocs decl),
          declLocation = declLocation existing <|> declLocation decl
        }
