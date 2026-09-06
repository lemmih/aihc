-- | Extract scoping and typing information from GHC .hi interface files.
--
-- Given a package name, this module locates the package via @ghc-pkg@,
-- reads every exposed module's .hi file using the GHC API, and produces
-- a structured 'PackageInterface' suitable for YAML serialization.
--
-- Handles re-export facades (e.g. @base@ re-exporting from @ghc-internal@)
-- by following exported names to their defining modules.
module Aihc.Dev.ExtractHi
  ( extractPackage,
    extractPackageMaybe,
    extractSourcePackage,
  )
where

import Aihc.Dev.ExtractHi.GhcSession (withReadIface)
import Aihc.Dev.ExtractHi.Types
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Pretty (prettyType)
import Aihc.Parser.Syntax
  ( BinderHead (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    ExportSpec (..),
    FieldDecl (..),
    FixityAssoc,
    GadtBody (..),
    IEBundledMember (..),
    IEEntityNamespace (..),
    Module (..),
    NewtypeDecl (..),
    Pattern (..),
    Type (..),
    TypeFamilyDecl (..),
    TypeFamilyResultSig (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    moduleExports,
    peelClassDeclItemAnn,
    peelDataConAnn,
    peelDeclAnn,
    renderName,
    renderUnqualifiedName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Distribution.ModuleName qualified as CabalModuleName
import Distribution.PackageDescription (GenericPackageDescription, condLibrary, exposedModules)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.CondTree (CondTree (condTreeData))
import GHC (Ghc, lookupName)
import GHC.Core.ConLike (ConLike (..), conLikeName)
import GHC.Core.DataCon (dataConName)
import GHC.Core.DataCon qualified as DataCon
import GHC.Core.PatSyn (patSynName, pprPatSynType)
import GHC.Core.TyCo.Ppr (pprSigmaType, pprType)
import GHC.Core.TyCon (isAlgTyCon, tyConDataCons, tyConName, tyConResKind)
import GHC.Iface.Syntax
  ( IfaceAT (..),
    IfaceClassBody (..),
    IfaceClassOp (..),
    IfaceConDecl (..),
    IfaceConDecls (..),
    IfaceDecl (..),
  )
import GHC.Iface.Type (IfaceTyConBinder, IfaceType, ShowForAllFlag (..), pprIfaceSigmaType, pprIfaceType)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Id (idType)
import GHC.Types.Name (Name, getOccString, nameModule_maybe)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.TyThing (TyThing (..))
import GHC.Unit.Module (moduleNameString)
import GHC.Unit.Module.ModIface (ModIface, mi_decls, mi_exports, mi_fixities)
import GHC.Unit.Types (moduleName, moduleUnit, unitString)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.Syntax.Basic qualified as GHC
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import System.Process (readProcess, readProcessWithExitCode)

-- | Cached module data: declarations and fixities from a single .hi file.
data CachedModule = CachedModule
  { cmDecls :: Map String IfaceDecl,
    cmFixities :: Map String GHC.Fixity
  }

-- | Extract the full interface for a locally installed package.
extractPackage :: String -> IO PackageInterface
extractPackage pkgName = do
  (pkgId, importDir, exposedMods) <- queryPackage pkgName
  extractPackageFromQuery pkgId importDir exposedMods

extractPackageMaybe :: String -> IO (Maybe PackageInterface)
extractPackageMaybe pkgName = do
  mPackage <- queryPackageMaybe pkgName
  case mPackage of
    Nothing -> pure Nothing
    Just (pkgId, importDir, exposedMods) -> Just <$> extractPackageFromQuery pkgId importDir exposedMods

extractSourcePackage :: FilePath -> String -> IO PackageInterface
extractSourcePackage root pkgName = do
  exposedMods <- exposedSourceModules root
  modules <- mapM (extractSourceModule (root </> "src")) exposedMods
  pure
    PackageInterface
      { piPackage = T.pack pkgName,
        piModules = modules
      }

extractPackageFromQuery :: String -> FilePath -> [String] -> IO PackageInterface
extractPackageFromQuery pkgId importDir exposedMods =
  withReadIface $ \readIface -> do
    cacheRef <- liftIO $ newIORef (Map.empty :: Map (String, String) CachedModule)
    modules <- mapM (extractSingleModule readIface cacheRef importDir) exposedMods
    pure
      PackageInterface
        { piPackage = T.pack pkgId,
          piModules = modules
        }

-- | Load a remote module's data into the cache, returning the cached data.
loadModule ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  String ->
  String ->
  IO CachedModule
loadModule readIface cacheRef unit modName = do
  cache <- readIORef cacheRef
  let key = (unit, modName)
  case Map.lookup key cache of
    Just cm -> pure cm
    Nothing -> do
      mImportDir <- queryImportDir unit
      cm <- case mImportDir of
        Nothing -> pure emptyCachedModule
        Just importDir -> do
          let hiPath = importDir </> map dotToSlash modName <.> "hi"
          exists <- doesFileExist hiPath
          if exists
            then do
              iface <- readIface hiPath
              let ds = map snd (mi_decls iface)
                  decls = Map.fromList [(getOccString (ifName d), d) | d <- ds]
                  fixs = Map.fromList [(occNameString occ, f) | (occ, f) <- mi_fixities iface]
              pure CachedModule {cmDecls = decls, cmFixities = fixs}
            else pure emptyCachedModule
      modifyIORef' cacheRef (Map.insert key cm)
      pure cm

emptyCachedModule :: CachedModule
emptyCachedModule = CachedModule Map.empty Map.empty

-- | Look up a declaration by following a Name to its defining module.
lookupDecl ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  Name ->
  IO (Maybe IfaceDecl)
lookupDecl readIface cacheRef name = case nameModule_maybe name of
  Nothing -> pure Nothing
  Just m -> do
    let unit = unitString (moduleUnit m)
        modName = moduleNameString (moduleName m)
    cm <- loadModule readIface cacheRef unit modName
    pure (Map.lookup (getOccString name) (cmDecls cm))

-- | Look up a fixity by following a Name to its defining module.
lookupFixity ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  Name ->
  IO (Maybe GHC.Fixity)
lookupFixity readIface cacheRef name = case nameModule_maybe name of
  Nothing -> pure Nothing
  Just m -> do
    let unit = unitString (moduleUnit m)
        modName = moduleNameString (moduleName m)
    cm <- loadModule readIface cacheRef unit modName
    pure (Map.lookup (getOccString name) (cmFixities cm))

-- | Extract interface information from a single module.
extractSingleModule ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  FilePath ->
  String ->
  Ghc ModuleInterface
extractSingleModule readIface cacheRef importDir modName = do
  let hiPath = importDir </> map dotToSlash modName <.> "hi"
  exists <- liftIO $ doesFileExist hiPath
  if exists
    then do
      iface <- liftIO $ readIface hiPath
      ifaceToModule readIface cacheRef modName iface
    else
      pure
        ModuleInterface
          { miModule = T.pack modName,
            miTypes = [],
            miValues = [],
            miClasses = [],
            miFixities = []
          }

-- | Convert a 'ModIface' to our output representation.
ifaceToModule ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  String ->
  ModIface ->
  Ghc ModuleInterface
ifaceToModule readIface cacheRef modName iface = do
  let exports = mi_exports iface
      localFixities = mi_fixities iface
      localDecls = map snd (mi_decls iface)
      localDeclMap = Map.fromList [(getOccString (ifName d), d) | d <- localDecls]
      localFixMap = Map.fromList [(occNameString occ, f) | (occ, f) <- localFixities]

  -- Classify exports
  (types, values, classes) <- classifyExports (T.pack modName) exports localDeclMap (lookupDecl readIface cacheRef)

  -- Collect fixities: local ones first, then look up re-exported names
  let allExportedNames = concatMap availNames exports
  remoteFixities <- liftIO $ collectFixities readIface cacheRef localFixMap allExportedNames

  pure
    ModuleInterface
      { miModule = T.pack modName,
        miTypes = types,
        miValues = values,
        miClasses = classes,
        miFixities = remoteFixities
      }

-- | Get all names from an AvailInfo.
availNames :: AvailInfo -> [Name]
availNames (Avail n) = [n]
availNames (AvailTC _ subs) = subs

-- | Collect fixities for all exported names.
--
-- First checks local fixities, then follows re-exports to defining modules.
-- Only includes names that actually have a non-default fixity.
collectFixities ::
  (FilePath -> IO ModIface) ->
  IORef (Map (String, String) CachedModule) ->
  Map String GHC.Fixity ->
  [Name] ->
  IO [FixityInfo]
collectFixities readIface cacheRef localFixMap names = do
  results <- mapM lookupOne names
  pure (concat results)
  where
    lookupOne name = do
      let occStr = getOccString name
      case Map.lookup occStr localFixMap of
        Just fix -> pure [toFixityInfo occStr fix]
        Nothing -> do
          mFix <- lookupFixity readIface cacheRef name
          case mFix of
            Just fix -> pure [toFixityInfo occStr fix]
            Nothing -> pure []

    toFixityInfo nameStr (GHC.Fixity prec dir) =
      FixityInfo
        { fiName = T.pack nameStr,
          fiDirection = convertDir dir,
          fiPrecedence = prec
        }

    convertDir GHC.InfixL = InfixL
    convertDir GHC.InfixR = InfixR
    convertDir GHC.InfixN = InfixN

-- | Classify exports into types, values, and classes.
classifyExports ::
  Text ->
  [AvailInfo] ->
  Map String IfaceDecl ->
  (Name -> IO (Maybe IfaceDecl)) ->
  Ghc ([ExportedType], [ExportedValue], [ExportedClass])
classifyExports modName avails localDeclMap cachedLookup = do
  results <- mapM (classifySingle modName localDeclMap cachedLookup) avails
  let (ts, vs, cs) = foldr merge ([], [], []) results
  pure (ts, vs, cs)
  where
    merge (t, v, c) (ts, vs, cs) = (t ++ ts, v ++ vs, c ++ cs)

-- | Classify a single AvailInfo.
classifySingle ::
  Text ->
  Map String IfaceDecl ->
  (Name -> IO (Maybe IfaceDecl)) ->
  AvailInfo ->
  Ghc ([ExportedType], [ExportedValue], [ExportedClass])
classifySingle modName localDeclMap cachedLookup avail = case avail of
  Avail name -> do
    let nameStr = getOccString name
    mDecl <- liftIO $ resolveDecl localDeclMap cachedLookup nameStr name
    case mDecl of
      Just decl
        | Just value <- extractValue decl ->
            pure ([], [value], [])
      Just IfaceData {} ->
        pure ([], [], [])
      Just IfaceSynonym {} ->
        pure ([], [], [])
      Just IfaceFamily {} ->
        pure ([], [], [])
      Just IfaceClass {} ->
        pure ([], [], [])
      _ -> do
        mValue <- extractValueFromName name
        case mValue of
          Just value -> pure ([], [value], [])
          Nothing -> unresolvedExport modName "value" nameStr
  AvailTC name subs -> do
    let nameStr = getOccString name
        subNames = map getOccString subs
    mDecl <- liftIO $ resolveDecl localDeclMap cachedLookup nameStr name
    case mDecl of
      Just decl@IfaceClass {} ->
        pure (associatedTypes decl, [], [extractClass decl subNames])
      Just decl@IfaceData {} ->
        pure ([extractDataType decl subNames], [], [])
      Just decl@IfaceSynonym {} ->
        pure ([extractSynonym decl], [], [])
      Just decl@IfaceFamily {} ->
        pure ([extractFamily decl], [], [])
      _ -> do
        mType <- extractTypeFromName name subNames
        case mType of
          Just typ -> pure ([typ], [], [])
          Nothing -> unresolvedExport modName "type" nameStr

unresolvedExport :: Text -> String -> String -> Ghc a
unresolvedExport modName exportKind name =
  liftIO $ ioError (userError ("could not resolve " <> exportKind <> " export " <> T.unpack modName <> "." <> name))

-- | Resolve a declaration: check local first, then follow re-exports.
resolveDecl ::
  Map String IfaceDecl ->
  (Name -> IO (Maybe IfaceDecl)) ->
  String ->
  Name ->
  IO (Maybe IfaceDecl)
resolveDecl localDeclMap cachedLookup nameStr name =
  case Map.lookup nameStr localDeclMap of
    Just decl -> pure (Just decl)
    Nothing -> cachedLookup name

-- | Extract value/function information from an IfaceId declaration.
extractValue :: IfaceDecl -> Maybe ExportedValue
extractValue decl = case decl of
  IfaceId {ifName, ifType} ->
    Just
      ExportedValue
        { evName = T.pack (getOccString ifName),
          evType = renderType ifType
        }
  _ -> Nothing

extractValueFromName :: Name -> Ghc (Maybe ExportedValue)
extractValueFromName name = do
  mThing <- lookupName name
  pure $
    case mThing of
      Just (AnId identifier) ->
        Just
          ExportedValue
            { evName = T.pack (getOccString name),
              evType = T.pack (showSDocUnsafe (pprSigmaType (idType identifier)))
            }
      Just (AConLike conLike@(RealDataCon dataCon)) ->
        Just
          ExportedValue
            { evName = T.pack (getOccString (conLikeName conLike)),
              evType = T.pack (showSDocUnsafe (pprSigmaType (DataCon.dataConRepType dataCon)))
            }
      Just (AConLike (PatSynCon patSyn)) ->
        Just
          ExportedValue
            { evName = T.pack (getOccString (patSynName patSyn)),
              evType = T.pack (showSDocUnsafe (pprPatSynType patSyn))
            }
      _ -> Nothing

extractTypeFromName :: Name -> [String] -> Ghc (Maybe ExportedType)
extractTypeFromName name subNames = do
  mThing <- lookupName name
  pure $
    case mThing of
      Just (ATyCon tyCon) ->
        let parentName = getOccString (tyConName tyCon)
            ctorNames =
              if isAlgTyCon tyCon
                then map (T.pack . getOccString . dataConName) (tyConDataCons tyCon)
                else []
            ctorStrings = map T.unpack ctorNames
            extraSubs =
              [ T.pack subName
              | subName <- subNames,
                subName /= parentName,
                subName `notElem` ctorStrings
              ]
         in Just
              ExportedType
                { etName = T.pack parentName,
                  etKind = T.pack (showSDocUnsafe (pprType (tyConResKind tyCon))),
                  etConstructors = ctorNames ++ extraSubs
                }
      _ -> Nothing

-- | Extract data type information.
extractDataType :: IfaceDecl -> [String] -> ExportedType
extractDataType decl subNames = case decl of
  IfaceData {ifName, ifBinders, ifResKind, ifCons} ->
    let parentName = getOccString ifName
        ctors = case ifCons of
          IfAbstractTyCon -> []
          IfDataTyCon _ cs -> map (T.pack . getOccString . ifConName) cs
          IfNewTyCon c -> [T.pack (getOccString (ifConName c))]
        ctorStrings = map T.unpack ctors
        extraSubs =
          [ T.pack s
          | s <- subNames,
            s /= parentName,
            s `notElem` ctorStrings
          ]
     in ExportedType
          { etName = T.pack parentName,
            etKind = renderKind ifBinders ifResKind,
            etConstructors = ctors ++ extraSubs
          }
  other ->
    ExportedType
      { etName = T.pack (getOccString (ifName other)),
        etKind = "<unexpected>",
        etConstructors = []
      }

-- | Extract type synonym information.
extractSynonym :: IfaceDecl -> ExportedType
extractSynonym decl = case decl of
  IfaceSynonym {ifName, ifBinders, ifResKind} ->
    ExportedType
      { etName = T.pack (getOccString ifName),
        etKind = renderKind ifBinders ifResKind,
        etConstructors = []
      }
  other ->
    ExportedType
      { etName = T.pack (getOccString (ifName other)),
        etKind = "<unexpected>",
        etConstructors = []
      }

-- | Extract type family information.
extractFamily :: IfaceDecl -> ExportedType
extractFamily decl = case decl of
  IfaceFamily {ifName, ifBinders, ifResKind} ->
    ExportedType
      { etName = T.pack (getOccString ifName),
        etKind = renderKind ifBinders ifResKind,
        etConstructors = []
      }
  other ->
    ExportedType
      { etName = T.pack (getOccString (ifName other)),
        etKind = "<unexpected>",
        etConstructors = []
      }

-- | Extract class information.
extractClass :: IfaceDecl -> [String] -> ExportedClass
extractClass decl _subNames = case decl of
  IfaceClass {ifName, ifBody} ->
    let methods = case ifBody of
          IfAbstractClass -> []
          IfConcreteClass {ifSigs} ->
            [extractClassOp op | op <- ifSigs]
     in ExportedClass
          { ecName = T.pack (getOccString ifName),
            ecMethods = methods
          }
  other ->
    ExportedClass
      { ecName = T.pack (getOccString (ifName other)),
        ecMethods = []
      }

-- | The associated type families of a class, exported with the class.
associatedTypes :: IfaceDecl -> [ExportedType]
associatedTypes decl = case decl of
  IfaceClass {ifBody = IfConcreteClass {ifATs}} ->
    [extractFamily familyDecl | IfaceAT familyDecl _ <- ifATs]
  _ -> []

-- | Extract a class method from its IfaceClassOp.
extractClassOp :: IfaceClassOp -> ClassMethod
extractClassOp (IfaceClassOp name ty _defMeth) =
  ClassMethod
    { cmName = T.pack (getOccString name),
      cmType = renderType ty
    }

-- | Render an IfaceType to a human-readable string.
renderType :: IfaceType -> Text
renderType ty = T.pack (showSDocUnsafe (pprIfaceSigmaType ShowForAllWhen ty))

-- | Render a kind from binders and result kind.
renderKind :: [IfaceTyConBinder] -> IfaceType -> Text
renderKind _binders resKind =
  T.pack (showSDocUnsafe (pprIfaceType resKind))

exposedSourceModules :: FilePath -> IO [String]
exposedSourceModules root = do
  cabalFiles <- filter ((".cabal" ==) . takeExtension) <$> listDirectory root
  case cabalFiles of
    cabalFile : _ -> do
      contents <- BS.readFile (root </> cabalFile)
      case runParseResult (parseGenericPackageDescription contents) of
        (_, Right gpd) -> pure (genericPackageExposedModules gpd)
        (_, Left (_, errors)) -> ioError (userError ("could not parse " <> cabalFile <> ": " <> show errors))
    [] -> ioError (userError ("no Cabal file found under " <> root))

genericPackageExposedModules :: GenericPackageDescription -> [String]
genericPackageExposedModules gpd =
  [ CabalModuleName.toFilePath modName
  | libTree <- maybe [] pure (condLibrary gpd),
    modName <- exposedModules (condTreeData libTree)
  ]

extractSourceModule :: FilePath -> String -> IO ModuleInterface
extractSourceModule srcRoot modPath = do
  let sourcePath = srcRoot </> modPath <.> "hs"
      modName = T.pack (map pathSepToDot modPath)
  exists <- doesFileExist sourcePath
  if not exists
    then ioError (userError ("source module " <> T.unpack modName <> " not found at " <> sourcePath))
    else do
      source <- TE.decodeUtf8 <$> BS.readFile sourcePath
      let (errs, parsed) =
            parseModule
              (defaultConfig {parserSourceName = sourcePath})
              source
      if null errs
        then sourceModuleInterface modName parsed
        else ioError (userError ("source module " <> T.unpack modName <> " parse failed: " <> show errs))

sourceModuleInterface :: Text -> Module -> IO ModuleInterface
sourceModuleInterface fallbackName modu =
  let iface =
        applySourceExports modu $
          ModuleInterface
            { miModule = fromMaybe fallbackName (Syntax.moduleName modu),
              miTypes = sourceTypes kindSigs decls,
              miValues = sourceValues decls,
              miClasses = sourceClasses decls,
              miFixities = sourceFixities decls
            }
   in case missingExportedValueSignatures modu decls iface of
        [] -> pure iface
        missing ->
          ioError
            ( userError
                ( "source module "
                    <> T.unpack (miModule iface)
                    <> " has exported value(s) without type signatures: "
                    <> T.unpack (T.intercalate ", " missing)
                )
            )
  where
    decls = map peelDeclAnn (moduleDecls modu)
    kindSigs = Map.fromList [(renderUnqualifiedName name, renderSourceType kind) | DeclStandaloneKindSig name kind <- decls]

sourceValues :: [Decl] -> [ExportedValue]
sourceValues decls =
  Map.elems $
    Map.fromList
      [ (renderUnqualifiedName name, ExportedValue (renderUnqualifiedName name) (renderSourceType ty))
      | DeclTypeSig names ty <- decls,
        name <- names
      ]

missingExportedValueSignatures :: Module -> [Decl] -> ModuleInterface -> [Text]
missingExportedValueSignatures modu decls iface =
  [ name
  | name <- candidateNames,
    Map.notMember name typedValues
  ]
  where
    typedValues = Map.fromList [(evName value, ()) | value <- miValues iface]
    declaredValues =
      Map.fromList [(name, ()) | DeclValue valueDecl <- decls, name <- valueDeclNames valueDecl]
    candidateNames =
      case moduleExports modu of
        Nothing -> Map.keys declaredValues
        Just specs ->
          [ name
          | name <- concatMap exportValueNames specs,
            Map.member name declaredValues
          ]

valueDeclNames :: ValueDecl -> [Text]
valueDeclNames valueDecl =
  case valueDecl of
    FunctionBind name _ -> [renderUnqualifiedName name]
    PatternBind _ pat _ -> patternBinderNames pat

patternBinderNames :: Pattern -> [Text]
patternBinderNames pat =
  case pat of
    PAnn _ inner -> patternBinderNames inner
    PVar name -> [renderUnqualifiedName name]
    _ -> []

sourceTypes :: Map Text Text -> [Decl] -> [ExportedType]
sourceTypes kindSigs =
  mapMaybe go
  where
    go decl =
      case decl of
        DeclTypeSyn syn ->
          let name = binderHeadName (typeSynHead syn)
           in Just (ExportedType name (sourceTypeKind kindSigs name Nothing) [])
        DeclTypeData dataDecl -> Just (sourceDataType kindSigs dataDecl)
        DeclData dataDecl -> Just (sourceDataType kindSigs dataDecl)
        DeclNewtype newtypeDecl -> Just (sourceNewtype kindSigs newtypeDecl)
        DeclTypeFamilyDecl familyDecl ->
          let name = sourceTypeFamilyName familyDecl
              kind = case typeFamilyDeclResultSig familyDecl of
                Just (TypeFamilyKindSig ty) -> Just ty
                _ -> Nothing
           in Just (ExportedType name (sourceTypeKind kindSigs name kind) [])
        _ -> Nothing

sourceDataType :: Map Text Text -> DataDecl -> ExportedType
sourceDataType kindSigs dataDecl =
  ExportedType
    { etName = name,
      etKind = sourceTypeKind kindSigs name (dataDeclKind dataDecl),
      etConstructors = concatMap dataConNames (dataDeclConstructors dataDecl)
    }
  where
    name = binderHeadName (dataDeclHead dataDecl)

sourceNewtype :: Map Text Text -> NewtypeDecl -> ExportedType
sourceNewtype kindSigs newtypeDecl =
  ExportedType
    { etName = name,
      etKind = sourceTypeKind kindSigs name (newtypeDeclKind newtypeDecl),
      etConstructors = maybe [] dataConNames (newtypeDeclConstructor newtypeDecl)
    }
  where
    name = binderHeadName (newtypeDeclHead newtypeDecl)

sourceTypeKind :: Map Text Text -> Text -> Maybe Type -> Text
sourceTypeKind kindSigs name inlineKind =
  case inlineKind of
    Just kind -> renderSourceType kind
    Nothing -> Map.findWithDefault "<unspecified-source-kind>" name kindSigs

sourceClasses :: [Decl] -> [ExportedClass]
sourceClasses decls =
  [ ExportedClass
      { ecName = binderHeadName (classDeclHead classDecl),
        ecMethods = sourceClassMethods classDecl
      }
  | DeclClass classDecl <- decls
  ]

sourceClassMethods :: ClassDecl -> [ClassMethod]
sourceClassMethods classDecl =
  [ ClassMethod (renderUnqualifiedName name) (renderSourceType ty)
  | item <- map peelClassDeclItemAnn (classDeclItems classDecl),
    ClassItemTypeSig names ty <- [item],
    name <- names
  ]

sourceFixities :: [Decl] -> [FixityInfo]
sourceFixities decls =
  [ FixityInfo
      { fiName = renderUnqualifiedName op,
        fiDirection = sourceFixityDirection assoc,
        fiPrecedence = fromMaybe 9 mPrec
      }
  | DeclFixity assoc _ mPrec ops <- decls,
    op <- ops
  ]

sourceFixityDirection :: FixityAssoc -> FixityDirection
sourceFixityDirection assoc =
  case assoc of
    Syntax.InfixL -> Aihc.Dev.ExtractHi.Types.InfixL
    Syntax.InfixR -> Aihc.Dev.ExtractHi.Types.InfixR
    Syntax.Infix -> Aihc.Dev.ExtractHi.Types.InfixN

applySourceExports :: Module -> ModuleInterface -> ModuleInterface
applySourceExports modu iface =
  case moduleExports modu of
    Nothing -> iface
    Just specs ->
      iface
        { miValues = filter (exportedValue allowedValues) (miValues iface),
          miTypes = mapMaybe (filterExportedType allowedTypes allowedConstructors) (miTypes iface),
          miClasses = mapMaybe (filterExportedClass allowedTypes allowedMethods) (miClasses iface),
          miFixities = filter (exportedFixity allowedValues allowedTypes) (miFixities iface)
        }
      where
        allowedValues = Map.fromList [(name, ()) | name <- concatMap exportValueNames specs]
        allowedTypes = Map.fromList [(name, exportMembers spec) | spec <- specs, name <- exportTypeNames spec]
        allowedConstructors = Map.unionsWith (<>) [Map.singleton parent members | spec <- specs, (parent, members) <- exportTypeMemberNames spec]
        allowedMethods = Map.unionsWith (<>) [Map.singleton parent members | spec <- specs, (parent, members) <- exportTypeMemberNames spec]

exportedValue :: Map Text () -> ExportedValue -> Bool
exportedValue allowed value =
  Map.member (evName value) allowed

filterExportedType :: Map Text (Maybe [Text]) -> Map Text [Text] -> ExportedType -> Maybe ExportedType
filterExportedType allowedTypes allowedConstructors typ =
  case Map.lookup (etName typ) allowedTypes of
    Nothing -> Nothing
    Just Nothing -> Just (typ {etConstructors = []})
    Just (Just []) -> Just typ
    Just (Just names) -> Just (typ {etConstructors = filter (`elem` names) (etConstructors typ)})
  where
    _ = allowedConstructors

filterExportedClass :: Map Text (Maybe [Text]) -> Map Text [Text] -> ExportedClass -> Maybe ExportedClass
filterExportedClass allowedTypes allowedMethods klass =
  case Map.lookup (ecName klass) allowedTypes of
    Nothing -> Nothing
    Just Nothing -> Just (klass {ecMethods = []})
    Just (Just []) -> Just klass
    Just (Just names) -> Just (klass {ecMethods = filter ((`elem` names) . cmName) (ecMethods klass)})
  where
    _ = allowedMethods

exportedFixity :: Map Text () -> Map Text (Maybe [Text]) -> FixityInfo -> Bool
exportedFixity allowedValues allowedTypes fixity =
  Map.member (fiName fixity) allowedValues || Map.member (fiName fixity) allowedTypes

exportValueNames :: ExportSpec -> [Text]
exportValueNames spec =
  case spec of
    ExportAnn _ inner -> exportValueNames inner
    ExportVar _ namespace name | namespace /= Just IEEntityNamespaceType -> [renderName name]
    _ -> []

exportTypeNames :: ExportSpec -> [Text]
exportTypeNames spec =
  case spec of
    ExportAnn _ inner -> exportTypeNames inner
    ExportVar _ (Just IEEntityNamespaceType) name -> [renderName name]
    ExportAbs _ _ name -> [renderName name]
    ExportAll _ _ name -> [renderName name]
    ExportWith _ _ name _ -> [renderName name]
    ExportWithAll _ _ name _ _ -> [renderName name]
    _ -> []

exportTypeMemberNames :: ExportSpec -> [(Text, [Text])]
exportTypeMemberNames spec =
  case spec of
    ExportAnn _ inner -> exportTypeMemberNames inner
    ExportWith _ _ name members -> [(renderName name, map (renderName . ieBundledMemberName) members)]
    ExportWithAll _ _ name _ members -> [(renderName name, map (renderName . ieBundledMemberName) members)]
    _ -> []

exportMembers :: ExportSpec -> Maybe [Text]
exportMembers spec =
  case spec of
    ExportAnn _ inner -> exportMembers inner
    ExportAbs {} -> Nothing
    ExportAll {} -> Just []
    ExportWith _ _ _ members -> Just (map (renderName . ieBundledMemberName) members)
    ExportWithAll _ _ _ _ members -> Just (map (renderName . ieBundledMemberName) members)
    _ -> Nothing

binderHeadName :: BinderHead UnqualifiedName -> Text
binderHeadName head' =
  case head' of
    PrefixBinderHead name _ -> renderUnqualifiedName name
    InfixBinderHead _ name _ _ -> renderUnqualifiedName name

sourceTypeFamilyName :: TypeFamilyDecl -> Text
sourceTypeFamilyName familyDecl =
  case typeFamilyDeclHead familyDecl of
    TCon name _ -> renderName name
    _ -> renderSourceType (typeFamilyDeclHead familyDecl)

dataConNames :: DataConDecl -> [Text]
dataConNames con =
  case peelDataConAnn con of
    PrefixCon _ _ name _ -> [renderUnqualifiedName name]
    InfixCon _ _ _ name _ -> [renderUnqualifiedName name]
    RecordCon _ _ name fields -> renderUnqualifiedName name : concatMap fieldDeclNames fields
    GadtCon _ _ names body -> map renderUnqualifiedName names <> gadtBodyFieldNames body
    TupleCon {} -> []
    UnboxedSumCon {} -> []
    ListCon {} -> ["[]"]
    DataConAnn {} -> []

gadtBodyFieldNames :: GadtBody -> [Text]
gadtBodyFieldNames body =
  case body of
    GadtPrefixBody {} -> []
    GadtRecordBody fields _ -> concatMap fieldDeclNames fields

fieldDeclNames :: FieldDecl -> [Text]
fieldDeclNames field =
  map renderUnqualifiedName (fieldNames field)

renderSourceType :: Type -> Text
renderSourceType =
  T.pack . renderString . layoutPretty defaultLayoutOptions . prettyType

-- | Query @ghc-pkg@ for package information.
--
-- Returns (package-id, import-dir, [exposed-module-names]).
queryPackage :: String -> IO (String, FilePath, [String])
queryPackage pkgName = do
  pkgId <- trim <$> readGhcPkg ["field", pkgName, "id", "--simple-output"]
  importDir <- trim <$> readGhcPkg ["field", pkgName, "import-dirs", "--simple-output"]
  exposedModsRaw <- readGhcPkg ["field", pkgName, "exposed-modules", "--simple-output"]
  let exposedMods = map stripComma (words exposedModsRaw)
  pure (pkgId, importDir, exposedMods)

queryPackageMaybe :: String -> IO (Maybe (String, FilePath, [String]))
queryPackageMaybe pkgName = do
  mPkgId <- fmap trim <$> tryReadGhcPkg ["field", pkgName, "id", "--simple-output"]
  mImportDir <- fmap trim <$> tryReadGhcPkg ["field", pkgName, "import-dirs", "--simple-output"]
  mExposedModsRaw <- tryReadGhcPkg ["field", pkgName, "exposed-modules", "--simple-output"]
  pure $ do
    pkgId <- mPkgId
    importDir <- mImportDir
    exposedModsRaw <- mExposedModsRaw
    let exposedMods = map stripComma (words exposedModsRaw)
    pure (pkgId, importDir, exposedMods)

-- | Query @ghc-pkg@ for a package's import directory by unit id.
queryImportDir :: String -> IO (Maybe FilePath)
queryImportDir unitId = do
  resultByUnitId <- tryReadGhcPkg ["--ipid", "field", unitId, "import-dirs", "--simple-output"]
  case resultByUnitId of
    Just dir -> pure (Just (trim dir))
    Nothing -> do
      resultByPackageId <- tryReadGhcPkg ["field", unitId, "import-dirs", "--simple-output"]
      pure (trim <$> resultByPackageId)

readGhcPkg :: [String] -> IO String
readGhcPkg args = do
  result <- tryReadGhcPkgRaw args
  case result of
    Just output -> pure output
    Nothing -> do
      mPackageDb <- findLocalPackageDb
      case mPackageDb of
        Nothing -> missingPackage
        Just packageDb -> do
          fallbackResult <- tryReadGhcPkgRaw ("--package-db" : packageDb : args)
          maybe missingPackage pure fallbackResult
  where
    missingPackage = ioError (userError ("ghc-pkg failed: " <> unwords args))

tryReadGhcPkg :: [String] -> IO (Maybe String)
tryReadGhcPkg args =
  (Just <$> readGhcPkg args)
    `catch` (\(_ :: IOException) -> pure Nothing)

tryReadGhcPkgRaw :: [String] -> IO (Maybe String)
tryReadGhcPkgRaw args = do
  (code, output, _) <- readProcessWithExitCode "ghc-pkg" args ""
  pure $
    case code of
      ExitSuccess -> Just output
      ExitFailure _ -> Nothing

findLocalPackageDb :: IO (Maybe FilePath)
findLocalPackageDb = do
  cwd <- getCurrentDirectory
  ghcVersion <- trim <$> readProcess "ghc" ["--numeric-version"] ""
  findAncestorContaining ("dist-newstyle" </> "packagedb" </> ("ghc-" <> ghcVersion)) cwd
  where
    findAncestorContaining rel dir = do
      let packageDb = dir </> rel
      exists <- doesDirectoryExist packageDb
      if exists
        then pure (Just packageDb)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure Nothing
            else findAncestorContaining rel parent

-- | Strip trailing commas from ghc-pkg output tokens.
stripComma :: String -> String
stripComma s = case reverse s of
  ',' : rest -> reverse rest
  _ -> s

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

dotToSlash :: Char -> Char
dotToSlash '.' = '/'
dotToSlash c = c

pathSepToDot :: Char -> Char
pathSepToDot '/' = '.'
pathSepToDot c = c
