{-# LANGUAGE OverloadedStrings #-}

-- | The installed package interface JSON format.
module Aihc.Cli.PackageInterface
  ( PackageInterface (..),
    PackageInterfaceBinding (..),
    PackageInterfaceDependency (..),
    PackageInterfaceDiagnostics (..),
    PackageInterfaceFlag (..),
    PackageInterfaceModule (..),
    PackageInterfacePackageKey (..),
    PackageInterfacePackageSpec (..),
    PackageInterfaceTcModule (..),
    packageInterfaceExports,
    packageInterfaceModulesFromExports,
    packageInterfacePackage,
    readPackageInterface,
    writePackageInterface,
  )
where

import Aihc.Parser.Syntax (qualifyName, unqualifiedNameFromText)
import Aihc.Resolve (ModuleExports, ModuleKey (..), Package (..), PackageId (..), ResolvedName (..), Scope (..))
import Aihc.Tc (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), renderTcType, tvKind)
import Aihc.Tc.Types (mkTyVarId, setTyVarKind)
import Control.Monad (when)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data PackageInterface = PackageInterface
  { packageInterfacePackageKey :: !PackageInterfacePackageKey,
    packageInterfaceStatus :: !Text,
    packageInterfaceContains :: ![Text],
    packageInterfaceSourceFiles :: ![FilePath],
    packageInterfaceModuleCount :: !Int,
    packageInterfaceModules :: ![PackageInterfaceModule],
    packageInterfaceDiagnostics :: !PackageInterfaceDiagnostics,
    packageInterfaceTypecheck :: ![PackageInterfaceTcModule]
  }
  deriving (Eq, Show)

data PackageInterfacePackageKey = PackageInterfacePackageKey
  { packageInterfaceKeyHash :: !Text,
    packageInterfaceKeyPackage :: !PackageInterfacePackageSpec,
    packageInterfaceKeyFlags :: ![PackageInterfaceFlag],
    packageInterfaceKeyDependencies :: ![PackageInterfaceDependency]
  }
  deriving (Eq, Show)

data PackageInterfacePackageSpec = PackageInterfacePackageSpec
  { packageInterfacePackageName :: !Text,
    packageInterfacePackageVersion :: !Text
  }
  deriving (Eq, Show)

data PackageInterfaceFlag = PackageInterfaceFlag
  { packageInterfaceFlagName :: !Text,
    packageInterfaceFlagEnabled :: !Bool
  }
  deriving (Eq, Show)

data PackageInterfaceDependency = PackageInterfaceDependency
  { packageInterfaceDependencyPackage :: !PackageInterfacePackageSpec,
    packageInterfaceDependencyHash :: !Text
  }
  deriving (Eq, Show)

data PackageInterfaceModule = PackageInterfaceModule
  { packageInterfaceModuleName :: !Text,
    packageInterfaceModuleTerms :: ![Text],
    packageInterfaceModuleTypes :: ![Text],
    packageInterfaceModuleConstructors :: !(Map.Map Text [Text]),
    packageInterfaceModuleRecordFields :: !(Map.Map Text [Text]),
    packageInterfaceModuleMethods :: !(Map.Map Text [Text]),
    -- | Associated type families by class name.
    packageInterfaceModuleAssociatedTypes :: !(Map.Map Text [Text])
  }
  deriving (Eq, Show)

data PackageInterfaceDiagnostics = PackageInterfaceDiagnostics
  { packageInterfaceCppDiagnostics :: ![Aeson.Value],
    packageInterfaceParseDiagnostics :: ![Aeson.Value],
    packageInterfaceResolveDiagnostics :: ![Aeson.Value],
    packageInterfaceTcDiagnostics :: ![Aeson.Value]
  }
  deriving (Eq, Show)

data PackageInterfaceTcModule = PackageInterfaceTcModule
  { packageInterfaceTcModuleName :: !Text,
    packageInterfaceTcModuleSuccess :: !Bool,
    packageInterfaceTcModuleBindings :: ![PackageInterfaceBinding],
    packageInterfaceTcModuleDiagnostics :: ![Aeson.Value]
  }
  deriving (Eq, Show)

data PackageInterfaceBinding = PackageInterfaceBinding
  { packageInterfaceBindingName :: !Text,
    packageInterfaceBindingType :: !TcType
  }
  deriving (Eq, Show)

packageInterfaceSchemaVersion :: Int
packageInterfaceSchemaVersion = 4

instance Aeson.ToJSON PackageInterface where
  toJSON interface =
    Aeson.object
      [ "schemaVersion" .= packageInterfaceSchemaVersion,
        "packageKey" .= packageInterfacePackageKey interface,
        "status" .= packageInterfaceStatus interface,
        "contains" .= packageInterfaceContains interface,
        "sourceFiles" .= packageInterfaceSourceFiles interface,
        "moduleCount" .= packageInterfaceModuleCount interface,
        "modules" .= packageInterfaceModules interface,
        "diagnostics" .= packageInterfaceDiagnostics interface,
        "typecheck" .= packageInterfaceTypecheck interface
      ]

instance Aeson.FromJSON PackageInterface where
  parseJSON =
    Aeson.withObject "package interface" $ \obj -> do
      schemaVersion <- obj .: "schemaVersion"
      when (schemaVersion /= packageInterfaceSchemaVersion) $
        fail ("unsupported package interface schema version: " <> show (schemaVersion :: Int))
      PackageInterface
        <$> obj .: "packageKey"
        <*> obj .: "status"
        <*> obj .: "contains"
        <*> obj .: "sourceFiles"
        <*> obj .: "moduleCount"
        <*> obj .: "modules"
        <*> obj .: "diagnostics"
        <*> obj .: "typecheck"

instance Aeson.ToJSON PackageInterfacePackageKey where
  toJSON key =
    Aeson.object
      [ "hash" .= packageInterfaceKeyHash key,
        "package" .= packageInterfaceKeyPackage key,
        "flags" .= packageInterfaceKeyFlags key,
        "dependencies" .= packageInterfaceKeyDependencies key
      ]

instance Aeson.FromJSON PackageInterfacePackageKey where
  parseJSON =
    Aeson.withObject "package key" $ \obj ->
      PackageInterfacePackageKey
        <$> obj .: "hash"
        <*> obj .: "package"
        <*> obj .: "flags"
        <*> obj .: "dependencies"

instance Aeson.ToJSON PackageInterfacePackageSpec where
  toJSON spec =
    Aeson.object
      [ "name" .= packageInterfacePackageName spec,
        "version" .= packageInterfacePackageVersion spec
      ]

instance Aeson.FromJSON PackageInterfacePackageSpec where
  parseJSON =
    Aeson.withObject "package" $ \obj ->
      PackageInterfacePackageSpec
        <$> obj .: "name"
        <*> obj .: "version"

instance Aeson.ToJSON PackageInterfaceFlag where
  toJSON flag =
    Aeson.object
      [ "name" .= packageInterfaceFlagName flag,
        "enabled" .= packageInterfaceFlagEnabled flag
      ]

instance Aeson.FromJSON PackageInterfaceFlag where
  parseJSON =
    Aeson.withObject "package flag" $ \obj ->
      PackageInterfaceFlag
        <$> obj .: "name"
        <*> obj .: "enabled"

instance Aeson.ToJSON PackageInterfaceDependency where
  toJSON dependency =
    Aeson.object
      [ "package" .= packageInterfaceDependencyPackage dependency,
        "hash" .= packageInterfaceDependencyHash dependency
      ]

instance Aeson.FromJSON PackageInterfaceDependency where
  parseJSON =
    Aeson.withObject "package dependency" $ \obj ->
      PackageInterfaceDependency
        <$> obj .: "package"
        <*> obj .: "hash"

instance Aeson.ToJSON PackageInterfaceModule where
  toJSON modu =
    Aeson.object
      [ "module" .= packageInterfaceModuleName modu,
        "terms" .= packageInterfaceModuleTerms modu,
        "types" .= packageInterfaceModuleTypes modu,
        "constructors" .= packageInterfaceModuleConstructors modu,
        "recordFields" .= packageInterfaceModuleRecordFields modu,
        "methods" .= packageInterfaceModuleMethods modu,
        "associatedTypes" .= packageInterfaceModuleAssociatedTypes modu
      ]

instance Aeson.FromJSON PackageInterfaceModule where
  parseJSON =
    Aeson.withObject "interface module" $ \obj ->
      PackageInterfaceModule
        <$> obj .: "module"
        <*> obj .: "terms"
        <*> obj .: "types"
        <*> obj .: "constructors"
        <*> obj .: "recordFields"
        <*> obj .: "methods"
        <*> (fromMaybe Map.empty <$> obj .:? "associatedTypes")

instance Aeson.ToJSON PackageInterfaceDiagnostics where
  toJSON diagnostics =
    Aeson.object
      [ "cpp" .= packageInterfaceCppDiagnostics diagnostics,
        "parse" .= packageInterfaceParseDiagnostics diagnostics,
        "resolve" .= packageInterfaceResolveDiagnostics diagnostics,
        "typecheck" .= packageInterfaceTcDiagnostics diagnostics
      ]

instance Aeson.FromJSON PackageInterfaceDiagnostics where
  parseJSON =
    Aeson.withObject "package interface diagnostics" $ \obj ->
      PackageInterfaceDiagnostics
        <$> obj .: "cpp"
        <*> obj .: "parse"
        <*> obj .: "resolve"
        <*> obj .: "typecheck"

instance Aeson.ToJSON PackageInterfaceTcModule where
  toJSON modu =
    Aeson.object
      [ "module" .= packageInterfaceTcModuleName modu,
        "success" .= packageInterfaceTcModuleSuccess modu,
        "bindings" .= packageInterfaceTcModuleBindings modu,
        "diagnostics" .= packageInterfaceTcModuleDiagnostics modu
      ]

instance Aeson.FromJSON PackageInterfaceTcModule where
  parseJSON =
    Aeson.withObject "typecheck module" $ \obj ->
      PackageInterfaceTcModule
        <$> obj .: "module"
        <*> obj .: "success"
        <*> obj .: "bindings"
        <*> obj .: "diagnostics"

instance Aeson.ToJSON PackageInterfaceBinding where
  toJSON binding =
    Aeson.object
      [ "name" .= packageInterfaceBindingName binding,
        "type" .= renderTcType (packageInterfaceBindingType binding),
        "typeJson" .= tcTypeValue (packageInterfaceBindingType binding)
      ]

instance Aeson.FromJSON PackageInterfaceBinding where
  parseJSON =
    Aeson.withObject "typecheck binding" $ \obj ->
      PackageInterfaceBinding
        <$> obj .: "name"
        <*> (obj .: "typeJson" >>= parseTcTypeJson)

readPackageInterface :: FilePath -> IO (Either String PackageInterface)
readPackageInterface path = Aeson.eitherDecode <$> BL.readFile path

writePackageInterface :: FilePath -> PackageInterface -> IO ()
writePackageInterface path = BL.writeFile path . Aeson.encode

packageInterfacePackage :: PackageInterfacePackageKey -> Package
packageInterfacePackage key =
  Package
    { packageName = packageInterfacePackageName spec,
      packageId = PackageId identity
    }
  where
    spec = packageInterfaceKeyPackage key
    identity =
      T.intercalate
        "-"
        ( T.splitOn "-" (packageInterfacePackageName spec)
            <> T.splitOn "." (packageInterfacePackageVersion spec)
            <> [packageInterfaceKeyHash key]
        )

packageInterfaceModulesFromExports :: ModuleExports -> [PackageInterfaceModule]
packageInterfaceModulesFromExports exports =
  [ PackageInterfaceModule
      { packageInterfaceModuleName = moduleKeyName moduleKey,
        packageInterfaceModuleTerms = Map.keys (scopeTerms scope),
        packageInterfaceModuleTypes = Map.keys (scopeTypes scope),
        packageInterfaceModuleConstructors = scopeConstructors scope,
        packageInterfaceModuleRecordFields = scopeRecordFields scope,
        packageInterfaceModuleMethods = scopeMethods scope,
        packageInterfaceModuleAssociatedTypes = scopeAssociatedTypes scope
      }
  | (moduleKey, scope) <- Map.toAscList exports
  ]

packageInterfaceExports :: PackageInterface -> ModuleExports
packageInterfaceExports interface =
  Map.fromList
    [ (ModuleKey package (packageInterfaceModuleName modu), packageInterfaceModuleScope package modu)
    | modu <- packageInterfaceModules interface
    ]
  where
    package = packageInterfacePackage (packageInterfacePackageKey interface)

packageInterfaceModuleScope :: Package -> PackageInterfaceModule -> Scope
packageInterfaceModuleScope package modu =
  Scope
    { scopeTerms =
        Map.fromList
          [ (name, resolvedTopLevel package (packageInterfaceModuleName modu) name)
          | name <- packageInterfaceModuleTerms modu
          ],
      scopeTypes =
        Map.fromList
          [ (name, resolvedTopLevel package (packageInterfaceModuleName modu) name)
          | name <- packageInterfaceModuleTypes modu
          ],
      scopeConstructors = packageInterfaceModuleConstructors modu,
      scopeRecordFields = packageInterfaceModuleRecordFields modu,
      scopeMethods = packageInterfaceModuleMethods modu,
      scopeAssociatedTypes = packageInterfaceModuleAssociatedTypes modu,
      scopeFixities = Map.empty,
      scopeQualifiedModules = Map.empty
    }

resolvedTopLevel :: Package -> Text -> Text -> ResolvedName
resolvedTopLevel package moduleName name =
  ResolvedTopLevel (packageId package) (qualifyName (Just moduleName) (unqualifiedNameFromText name))

tcTypeValue :: TcType -> Aeson.Value
tcTypeValue ty =
  case ty of
    TcTyVar tv ->
      Aeson.object
        [ "tag" .= ("var" :: Text),
          "name" .= tvName tv,
          "unique" .= uniqueValue (tvUnique tv),
          -- An occurrence carries its kind as a binder does. The reader
          -- knows no kind vocabulary, so it has nothing to default to.
          "kind" .= tcTypeValue (tvKind tv)
        ]
    TcMetaTv unique ->
      Aeson.object
        [ "tag" .= ("meta" :: Text),
          "unique" .= uniqueValue unique
        ]
    TcTyCon tyCon args ->
      Aeson.object
        [ "tag" .= ("con" :: Text),
          "tyCon" .= show tyCon,
          "args" .= map tcTypeValue args
        ]
    TcFunTy arg result ->
      Aeson.object
        [ "tag" .= ("fun" :: Text),
          "arg" .= tcTypeValue arg,
          "result" .= tcTypeValue result
        ]
    TcForAllTy tv body ->
      Aeson.object
        [ "tag" .= ("forall" :: Text),
          "binder" .= tyVarValue tv,
          "body" .= tcTypeValue body
        ]
    TcQualTy preds body ->
      Aeson.object
        [ "tag" .= ("qual" :: Text),
          "predicates" .= map predValue preds,
          "body" .= tcTypeValue body
        ]
    TcAppTy fun arg ->
      Aeson.object
        [ "tag" .= ("app" :: Text),
          "fun" .= tcTypeValue fun,
          "arg" .= tcTypeValue arg
        ]

parseTcTypeJson :: Aeson.Value -> AesonTypes.Parser TcType
parseTcTypeJson =
  Aeson.withObject "type" $ \obj -> do
    tag <- obj .: "tag" :: AesonTypes.Parser Text
    case tag of
      "var" -> TcTyVar <$> parseTyVarObject obj
      "meta" -> TcMetaTv . Unique <$> obj .: "unique"
      "con" ->
        TcTyCon
          <$> (obj .: "tyCon" >>= parseTyConText)
          <*> (obj .: "args" >>= traverse parseTcTypeJson)
      "fun" ->
        TcFunTy
          <$> (obj .: "arg" >>= parseTcTypeJson)
          <*> (obj .: "result" >>= parseTcTypeJson)
      "forall" ->
        TcForAllTy
          <$> (obj .: "binder" >>= parseTyVarValue)
          <*> (obj .: "body" >>= parseTcTypeJson)
      "qual" ->
        TcQualTy
          <$> (obj .: "predicates" >>= traverse parsePredJson)
          <*> (obj .: "body" >>= parseTcTypeJson)
      "app" ->
        TcAppTy
          <$> (obj .: "fun" >>= parseTcTypeJson)
          <*> (obj .: "arg" >>= parseTcTypeJson)
      other -> fail ("unknown type tag: " <> T.unpack other)

parseTyVarObject :: AesonTypes.Object -> AesonTypes.Parser TyVarId
parseTyVarObject obj =
  mkTyVarId
    <$> obj .: "name"
    <*> (Unique <$> obj .: "unique")
    <*> (obj .: "kind" >>= parseTcTypeJson)

parseTyVarValue :: Aeson.Value -> AesonTypes.Parser TyVarId
parseTyVarValue =
  Aeson.withObject "type variable" $ \obj -> do
    variable <- parseTyVarObject obj
    kind <- obj .: "kind" >>= parseTcTypeJson
    pure (setTyVarKind kind variable)

parsePredJson :: Aeson.Value -> AesonTypes.Parser Pred
parsePredJson =
  Aeson.withObject "predicate" $ \obj -> do
    tag <- obj .: "tag" :: AesonTypes.Parser Text
    case tag of
      "class" ->
        ClassPred
          <$> (obj .: "class" >>= parseTyConText)
          <*> (obj .: "args" >>= traverse parseTcTypeJson)
      "eq" ->
        EqPred
          <$> (obj .: "left" >>= parseTcTypeJson)
          <*> (obj .: "right" >>= parseTcTypeJson)
      "quantified" ->
        QuantifiedPred
          <$> (obj .: "variables" >>= traverse parseTyVarValue)
          <*> (obj .: "antecedents" >>= traverse parsePredJson)
          <*> (obj .: "consequent" >>= parsePredJson)
      "implicit" ->
        IParamPred
          <$> obj .: "name"
          <*> (obj .: "type" >>= parseTcTypeJson)
      other -> fail ("unknown predicate tag: " <> T.unpack other)

predValue :: Pred -> Aeson.Value
predValue pred' =
  case pred' of
    ClassPred cls args ->
      Aeson.object
        [ "tag" .= ("class" :: Text),
          "class" .= show cls,
          "args" .= map tcTypeValue args
        ]
    EqPred left right ->
      Aeson.object
        [ "tag" .= ("eq" :: Text),
          "left" .= tcTypeValue left,
          "right" .= tcTypeValue right
        ]
    IParamPred name payload ->
      Aeson.object
        [ "tag" .= ("implicit" :: Text),
          "name" .= name,
          "type" .= tcTypeValue payload
        ]
    QuantifiedPred variables antecedents consequent ->
      Aeson.object
        [ "tag" .= ("quantified" :: Text),
          "variables" .= map tyVarValue variables,
          "antecedents" .= map predValue antecedents,
          "consequent" .= predValue consequent
        ]

parseTyConText :: Text -> AesonTypes.Parser TyCon
parseTyConText encoded =
  case readMaybe (T.unpack encoded) of
    Just tyCon -> pure tyCon
    Nothing -> fail "invalid exact type constructor"

tyVarValue :: TyVarId -> Aeson.Value
tyVarValue tv =
  Aeson.object
    [ "name" .= tvName tv,
      "unique" .= uniqueValue (tvUnique tv),
      "kind" .= tcTypeValue (tvKind tv)
    ]

uniqueValue :: Unique -> Int
uniqueValue (Unique unique) = unique
