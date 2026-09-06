{-# LANGUAGE OverloadedStrings #-}

module Aihc.Resolve.Scope
  ( Scope (..),
    OperatorFixity (..),
    ModuleExports,
    isTermNamespace,
    ModuleKey (..),
    collectModuleExports,
    collectModuleExportsWithDeps,
    moduleScope,
    moduleKey,
    matchingModuleScopes,
    lookupImportedModule,
    emptyScope,
    unionScope,
    insertTerm,
    insertType,
    lookupTerm,
    lookupType,
    lookupFixity,
    resolveTermName,
    resolveTypeName,
    resolveFixityName,
    collectPatVarBinders,
    tupleConName,
    unboxedSumConName,
    listConName,
    importItemTypeName,
  )
where

import Aihc.Parser.Syntax
  ( BinderHead,
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    DataFamilyDecl (..),
    DataFamilyInst (..),
    Decl (..),
    ExportSpec (..),
    Extension (..),
    FieldDecl (..),
    FixityAssoc (..),
    ForeignDecl (..),
    ForeignDirection (..),
    GadtBody (..),
    IEBundledMember (..),
    IEEntityNamespace (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    PatSynArgs (..),
    PatSynDecl (..),
    Pattern (..),
    RecordField (..),
    SourceSpan (..),
    TupleFlavor (..),
    Type (..),
    TypeFamilyDecl (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    binderHeadName,
    mkUnqualifiedName,
    moduleExports,
    moduleName,
    peelPatternAnn,
    peelTypeHead,
    qualifyName,
    recordFieldValue,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Span (spanStartNameSpan)
import Aihc.Resolve.Types
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName,
    scopeConstructors :: Map.Map Text [Text],
    scopeRecordFields :: Map.Map Text [Text],
    scopeMethods :: Map.Map Text [Text],
    -- | Associated type families of a class, keyed by the class name.
    scopeAssociatedTypes :: Map.Map Text [Text],
    scopeFixities :: Map.Map Text OperatorFixity,
    scopeQualifiedModules :: Map.Map Text Scope
  }
  deriving (Eq)

data OperatorFixity = OperatorFixity
  { operatorFixityAssoc :: !FixityAssoc,
    operatorFixityPrecedence :: !Int
  }
  deriving (Eq, Show, Read)

data ModuleKey = ModuleKey
  { moduleKeyPackage :: !Package,
    moduleKeyName :: !Text
  }
  deriving (Eq, Ord, Show)

type ModuleExports = Map.Map ModuleKey Scope

collectModuleExports :: [(Package, Module)] -> ModuleExports
collectModuleExports = collectModuleExportsWithDeps Map.empty

-- | Extract interfaces for a compilation unit while allowing its explicit
-- export lists to re-export names supplied by predecessor units.
collectModuleExportsWithDeps :: ModuleExports -> [(Package, Module)] -> ModuleExports
collectModuleExportsWithDeps depExports packageModules = Map.restrictKeys (closeExports initialExports) moduleKeys
  where
    moduleKeys = Map.keysSet localExports
    localExports =
      Map.fromList
        [ (exportKey package modu, emptyScope)
        | (package, modu) <- packageModules
        ]
    initialExports =
      localExports `Map.union` depExports

    closeExports exports =
      let exports' =
            Map.fromList [(exportKey package modu, exportedScope package exports modu) | (package, modu) <- packageModules]
              `Map.union` depExports
       in if exports' == exports then exports else closeExports exports'
    exportKey package modu = ModuleKey package (moduleKey modu)

exportedScope :: Package -> ModuleExports -> Module -> Scope
exportedScope package exports modu =
  case moduleExports modu of
    Nothing -> topLevelScope package modu
    Just specs -> List.foldl' unionScope emptyScope (map exportSpecScope specs)
  where
    availableScope = topLevelScope package modu `unionScope` importedScope package exports modu

    exportSpecScope spec =
      case spec of
        ExportAnn _ inner -> exportSpecScope inner
        ExportModule _ exportModuleName
          | exportModuleName == moduleKey modu -> topLevelScope package modu
          | otherwise -> lookupImportedModule package Nothing exportModuleName exports
        ExportVar _ _ name -> selectTerm (nameText name) (exportSource name)
        ExportAbs _ (Just namespace) name
          | isTermNamespace namespace -> selectTerm (nameText name) (exportSource name)
        ExportAbs _ _ name -> selectType (nameText name) (exportSource name)
        ExportAll _ _ name -> selectTypeWithMembers (nameText name) (exportSource name) (allTypeMembers (nameText name) (exportSource name))
        ExportWith _ _ name members -> selectTypeWithMembers (nameText name) (exportSource name) (map exportBundledMemberName members)
        ExportWithAll _ _ name _ members ->
          selectTypeWithMembers (nameText name) (exportSource name) (map exportBundledMemberName members <> allTypeMembers (nameText name) (exportSource name))

    -- A qualified export item such as @L.smallChunkSize@ names an entity
    -- of the module that the qualifier imports. The module can also use
    -- its own name as the qualifier.
    exportSource name =
      case nameQualifier name of
        Nothing -> availableScope
        Just qualifier
          | qualifier == moduleKey modu -> availableScope
          | otherwise -> Map.findWithDefault availableScope qualifier (scopeQualifiedModules availableScope)

selectTerm :: Text -> Scope -> Scope
selectTerm name scope =
  emptyScope
    { scopeTerms = Map.filterWithKey (\n _ -> n == name) (scopeTerms scope),
      scopeFixities = Map.filterWithKey (\n _ -> n == name) (scopeFixities scope)
    }

selectType :: Text -> Scope -> Scope
selectType name scope =
  emptyScope
    { scopeTypes = Map.filterWithKey (\n _ -> n == name) (scopeTypes scope)
    }

-- | Select a type with its bundled members. A bundled member that is a
-- term but not a constructor, a record field, or a method of the type is a
-- pattern synonym. It becomes a constructor of the exported type.
selectTypeWithMembers :: Text -> Scope -> [Text] -> Scope
selectTypeWithMembers name scope members =
  selectType name scope
    `unionScope` emptyScope
      { scopeTerms = Map.filterWithKey (\n _ -> n `elem` members) (scopeTerms scope),
        scopeTypes = Map.filterWithKey (\n _ -> n `elem` bundledAssociatedTypes) (scopeTypes scope),
        scopeConstructors = bundledConstructors,
        scopeRecordFields = Map.filterWithKey (\n _ -> n `elem` members) (scopeRecordFields scope),
        scopeMethods = Map.filterWithKey (\n _ -> n == name) (scopeMethods scope),
        scopeAssociatedTypes =
          if null bundledAssociatedTypes then Map.empty else Map.singleton name bundledAssociatedTypes,
        scopeFixities = Map.filterWithKey (\n _ -> n `elem` members) (scopeFixities scope)
      }
  where
    existingConstructors = Map.findWithDefault [] name (scopeConstructors scope)
    bundledAssociatedTypes =
      [member | member <- associatedTypeMembers name scope, member `elem` members]
    knownMembers =
      existingConstructors
        <> concat (Map.elems (scopeRecordFields scope))
        <> concat (Map.elems (scopeMethods scope))
        <> bundledAssociatedTypes
    bundledPatternSynonyms =
      List.nub [member | member <- members, member `notElem` knownMembers, Map.member member (scopeTerms scope)]
    bundledConstructors
      | Map.member name (scopeConstructors scope) || not (null bundledPatternSynonyms) =
          Map.singleton name (existingConstructors <> bundledPatternSynonyms)
      | otherwise = Map.empty

allTypeMembers :: Text -> Scope -> [Text]
allTypeMembers name scope =
  constructors <> recordFields <> methods <> associatedTypeMembers name scope
  where
    constructors = Map.findWithDefault [] name (scopeConstructors scope)
    recordFields = concatMap (\constructor -> Map.findWithDefault [] constructor (scopeRecordFields scope)) constructors
    methods = Map.findWithDefault [] name (scopeMethods scope)

-- | The associated type families that a class bundles in an export or
-- import item such as @C(..)@.
associatedTypeMembers :: Text -> Scope -> [Text]
associatedTypeMembers name scope = Map.findWithDefault [] name (scopeAssociatedTypes scope)

-- | The @pattern@ and @data@ namespace keywords select a term in an import
-- or export list.
isTermNamespace :: IEEntityNamespace -> Bool
isTermNamespace namespace =
  case namespace of
    IEEntityNamespaceType -> False
    IEEntityNamespacePattern -> True
    IEEntityNamespaceData -> True

exportBundledMemberName :: IEBundledMember -> Text
exportBundledMemberName = nameText . ieBundledMemberName

topLevelScope :: Package -> Module -> Scope
topLevelScope package modu =
  List.foldl' addDecl emptyScope (moduleDecls modu)
  where
    moduleKeyText = moduleKey modu
    qualify = ResolvedTopLevel (packageId package) . qualifyName (Just moduleKeyText)
    addDecl scope decl =
      let DeclExports termNames typeNames constructors recordFields methods associatedTypes fixities = declExportedNames decl
          scope' = List.foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
          scope'' = List.foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames
          scope''' = scope'' {scopeConstructors = constructors `Map.union` scopeConstructors scope''}
          scope'''' = scope''' {scopeRecordFields = recordFields `Map.union` scopeRecordFields scope'''}
          scope''''' = scope'''' {scopeMethods = methods `Map.union` scopeMethods scope''''}
          scope'''''' = scope''''' {scopeAssociatedTypes = associatedTypes `Map.union` scopeAssociatedTypes scope'''''}
       in scope'''''' {scopeFixities = fixities `Map.union` scopeFixities scope''''''}

-- | The names that one declaration adds to the module scope: terms, types,
-- constructors by type, record fields by constructor, methods by class,
-- associated type families by class, and operator fixities.
data DeclExports = DeclExports [UnqualifiedName] [UnqualifiedName] (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text OperatorFixity)

declExportedNames :: Decl -> DeclExports
declExportedNames decl =
  case decl of
    DeclAnn _ inner -> declExportedNames inner
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind name _ -> DeclExports [name] [] Map.empty Map.empty Map.empty Map.empty Map.empty
        PatternBind _ pat _ ->
          DeclExports (map snd (collectPatVarBinders NoSourceSpan pat)) [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclTypeSig names _ -> DeclExports names [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclForeign foreignDecl
      | foreignDirection foreignDecl == ForeignImport ->
          DeclExports [foreignName foreignDecl] [] Map.empty Map.empty Map.empty Map.empty Map.empty
      | otherwise -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclFixity assoc mNamespace mPrec ops
      | mNamespace /= Just IEEntityNamespaceType ->
          DeclExports
            []
            []
            Map.empty
            Map.empty
            Map.empty
            Map.empty
            (Map.fromList [(renderUnqualifiedName op, OperatorFixity assoc (fromMaybe 9 mPrec)) | op <- ops])
      | otherwise -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclClass classDecl ->
      let className = binderHeadName (classDeclHead classDecl)
          methodNames = classDeclMethodNames (classDeclItems classDecl)
          associatedNames = classDeclAssociatedTypeNames (classDeclItems classDecl)
       in DeclExports
            methodNames
            (className : associatedNames)
            Map.empty
            Map.empty
            (Map.singleton (renderUnqualifiedName className) (map renderUnqualifiedName methodNames))
            (Map.singleton (renderUnqualifiedName className) (map renderUnqualifiedName associatedNames))
            Map.empty
    DeclTypeData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl ->
      let typeName = binderHeadName (newtypeDeclHead newtypeDecl)
          termNames = maybe [] dataConDeclNames (newtypeDeclConstructor newtypeDecl)
          constructorNames = maybe [] dataConDeclConstructorNames (newtypeDeclConstructor newtypeDecl)
       in DeclExports termNames [typeName] (constructorMap typeName constructorNames) (maybe Map.empty (recordFieldMap . (: [])) (newtypeDeclConstructor newtypeDecl)) Map.empty Map.empty Map.empty
    DeclDataFamilyDecl familyDecl ->
      DeclExports [] [binderHeadName (dataFamilyDeclHead familyDecl)] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclDataFamilyInst familyInst -> dataFamilyInstExports familyInst
    DeclTypeFamilyDecl familyDecl ->
      case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of
        Just name -> DeclExports [] [name] Map.empty Map.empty Map.empty Map.empty Map.empty
        Nothing -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclTypeSyn typeSynDecl -> DeclExports [] [binderHeadName (typeSynHead typeSynDecl)] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclPatSyn patSyn ->
      let name = patSynDeclName patSyn
          fields = patSynFieldNames patSyn
       in DeclExports (name : fields) [] Map.empty (patSynRecordFieldMap name fields) Map.empty Map.empty Map.empty
    DeclPatSynSig names _ -> DeclExports names [] Map.empty Map.empty Map.empty Map.empty Map.empty
    _ -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty

-- | The field selectors of a record pattern synonym.
patSynFieldNames :: PatSynDecl -> [UnqualifiedName]
patSynFieldNames patSyn =
  case patSynDeclArgs patSyn of
    PatSynRecordArgs fields -> map (mkUnqualifiedName NameVarId) fields
    _ -> []

patSynRecordFieldMap :: UnqualifiedName -> [UnqualifiedName] -> Map.Map Text [Text]
patSynRecordFieldMap name fields
  | null fields = Map.empty
  | otherwise = Map.singleton (renderUnqualifiedName name) (map renderUnqualifiedName fields)

dataFamilyInstExports :: DataFamilyInst -> DeclExports
dataFamilyInstExports familyInst =
  case typeFamilyHeadName (dataFamilyInstHead familyInst) of
    Nothing -> DeclExports termNames [] Map.empty recordFields Map.empty Map.empty Map.empty
    Just familyName ->
      DeclExports
        termNames
        []
        (constructorMap familyName constructorNames)
        recordFields
        Map.empty
        Map.empty
        Map.empty
  where
    constructors = dataFamilyInstConstructors familyInst
    termNames = dataDeclConstructorNames constructors
    constructorNames = concatMap dataConDeclConstructorNames constructors
    recordFields = recordFieldMap constructors

typeFamilyHeadName :: Type -> Maybe UnqualifiedName
typeFamilyHeadName ty =
  case peelTypeHead ty of
    TCon name _ -> Just (mkUnqualifiedName (nameType name) (nameText name))
    TInfix _ name _ _ -> Just (mkUnqualifiedName (nameType name) (nameText name))
    TApp function _ -> typeFamilyHeadName function
    TTypeApp function _ -> typeFamilyHeadName function
    _ -> Nothing

dataDeclExports :: BinderHead UnqualifiedName -> [DataConDecl] -> DeclExports
dataDeclExports headBinder constructors =
  let typeName = binderHeadName headBinder
   in DeclExports
        (dataDeclConstructorNames constructors)
        [typeName]
        (constructorMap typeName (concatMap dataConDeclConstructorNames constructors))
        (recordFieldMap constructors)
        Map.empty
        Map.empty
        Map.empty

constructorMap :: UnqualifiedName -> [UnqualifiedName] -> Map.Map Text [Text]
constructorMap typeName constructors =
  Map.singleton (renderUnqualifiedName typeName) (map renderUnqualifiedName constructors)

recordFieldMap :: [DataConDecl] -> Map.Map Text [Text]
recordFieldMap constructors =
  Map.fromList
    [ (renderUnqualifiedName conName, concatMap (map renderUnqualifiedName . fieldNames) fields)
    | (conName, fields) <- concatMap dataConDeclRecordFields constructors
    ]

classDeclMethodNames :: [ClassDeclItem] -> [UnqualifiedName]
classDeclMethodNames = concatMap go
  where
    go (ClassItemAnn _ inner) = go inner
    go (ClassItemTypeSig names _) = names
    go (ClassItemDefaultSig name _) = [name]
    go _ = []

-- | The associated type and data families that a class declares.
classDeclAssociatedTypeNames :: [ClassDeclItem] -> [UnqualifiedName]
classDeclAssociatedTypeNames = concatMap go
  where
    go (ClassItemAnn _ inner) = go inner
    go (ClassItemTypeFamilyDecl familyDecl) = maybeToList (typeFamilyHeadName (typeFamilyDeclHead familyDecl))
    go (ClassItemDataFamilyDecl familyDecl) = [binderHeadName (dataFamilyDeclHead familyDecl)]
    go _ = []

dataDeclConstructorNames :: [DataConDecl] -> [UnqualifiedName]
dataDeclConstructorNames = concatMap dataConDeclNames

dataConDeclNames :: DataConDecl -> [UnqualifiedName]
dataConDeclNames dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          PrefixCon _ _ name _ -> [name]
          InfixCon _ _ _ name _ -> [name]
          RecordCon _ _ name fields -> name : concatMap fieldNames fields
          GadtCon _ _ names (GadtRecordBody fields _) -> names <> concatMap fieldNames fields
          GadtCon _ _ names _ -> names
          TupleCon _ _ flavor fields -> [tupleConName flavor (length fields)]
          UnboxedSumCon _ _ pos arity _ -> [unboxedSumConName pos arity]
          ListCon {} -> [listConName]
   in go dataConDecl

dataConDeclConstructorNames :: DataConDecl -> [UnqualifiedName]
dataConDeclConstructorNames dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          PrefixCon _ _ name _ -> [name]
          InfixCon _ _ _ name _ -> [name]
          RecordCon _ _ name _ -> [name]
          GadtCon _ _ names _ -> names
          TupleCon _ _ flavor fields -> [tupleConName flavor (length fields)]
          UnboxedSumCon _ _ pos arity _ -> [unboxedSumConName pos arity]
          ListCon {} -> [listConName]
   in go dataConDecl

dataConDeclRecordFields :: DataConDecl -> [(UnqualifiedName, [FieldDecl])]
dataConDeclRecordFields dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          RecordCon _ _ name fields -> [(name, fields)]
          GadtCon _ _ names (GadtRecordBody fields _) -> [(name, fields) | name <- names]
          _ -> []
   in go dataConDecl

tupleConName :: TupleFlavor -> Int -> UnqualifiedName
tupleConName flavor arity =
  mkUnqualifiedName NameConSym $ case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

unboxedSumConName :: Int -> Int -> UnqualifiedName
unboxedSumConName pos arity =
  mkUnqualifiedName NameConSym ("(#" <> bars (pos - 1) <> "_" <> bars (arity - pos) <> "#)")

listConName :: UnqualifiedName
listConName = mkUnqualifiedName NameConSym "[]"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = T.replicate (n - 1) ","

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = T.replicate n "|"

moduleScope :: Package -> ModuleExports -> Module -> Scope
moduleScope packageId exports modu =
  ownScope
    `unionScope` importedScope packageId exports modu
    `unionScope` implicitPrelude
    `unionScope` listConstructorScope
    `unionScope` builtinScope
  where
    -- A module's own top-level names are also in scope qualified by the
    -- module name, so @M.x@ inside module @M@ names the local @x@.
    ownScope = insertQualifiedModule (moduleKey modu) unqualifiedOwnScope unqualifiedOwnScope
    unqualifiedOwnScope = topLevelScope packageId modu
    preludeScope = lookupImportedModule packageId Nothing "Prelude" exports
    -- Implicit Prelude: names available unqualified AND as Prelude.xxx
    implicitPrelude
      | moduleImportsImplicitPrelude modu = preludeScope {scopeQualifiedModules = Map.singleton "Prelude" preludeScope}
      | otherwise = emptyScope
    ghcTypesScope = lookupImportedModule packageId Nothing "GHC.Types" exports
    listConstructorScope = selectTerm ":" ghcTypesScope `unionScope` selectTerm "[]" ghcTypesScope

-- | Whether the module gets the implicit Prelude import.
--
-- NoImplicitPrelude removes the implicit import.
-- RebindableSyntax implies NoImplicitPrelude.
-- An explicit Prelude import replaces the implicit import.
moduleImportsImplicitPrelude :: Module -> Bool
moduleImportsImplicitPrelude modu =
  ImplicitPrelude `elem` extensions && not explicitPreludeImport
  where
    extensions =
      applyImpliedExtensions (foldr applyExtensionSetting [ImplicitPrelude] (moduleLanguagePragmas modu))
    explicitPreludeImport = any ((== "Prelude") . importDeclModule) (moduleImports modu)

importedScope :: Package -> ModuleExports -> Module -> Scope
importedScope packageId exports modu =
  List.foldl' addImport emptyScope (moduleImports modu)
  where
    addImport acc importDecl
      | importDeclQualified importDecl || importDeclQualifiedPost importDecl =
          insertQualifiedModule qualifier imported acc
      | otherwise =
          let qualifiedAcc = insertQualifiedModule qualifier imported acc
           in unionScope qualifiedAcc imported
      where
        originModule = importDeclModule importDecl
        qualifier = fromMaybe originModule (importDeclAs importDecl)
        imported = filterImportSpec (importDeclSpec importDecl) (lookupImportedModule packageId (importDeclPackage importDecl) originModule exports)

lookupImportedModule :: Package -> Maybe Text -> Text -> ModuleExports -> Scope
lookupImportedModule currentPackage requestedPackage moduleName' exports =
  case matchingScopes of
    [scope] -> scope
    _ -> emptyScope
  where
    matchingScopes = matchingModuleScopes currentPackage requestedPackage moduleName' exports

matchingModuleScopes :: Package -> Maybe Text -> Text -> ModuleExports -> [Scope]
matchingModuleScopes currentPackage requestedPackage moduleName' exports =
  [ scope
  | (ModuleKey package name, scope) <- Map.toList exports,
    name == moduleName',
    packageMatches package
  ]
  where
    packageMatches package = case requestedPackage of
      Nothing -> True
      Just "this" -> package == currentPackage
      Just requested -> requested == packageName package

filterImportSpec :: Maybe ImportSpec -> Scope -> Scope
filterImportSpec maybeSpec scope =
  case maybeSpec of
    Nothing -> scope
    Just ImportSpec {importSpecHiding = False, importSpecItems} ->
      let allowedTypes = allowedTypeNames scope importSpecItems
          allowedTerms = allowedTermNames scope importSpecItems
       in Scope
            { scopeTerms =
                Map.filterWithKey (\n _ -> n `elem` allowedTerms) (scopeTerms scope),
              scopeTypes = Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeTypes scope),
              scopeConstructors = Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeConstructors scope),
              scopeRecordFields = Map.filterWithKey (\n _ -> n `elem` allowedTerms) (scopeRecordFields scope),
              scopeMethods = Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeMethods scope),
              scopeAssociatedTypes = Map.map (filter (`elem` allowedTypes)) (Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeAssociatedTypes scope)),
              scopeFixities = Map.filterWithKey (\n _ -> n `elem` allowedTerms) (scopeFixities scope),
              scopeQualifiedModules = scopeQualifiedModules scope
            }
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      filterScopeByNames (`notElem` (allowedTypeNames scope importSpecItems <> allowedTermNames scope importSpecItems)) scope

-- | The type names that an import list admits. A bundled member of a class
-- item that is an associated type family of the class is a type name.
allowedTypeNames :: Scope -> [ImportItem] -> [Text]
allowedTypeNames scope = concatMap allowedTypeNamesForItem
  where
    allowedTypeNamesForItem item =
      case importItemTypeName item of
        Nothing -> []
        Just itemName ->
          let parentName = renderUnqualifiedName itemName
              associated = associatedTypeMembers parentName scope
           in parentName : filter (`elem` associated) (bundledImportMembers scope item)

allowedTermNames :: Scope -> [ImportItem] -> [Text]
allowedTermNames scope = concatMap (allowedTermNamesForItem scope)

allowedTermNamesForItem :: Scope -> ImportItem -> [Text]
allowedTermNamesForItem scope item =
  case item of
    ImportAnn _ sub -> allowedTermNamesForItem scope sub
    ImportItemVar _ itemName -> [renderUnqualifiedName itemName]
    ImportItemAbs (Just namespace) itemName
      | isTermNamespace namespace -> [renderUnqualifiedName itemName]
    ImportItemAbs {} -> []
    _ ->
      case importItemTypeName item of
        Nothing -> []
        Just itemName ->
          let associated = associatedTypeMembers (renderUnqualifiedName itemName) scope
           in filter (`notElem` associated) (bundledImportMembers scope item)

-- | The bundled members that an import item names, with @(..)@ expanded
-- to every member of the parent. A parent without members stands for
-- itself.
bundledImportMembers :: Scope -> ImportItem -> [Text]
bundledImportMembers scope item =
  case item of
    ImportAnn _ sub -> bundledImportMembers scope sub
    ImportItemAll _ itemName -> allBundledMembers itemName
    ImportItemWith _ _ members -> map bundledMemberName members
    ImportItemAllWith _ itemName _ members -> map bundledMemberName members <> allBundledMembers itemName
    _ -> []
  where
    bundledMemberName = nameText . ieBundledMemberName
    allBundledMembers itemName =
      let parentName = renderUnqualifiedName itemName
          members = allTypeMembers parentName scope
       in if null members then [parentName] else members

importItemTypeName :: ImportItem -> Maybe UnqualifiedName
importItemTypeName item =
  case item of
    ImportAnn _ sub -> importItemTypeName sub
    ImportItemVar {} -> Nothing
    ImportItemAbs (Just namespace) _
      | isTermNamespace namespace -> Nothing
    ImportItemAbs _ itemName -> Just itemName
    ImportItemAll _ itemName -> Just itemName
    ImportItemWith _ itemName _ -> Just itemName
    ImportItemAllWith _ itemName _ _ -> Just itemName

resolveTermName :: Scope -> Name -> ResolvedName
resolveTermName scope name =
  case nameQualifier name of
    Just qualifier ->
      resolveQualifiedName scope lookupTerm qualifier name
    Nothing ->
      lookupTerm (nameText name) scope

resolveTypeName :: Scope -> Name -> ResolvedName
resolveTypeName scope name =
  case nameQualifier name of
    Just qualifier ->
      resolveQualifiedName scope lookupType qualifier name
    Nothing ->
      lookupType (nameText name) scope

resolveQualifiedName :: Scope -> (Text -> Scope -> ResolvedName) -> Text -> Name -> ResolvedName
resolveQualifiedName scope lookupName qualifier name =
  case Map.lookup qualifier (scopeQualifiedModules scope) of
    Nothing -> ResolvedError ("unknown qualified import: " <> T.unpack qualifier)
    Just qualifiedScope ->
      case lookupName (nameText name) qualifiedScope of
        ResolvedTopLevel packageId resolved -> ResolvedTopLevel packageId resolved
        other -> other

moduleKey :: Module -> Text
moduleKey modu = fromMaybe (T.pack "Main") (moduleName modu)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Scope containing fixed Haskell names that are always available.
--
-- The term namespace is empty.
-- Promoted constructors from @aihc-prim@ use ordinary name resolution.
-- The type namespace contains only the function arrow.
-- Types from @aihc-prim@ use ordinary name resolution.
--
-- This scope is merged into every module's scope unconditionally (lowest
-- priority — user-defined and imported names shadow it).
builtinScope :: Scope
builtinScope =
  Scope
    { scopeTerms = Map.empty,
      scopeTypes = Map.fromList (map mkBuiltinType builtinTypeNames),
      scopeConstructors = Map.empty,
      scopeRecordFields = Map.empty,
      scopeMethods = Map.empty,
      scopeAssociatedTypes = Map.empty,
      scopeFixities = Map.empty,
      scopeQualifiedModules = Map.empty
    }
  where
    mkBuiltinType n = (n, ResolvedSyntax)

-- | Wired-in type-namespace names.
--
-- Note: names here must match exactly what the parser emits as the 'Name'
-- text inside 'TCon'.  For example, the function arrow appears as @"->"@
-- (not @"(->)"@).
builtinTypeNames :: [T.Text]
builtinTypeNames =
  ["->"]

unionScope :: Scope -> Scope -> Scope
unionScope left right =
  Scope
    { scopeTerms = scopeTerms left `Map.union` scopeTerms right,
      scopeTypes = scopeTypes left `Map.union` scopeTypes right,
      scopeConstructors = scopeConstructors left `Map.union` scopeConstructors right,
      scopeRecordFields = scopeRecordFields left `Map.union` scopeRecordFields right,
      scopeMethods = scopeMethods left `Map.union` scopeMethods right,
      scopeAssociatedTypes = scopeAssociatedTypes left `Map.union` scopeAssociatedTypes right,
      scopeFixities = scopeFixities left `Map.union` scopeFixities right,
      scopeQualifiedModules = scopeQualifiedModules left `Map.union` scopeQualifiedModules right
    }

insertTerm :: Text -> ResolvedName -> Scope -> Scope
insertTerm name resolved scope = scope {scopeTerms = Map.insert name resolved (scopeTerms scope)}

insertType :: Text -> ResolvedName -> Scope -> Scope
insertType name resolved scope = scope {scopeTypes = Map.insert name resolved (scopeTypes scope)}

-- | Add names from one qualified import. Combine scopes that share an alias.
insertQualifiedModule :: Text -> Scope -> Scope -> Scope
insertQualifiedModule qualifier imported scope =
  scope
    { scopeQualifiedModules =
        Map.insertWith unionScope qualifier imported (scopeQualifiedModules scope)
    }

lookupTerm :: Text -> Scope -> ResolvedName
lookupTerm name scope =
  Map.findWithDefault
    (ResolvedError "unbound")
    name
    (scopeTerms scope)

lookupType :: Text -> Scope -> ResolvedName
lookupType name scope =
  Map.findWithDefault
    (ResolvedError "unbound")
    name
    (scopeTypes scope)

lookupFixity :: Text -> Scope -> OperatorFixity
lookupFixity name scope =
  Map.findWithDefault defaultOperatorFixity name (scopeFixities scope)

defaultOperatorFixity :: OperatorFixity
defaultOperatorFixity = OperatorFixity InfixL 9

filterScopeByNames :: (Text -> Bool) -> Scope -> Scope
filterScopeByNames keep scope =
  Scope
    { scopeTerms = Map.filterWithKey (\name _ -> keep name) (scopeTerms scope),
      scopeTypes = Map.filterWithKey (\name _ -> keep name) (scopeTypes scope),
      scopeConstructors = Map.filterWithKey (\name _ -> keep name) (scopeConstructors scope),
      scopeRecordFields = Map.filterWithKey (\name _ -> keep name) (scopeRecordFields scope),
      scopeMethods = Map.filterWithKey (\name _ -> keep name) (scopeMethods scope),
      scopeAssociatedTypes = Map.filterWithKey (\name _ -> keep name) (scopeAssociatedTypes scope),
      scopeFixities = Map.filterWithKey (\name _ -> keep name) (scopeFixities scope),
      scopeQualifiedModules = scopeQualifiedModules scope
    }

resolveFixityName :: Scope -> Name -> OperatorFixity
resolveFixityName scope name =
  case nameQualifier name of
    Just qualifier ->
      case Map.lookup qualifier (scopeQualifiedModules scope) of
        Nothing -> defaultOperatorFixity
        Just qualifiedScope -> lookupFixity (nameText name) qualifiedScope
    Nothing ->
      lookupFixity (nameText name) scope

collectPatVarBinders :: SourceSpan -> Pattern -> [(SourceSpan, UnqualifiedName)]
collectPatVarBinders ambient pat =
  case peelPatternAnn pat of
    PVar name -> [(spanStartNameSpan ambient (renderUnqualifiedName name), name)]
    PTuple _ pats -> concatMap (collectPatVarBinders ambient) pats
    PList pats -> concatMap (collectPatVarBinders ambient) pats
    PParen inner -> collectPatVarBinders ambient inner
    PAs alias inner ->
      (spanStartNameSpan ambient (renderUnqualifiedName alias), alias)
        : collectPatVarBinders ambient inner
    PStrict inner -> collectPatVarBinders ambient inner
    PIrrefutable inner -> collectPatVarBinders ambient inner
    PRecord _ fields _ -> concatMap (collectPatVarBinders ambient . recordFieldValue) fields
    PInfix left _ right ->
      collectPatVarBinders ambient left <> collectPatVarBinders ambient right
    PCon _ _ pats -> concatMap (collectPatVarBinders ambient) pats
    PBuiltinCon _ _ pats -> concatMap (collectPatVarBinders ambient) pats
    _ -> []
