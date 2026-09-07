{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Resolve
  ( pattern DeclResolution,
    pattern EResolution,
    pattern ImportResolution,
    pattern PResolution,
    pattern TResolution,
    resolveWithDeps,
    extractInterface,
    extractInterfaceWithDeps,
    OperatorFixity (..),
    Scope (..),
    ModuleExports,
    ModuleKey (..),
    PackageId (..),
    Package (..),
    unnamedPackage,
    modulesInPackage,
    collectModuleExports,
    collectModuleExportsWithDeps,
    exportedLocalNames,
    lookupImportedModule,
    emptyScope,
    unionScope,
    ResolveError (..),
    ResolveResult (..),
    resolvedModuleAsts,
    ResolutionNamespace (..),
    Identifier (..),
    displayIdentifier,
    ResolvedName (..),
    ResolutionAnnotation (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ArrowKind (..),
    BangType (..),
    BinderHead (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    CompStmt (..),
    DataConDecl (..),
    DataDecl (..),
    DataFamilyDecl (..),
    DataFamilyInst (..),
    Decl (..),
    DerivingClause (..),
    DerivingStrategy (..),
    DoStmt (..),
    Expr (..),
    Extension (..),
    FieldDecl (..),
    FixityAssoc (..),
    FloatType (..),
    ForallTelescope (..),
    ForeignDecl (..),
    GadtBody (..),
    GuardQualifier (..),
    GuardedRhs (..),
    IEBundledMember (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    LambdaCaseAlt (..),
    Literal (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    NumericType (..),
    PatSynArgs (..),
    PatSynDecl (..),
    PatSynDir (..),
    Pattern (..),
    Pragma (..),
    PragmaType (..),
    RecordField (..),
    Rhs (..),
    RoleAnnotation (..),
    SourceSpan (..),
    StandaloneDerivingDecl (..),
    TyVarBinder (..),
    Type (..),
    TypeFamilyDecl (..),
    TypeFamilyEq (..),
    TypeFamilyInst (..),
    TypeFamilyResultSig (..),
    TypePromotion (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    fromAnnotation,
    mkAnnotation,
    mkUnqualifiedName,
    peelGuardQualifierAnn,
    peelLiteralAnn,
    peelPatternAnn,
    recordFieldName,
    recordFieldValue,
    renderUnqualifiedName,
    unqualifiedNameAnns,
    unqualifiedNameText,
  )
import Aihc.Resolve.Monad
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Traverse (HasAnnotations, annotationList)
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM, (<=<))
import Data.List (find, mapAccumL)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

-- | Every resolution error of a piece of syntax, in source order. All
-- errors live in resolution annotations, so one annotation walk finds them.
collectResolveErrors :: (HasAnnotations a) => a -> [ResolveError]
collectResolveErrors = mapMaybe (annotationResolveError <=< fromAnnotation) . annotationList

annotationResolveError :: ResolutionAnnotation -> Maybe ResolveError
annotationResolveError resolution =
  case resolutionTarget resolution of
    ResolvedError msg ->
      Just
        ResolveResolutionError
          { resolveErrorSpan = resolutionSpan resolution,
            resolveErrorName = displayIdentifier (resolutionIdentifier resolution),
            resolveErrorNamespace = resolutionNamespace resolution,
            resolveErrorMessage = msg
          }
    ResolvedSyntax -> Nothing
    _ -> Nothing

resolveWithDeps :: Scope -> ModuleExports -> [(Package, Module)] -> ResolveResult
resolveWithDeps builtinScope depExports packageModules =
  ResolveResult
    { resolvedModules = packageModules',
      resolveErrors = collectResolveErrors modules'
    }
  where
    step currentNextLocal (package, modu) =
      let (nextLocal', modu') = resolveModule builtinScope package exports currentNextLocal modu
       in (nextLocal', modu')
    (_, resolved) = mapAccumL step 0 packageModules
    modules' = resolved
    packageModules' = zip (map fst packageModules) modules'
    ownExports = collectModuleExportsWithDeps depExports packageModules
    exports = ownExports `Map.union` depExports

extractInterface :: ResolveResult -> ModuleExports
extractInterface = collectModuleExports . resolvedModules

extractInterfaceWithDeps :: ModuleExports -> ResolveResult -> ModuleExports
extractInterfaceWithDeps depExports = collectModuleExportsWithDeps depExports . resolvedModules

resolveModule :: Scope -> Package -> ModuleExports -> Int -> Module -> (Int, Module)
resolveModule builtinScope package exports nextLocal modu =
  let imports' = resolveModuleImports package exports (moduleImports modu)
      modu' = modu {moduleImports = imports'}
      scope = moduleScope package exports modu'
      (nextLocal', decls') =
        runResolveM
          scope
          (moduleInfo builtinScope modu')
          nextLocal
          (resolveBindingGroup (topLevelTermDefinition scope) Map.empty (moduleDecls modu))
   in (nextLocal', modu' {moduleDecls = decls'})

moduleInfo :: Scope -> Module -> ModuleInfo
moduleInfo builtinScope modu =
  ModuleInfo
    { moduleInfoExtensions =
        applyImpliedExtensions $
          foldr applyExtensionSetting [] (moduleLanguagePragmas modu),
      moduleInfoExplicitPreludeImport =
        any ((== "Prelude") . importDeclModule) (moduleImports modu),
      moduleInfoBuiltinScope = builtinScope
    }

resolveModuleImports :: Package -> ModuleExports -> [ImportDecl] -> [ImportDecl]
resolveModuleImports package exports =
  map resolveModuleImport
  where
    resolveModuleImport importDecl
      | [originScope] <- matches =
          annotateMissingImportItems originScope importDecl
      | null matches = annotateImport (missingModuleImportAnnotation "not found" importDecl) importDecl
      | otherwise = annotateImport (missingModuleImportAnnotation "ambiguous" importDecl) importDecl
      where
        matches = matchingModuleScopes package (importDeclPackage importDecl) (importDeclModule importDecl) exports

missingModuleImportAnnotation :: String -> ImportDecl -> ResolutionAnnotation
missingModuleImportAnnotation message importDecl =
  let importedModule = importDeclModule importDecl
   in ResolutionAnnotation
        (importModuleNameSpan importDecl)
        (IdentifierNamed importedModule)
        ResolutionNamespaceModule
        (ResolvedError message)

annotateMissingImportItems :: Scope -> ImportDecl -> ImportDecl
annotateMissingImportItems originScope importDecl =
  case importDeclSpec importDecl of
    Just importSpec@ImportSpec {importSpecHiding = False, importSpecItems} ->
      importDecl {importDeclSpec = Just importSpec {importSpecItems = map annotateItem importSpecItems}}
    _ -> importDecl
  where
    annotateItem item =
      case missingImportItemAnnotation originScope item of
        Nothing -> item
        Just annotation -> annotateImportItemError annotation item

annotateImportItemError :: ResolutionAnnotation -> ImportItem -> ImportItem
annotateImportItemError annotation item =
  -- Keep the diagnostic span as the carrier span for annotated-source overlays.
  ImportAnn (mkAnnotation annotation) (ImportAnn (mkAnnotation (resolutionSpan annotation)) item)

missingImportItemAnnotation :: Scope -> ImportItem -> Maybe ResolutionAnnotation
missingImportItemAnnotation originScope item =
  go item
  where
    go current =
      case current of
        ImportAnn _ sub -> go sub
        ImportItemVar _ itemName ->
          missingImportedName item ResolutionNamespaceTerm itemName (scopeTerms originScope)
        ImportItemAbs (Just namespace) itemName
          | isTermNamespace namespace ->
              missingImportedName item ResolutionNamespaceTerm itemName (scopeTerms originScope)
        ImportItemAbs _ itemName ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
        ImportItemAll _ itemName ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
        ImportItemWith _ itemName members ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
            <|> missingImportMemberAnnotation originScope item members
        ImportItemAllWith _ itemName _ members ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
            <|> missingImportMemberAnnotation originScope item members

missingImportMemberAnnotation :: Scope -> ImportItem -> [IEBundledMember] -> Maybe ResolutionAnnotation
missingImportMemberAnnotation originScope item members =
  missingMemberAnnotation <$> find missingMember members
  where
    missingMember member = nameText (ieBundledMemberName member) `notElem` exportedMembers
    exportedMembers =
      case importItemTypeName item of
        Nothing -> []
        Just itemName ->
          let parentName = renderUnqualifiedName itemName
           in Map.findWithDefault [] parentName (scopeConstructors originScope)
                <> Map.findWithDefault [] parentName (scopeMethods originScope)
                <> Map.findWithDefault [] parentName (scopeAssociatedTypes originScope)
    missingMemberAnnotation member =
      let memberName = nameText (ieBundledMemberName member)
       in ResolutionAnnotation
            (importMemberNameSpan (peelImportItemSpan NoSourceSpan item) memberName)
            (IdentifierNamed memberName)
            ResolutionNamespaceTerm
            (ResolvedError "not exported")

missingImportedName :: ImportItem -> ResolutionNamespace -> UnqualifiedName -> Map.Map Text ResolvedName -> Maybe ResolutionAnnotation
missingImportedName item namespace itemName candidates
  | Map.member rendered candidates = Nothing
  | otherwise =
      Just
        ( ResolutionAnnotation
            (spanStartNameSpan (peelImportItemSpan NoSourceSpan item) rendered)
            (IdentifierNamed rendered)
            namespace
            (ResolvedError "not exported")
        )
  where
    rendered = renderUnqualifiedName itemName

type TermDefinition = UnqualifiedName -> Maybe ResolvedName

resolveBindingGroup :: TermDefinition -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBindingGroup _ _ [] = pure []
resolveBindingGroup termDefinition signatureScopes (decl : rest) = do
  (signatureScopes', decl') <- resolveBindingDecl termDefinition signatureScopes decl
  decls' <- resolveBindingGroup termDefinition signatureScopes' rest
  pure (decl' : decls')

resolveBindingDecl :: TermDefinition -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveBindingDecl termDefinition signatureScopes decl = do
  scope <- currentScope
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
  withScope scoped (resolveDeclWithSignatureScope termDefinition signatureScopes decl)

resolveDeclWithSignatureScope :: TermDefinition -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveDeclWithSignatureScope termDefinition signatureScopes decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann $ do
        (signatureScopes', inner') <- resolveDeclWithSignatureScope termDefinition signatureScopes inner
        pure (signatureScopes', DeclAnn ann inner')
    DeclTypeSig names ty -> do
      sp <- currentSpan
      (binderScope, ty') <- resolveTypeSignature ty
      let names' = map (resolveTermDefinitionAt sp termDefinition) names
      let signatureScopes' =
            List.foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
      pure (signatureScopes', DeclTypeSig names' ty')
    _ -> do
      decl' <- resolveDecl termDefinition decl
      let signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
      pure (signatureScopes', decl')

resolveDecl :: TermDefinition -> Decl -> ResolveM Decl
resolveDecl termDefinition (DeclAnn ann inner) =
  withPushedSpan ann (resolveDecl termDefinition inner)
resolveDecl termDefinition decl =
  resolveDeclCore termDefinition decl

resolveDeclCore :: TermDefinition -> Decl -> ResolveM Decl
resolveDeclCore termDefinition decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann (resolveDeclCore termDefinition inner)
    DeclValue valueDecl ->
      DeclValue <$> resolveValueDecl termDefinition valueDecl
    DeclImplicitParam name expr mDecls -> do
      -- An implicit-parameter binding does not bind a term name.
      -- The type checker connects each use to its binding.
      (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
      expr' <- extendScope localScope (resolveExpr expr)
      mDecls' <- traverse (extendScope localScope . resolveBoundDecls binderAnnotations Map.empty) mDecls
      pure (DeclImplicitParam name expr' mDecls')
    DeclTypeSig names ty -> do
      ty' <- resolveType ty
      pure (DeclTypeSig names ty')
    DeclStandaloneKindSig name kind -> do
      scope <- currentScope
      sp <- currentSpan
      let rendered = renderUnqualifiedName name
          name' = resolveUnqualifiedNameTo sp ResolutionNamespaceType (lookupType rendered scope) name
      DeclStandaloneKindSig name' <$> resolveType kind
    DeclTypeData dataDecl ->
      DeclTypeData <$> resolveDataDecl "type data " dataDecl
    DeclData dataDecl ->
      DeclData <$> resolveDataDecl "data " dataDecl
    DeclTypeSyn typeSynDecl ->
      DeclTypeSyn <$> resolveTypeSynDecl typeSynDecl
    DeclSplice expr -> DeclSplice <$> resolveExpr expr
    DeclNewtype newtypeDecl ->
      DeclNewtype <$> resolveNewtypeDecl newtypeDecl
    DeclClass classDecl ->
      DeclClass <$> resolveClassDecl classDecl
    DeclDefault tys ->
      DeclDefault <$> mapM resolveType tys
    DeclFixity {} -> pure decl
    DeclForeign foreignDecl ->
      DeclForeign <$> resolveForeignDecl termDefinition foreignDecl
    DeclRoleAnnotation roleAnnotation -> do
      scope <- currentScope
      sp <- currentSpan
      let name = roleAnnotationName roleAnnotation
          rendered = renderUnqualifiedName name
          name' = resolveUnqualifiedNameTo sp ResolutionNamespaceType (lookupType rendered scope) name
      pure (DeclRoleAnnotation roleAnnotation {roleAnnotationName = name'})
    DeclPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure decl
      | otherwise -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclPatSyn patSyn -> do
      sp <- currentSpan
      (patSyn', unboundArgs) <- resolvePatSynDecl termDefinition patSyn
      pure (List.foldl' (\acc arg -> annotateDecl (unboundPatSynArgAnnotation sp arg) acc) (DeclPatSyn patSyn') unboundArgs)
    DeclPatSynSig names ty -> do
      sp <- currentSpan
      (_, ty') <- resolveTypeSignature ty
      let names' = map (resolveTermDefinitionAt (declKeywordNameSpan "pattern " sp "") termDefinition) names
      pure (DeclPatSynSig names' ty')
    DeclInstance instanceDecl ->
      DeclInstance <$> resolveInstanceDecl instanceDecl
    DeclStandaloneDeriving derivingDecl ->
      DeclStandaloneDeriving <$> resolveStandaloneDerivingDecl derivingDecl
    DeclTypeFamilyDecl familyDecl ->
      DeclTypeFamilyDecl <$> resolveTypeFamilyDecl familyDecl
    DeclDataFamilyDecl dataFamilyDecl ->
      DeclDataFamilyDecl <$> resolveDataFamilyDecl dataFamilyDecl
    DeclTypeFamilyInst familyInst ->
      DeclTypeFamilyInst <$> resolveTypeFamilyInst familyInst
    DeclDataFamilyInst dataFamilyInst ->
      DeclDataFamilyInst <$> resolveDataFamilyInst dataFamilyInst

-- | Pragmas that only give optimisation or documentation hints.
-- The resolver accepts them and does not resolve their contents.
ignoredPragma :: PragmaType -> Bool
ignoredPragma pragma =
  case pragma of
    PragmaInline kind _
      | kind == "INLINE"
          || kind == "INLINABLE"
          || kind == "INLINEABLE"
          || kind == "NOINLINE"
          || kind == "NOINLINEABLE"
          || kind == "NOINLINABLE"
          || kind == "CONLIKE" ->
          True
    PragmaDeprecated _ -> True
    PragmaWarning _ -> True
    PragmaUnknown rawText ->
      -- A phase such as @INLINE[1]@ is part of the first word.
      case T.words (T.toUpper (T.drop 3 rawText)) of
        keyword : _ -> T.takeWhile (/= '[') keyword `elem` ignoredPragmaKeywords
        [] -> False
    _ -> False

-- | Keywords of hint pragmas that do not change name resolution.
ignoredPragmaKeywords :: [Text]
ignoredPragmaKeywords =
  [ "INLINE",
    "INLINABLE",
    "INLINEABLE",
    "NOINLINE",
    "NOINLINABLE",
    "NOINLINEABLE",
    "CONLIKE",
    "RULES",
    "SPECIALISE",
    "SPECIALIZE",
    "SPECIALISE_INLINE",
    "SPECIALIZE_INLINE",
    "MINIMAL",
    "COMPLETE",
    "ANN",
    "OPAQUE",
    "DEPRECATED",
    "WARNING",
    "CFILES"
  ]

resolveValueDecl :: TermDefinition -> ValueDecl -> ResolveM ValueDecl
resolveValueDecl termDefinition valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      sp <- currentSpan
      let name' = resolveTermDefinitionAt sp termDefinition name
      FunctionBind name' <$> mapM resolveMatch matches
    PatternBind multTag pat rhs ->
      PatternBind multTag <$> resolvePatternDefinition termDefinition pat <*> resolveRhs rhs

-- | Resolve a pattern synonym declaration. The right-hand side pattern
-- binds the argument variables. The result gives the arguments that the
-- pattern does not bind.
resolvePatSynDecl :: TermDefinition -> PatSynDecl -> ResolveM (PatSynDecl, [Text])
resolvePatSynDecl termDefinition patSyn = do
  sp <- currentSpan
  let name = patSynDeclName patSyn
      nameSpan = patSynNameSpan sp patSyn
      name' =
        case termDefinition name of
          Just resolved -> resolveUnqualifiedNameTo nameSpan ResolutionNamespaceTerm resolved name
          Nothing -> name
  (patScope, pat') <- bindPattern (patSynDeclPat patSyn)
  dir' <-
    case patSynDeclDir patSyn of
      PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional <$> mapM resolveMatch matches
      dir -> pure dir
  let unboundArgs = [arg | arg <- patSynArgNames (patSynDeclArgs patSyn), not (Map.member arg (scopeTerms patScope))]
  pure (patSyn {patSynDeclName = name', patSynDeclPat = pat', patSynDeclDir = dir'}, unboundArgs)

patSynArgNames :: PatSynArgs -> [Text]
patSynArgNames args =
  case args of
    PatSynPrefixArgs names -> names
    PatSynInfixArgs left right -> [left, right]
    PatSynRecordArgs fields -> fields

-- | The span of the pattern synonym name. An infix name follows its left
-- argument.
patSynNameSpan :: SourceSpan -> PatSynDecl -> SourceSpan
patSynNameSpan sp patSyn =
  case patSynDeclArgs patSyn of
    PatSynInfixArgs left _ -> declKeywordNameSpan ("pattern " <> left <> " ") sp nameText'
    _ -> declKeywordNameSpan "pattern " sp nameText'
  where
    nameText' = unqualifiedNameText (patSynDeclName patSyn)

unboundPatSynArgAnnotation :: SourceSpan -> Text -> ResolutionAnnotation
unboundPatSynArgAnnotation sp arg =
  ResolutionAnnotation sp (IdentifierNamed arg) ResolutionNamespaceTerm (ResolvedError "pattern synonym argument is not bound by the pattern")

resolveForeignDecl :: TermDefinition -> ForeignDecl -> ResolveM ForeignDecl
resolveForeignDecl termDefinition foreignDecl = do
  sp <- currentSpan
  let name' = resolveTermDefinitionAt sp termDefinition (foreignName foreignDecl)
  ty' <- resolveType (foreignType foreignDecl)
  pure foreignDecl {foreignName = name', foreignType = ty'}

resolveClassDecl :: ClassDecl -> ResolveM ClassDecl
resolveClassDecl classDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "class " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case classDeclHead classDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  context' <- traverse (mapM resolveType) (classDeclContext classDecl)
  items' <- mapM resolveClassDeclItem (classDeclItems classDecl)
  pure
    classDecl
      { classDeclHead = head',
        classDeclContext = context',
        classDeclItems = items'
      }

resolveClassDeclItem :: ClassDeclItem -> ResolveM ClassDeclItem
resolveClassDeclItem classDeclItem =
  case classDeclItem of
    ClassItemAnn ann inner -> ClassItemAnn ann <$> withPushedSpan ann (resolveClassDeclItem inner)
    ClassItemTypeSig names ty -> do
      scope <- currentScope
      sp <- currentSpan
      let names' = map (resolveTermDefinitionAt sp (topLevelTermDefinition scope)) names
      ClassItemTypeSig names' <$> resolveType ty
    ClassItemDefaultSig name ty -> ClassItemDefaultSig name <$> resolveType ty
    ClassItemDefault valueDecl -> do
      scope <- currentScope
      ClassItemDefault <$> withResetLocalSupply (resolveValueDecl (topLevelTermDefinition scope) valueDecl)
    ClassItemFixity {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure classDeclItem
      | otherwise -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemTypeFamilyDecl familyDecl -> ClassItemTypeFamilyDecl <$> resolveTypeFamilyDecl familyDecl
    ClassItemDataFamilyDecl {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemDefaultTypeInst familyInst -> ClassItemDefaultTypeInst <$> resolveTypeFamilyInst familyInst

resolveInstanceDecl :: InstanceDecl -> ResolveM InstanceDecl
resolveInstanceDecl instanceDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (instanceDeclForall instanceDecl)
  (context', head', items') <-
    extendScope forallScope $ do
      context' <- mapM resolveType (instanceDeclContext instanceDecl)
      head' <- resolveType (instanceDeclHead instanceDecl)
      items' <- mapM (resolveInstanceDeclItem (instanceHeadClass head')) (instanceDeclItems instanceDecl)
      pure (context', head', items')
  pure
    instanceDecl
      { instanceDeclForall = forallBinders',
        instanceDeclContext = context',
        instanceDeclHead = head',
        instanceDeclItems = items'
      }

-- | The class of a resolved instance head, with the name as written.
instanceHeadClass :: Type -> Maybe (Text, ResolvedName)
instanceHeadClass ty =
  case ty of
    TAnn _ inner -> instanceHeadClass inner
    TParen inner -> instanceHeadClass inner
    TKindSig inner _ -> instanceHeadClass inner
    TApp fun _ -> instanceHeadClass fun
    TCon name Unpromoted ->
      listToMaybe
        [ (nameText name, resolutionTarget resolution)
        | resolution <- mapMaybe fromAnnotation (nameAnns name)
        ]
    _ -> Nothing

-- | A method binding in an instance names a method of the class. The
-- class may be in scope only under a qualifier, so the lookup goes
-- through the scopes that export the class rather than the plain term
-- scope.
instanceMethodDefinition :: Maybe (Text, ResolvedName) -> Scope -> TermDefinition
instanceMethodDefinition headClass scope name =
  case (headClass, lookupTerm rendered scope) of
    (Just (className, resolvedClass), ResolvedError _)
      | found : _ <- classMethods className resolvedClass -> Just found
    (_, resolved) -> Just resolved
  where
    rendered = renderUnqualifiedName name
    classMethods className resolvedClass =
      [ resolved
      | candidate <- scope : Map.elems (scopeQualifiedModules scope),
        lookupType className candidate == resolvedClass,
        rendered `elem` Map.findWithDefault [] className (scopeMethods candidate),
        resolved@ResolvedTopLevel {} <- [lookupTerm rendered candidate]
      ]

-- | The scope that resolves the family name of an associated type instance
-- through the class of the instance head, like an instance method. The
-- family name can be out of scope when only the class is in scope, for
-- example through a qualified import.
associatedTypeInstanceScope :: Maybe (Text, ResolvedName) -> Scope -> Type -> Scope
associatedTypeInstanceScope headClass scope lhs =
  case (headClass, typeHeadConstructorName lhs) of
    (Just (className, resolvedClass), Just familyName)
      | ResolvedError _ <- lookupType familyName scope,
        found : _ <- associatedTypes className resolvedClass familyName ->
          emptyScope {scopeTypes = Map.singleton familyName found}
    _ -> emptyScope
  where
    associatedTypes className resolvedClass familyName =
      [ resolved
      | candidate <- scope : Map.elems (scopeQualifiedModules scope),
        lookupType className candidate == resolvedClass,
        familyName `elem` Map.findWithDefault [] className (scopeAssociatedTypes candidate),
        resolved@ResolvedTopLevel {} <- [lookupType familyName candidate]
      ]

-- | The name of the type constructor at the head of a type application.
typeHeadConstructorName :: Type -> Maybe Text
typeHeadConstructorName ty =
  case ty of
    TAnn _ inner -> typeHeadConstructorName inner
    TParen inner -> typeHeadConstructorName inner
    TKindSig inner _ -> typeHeadConstructorName inner
    TApp fun _ -> typeHeadConstructorName fun
    TCon name Unpromoted -> Just (nameText name)
    TInfix _ name Unpromoted _ -> Just (nameText name)
    _ -> Nothing

resolveInstanceDeclItem :: Maybe (Text, ResolvedName) -> InstanceDeclItem -> ResolveM InstanceDeclItem
resolveInstanceDeclItem headClass instanceDeclItem =
  case instanceDeclItem of
    InstanceItemAnn ann inner -> InstanceItemAnn ann <$> withPushedSpan ann (resolveInstanceDeclItem headClass inner)
    InstanceItemBind valueDecl -> do
      scope <- currentScope
      InstanceItemBind <$> withResetLocalSupply (resolveValueDecl (instanceMethodDefinition headClass scope) valueDecl)
    InstanceItemTypeSig names ty -> InstanceItemTypeSig names <$> resolveType ty
    InstanceItemFixity {} -> pure instanceDeclItem
    InstanceItemTypeFamilyInst familyInst -> do
      scope <- currentScope
      let familyScope = associatedTypeInstanceScope headClass scope (typeFamilyInstLhs familyInst)
      InstanceItemTypeFamilyInst <$> extendScope familyScope (resolveTypeFamilyInst familyInst)
    InstanceItemDataFamilyInst {} -> annotateUnhandledInstanceDeclItem <$> currentSpan <*> pure instanceDeclItem
    InstanceItemPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure instanceDeclItem
      | otherwise -> annotateUnhandledInstanceDeclItem <$> currentSpan <*> pure instanceDeclItem

resolveStandaloneDerivingDecl :: StandaloneDerivingDecl -> ResolveM StandaloneDerivingDecl
resolveStandaloneDerivingDecl derivingDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (standaloneDerivingForall derivingDecl)
  (strategy', context', head') <-
    extendScope forallScope $
      (,,)
        <$> traverse resolveDerivingStrategy (standaloneDerivingStrategy derivingDecl)
        <*> mapM resolveType (standaloneDerivingContext derivingDecl)
        <*> resolveType (standaloneDerivingHead derivingDecl)
  pure
    derivingDecl
      { standaloneDerivingStrategy = strategy',
        standaloneDerivingForall = forallBinders',
        standaloneDerivingContext = context',
        standaloneDerivingHead = head'
      }

resolveDerivingClause :: DerivingClause -> ResolveM DerivingClause
resolveDerivingClause clause = do
  strategy' <- traverse resolveDerivingStrategy (derivingStrategy clause)
  classes' <-
    case derivingClasses clause of
      Left name -> Left <$> resolveTypeUseAtName name
      Right tys -> Right <$> mapM resolveType tys
  pure clause {derivingStrategy = strategy', derivingClasses = classes'}

resolveDerivingStrategy :: DerivingStrategy -> ResolveM DerivingStrategy
resolveDerivingStrategy strategy =
  case strategy of
    DerivingVia ty -> DerivingVia <$> resolveType ty
    _ -> pure strategy

resolveMatch :: Match -> ResolveM Match
resolveMatch match =
  withEffectiveSpan (sourceSpanFromAnns (matchAnns match)) $ do
    (patScope, pats') <- bindPatterns (matchPats match)
    rhsHere <- effectiveResolutionSpan <$> currentSpan <*> pure (rhsSpan (matchRhs match))
    rhs' <- extendScope patScope (withAmbientSpan rhsHere (resolveRhs (matchRhs match)))
    pure match {matchPats = pats', matchRhs = rhs'}

resolveRhs :: Rhs Expr -> ResolveM (Rhs Expr)
resolveRhs rhs =
  case rhs of
    UnguardedRhs anns expr mDecls ->
      withEffectiveSpan (sourceSpanFromAnns anns) $ do
        -- Pre-allocate where-clause binders so the body can reference them.
        (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
        expr' <- extendScope localScope (resolveExpr expr)
        mDecls' <- case mDecls of
          Nothing -> pure Nothing
          Just decls -> Just <$> extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (UnguardedRhs anns expr' mDecls')
    GuardedRhss anns guardedRhss mDecls ->
      withEffectiveSpan (sourceSpanFromAnns anns) $ do
        -- Pre-allocate where-clause binders so guards can reference them.
        (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
        guardedRhss' <- extendScope localScope (mapM resolveGuardedRhs guardedRhss)
        mDecls' <- case mDecls of
          Nothing -> pure Nothing
          Just decls -> Just <$> extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (GuardedRhss anns guardedRhss' mDecls')

resolveGuardedRhs :: GuardedRhs Expr -> ResolveM (GuardedRhs Expr)
resolveGuardedRhs guardedRhs =
  withEffectiveSpan (sourceSpanFromAnns (guardedRhsAnns guardedRhs)) $ do
    (scope', guards') <- resolveGuardQualifiers (guardedRhsGuards guardedRhs)
    body' <- withScope scope' (resolveExpr (guardedRhsBody guardedRhs))
    pure guardedRhs {guardedRhsGuards = guards', guardedRhsBody = body'}

resolveGuardQualifiers :: [GuardQualifier] -> ResolveM (Scope, [GuardQualifier])
resolveGuardQualifiers qualifiers = do
  scope <- currentScope
  go scope qualifiers
  where
    go scope qualifiers' =
      withScope scope $
        case qualifiers' of
          [] -> pure (scope, [])
          qualifier : rest -> do
            (scope', qualifier') <- resolveGuardQualifier qualifier
            (scope'', rest') <- go scope' rest
            pure (scope'', qualifier' : rest')

resolveGuardQualifier :: GuardQualifier -> ResolveM (Scope, GuardQualifier)
resolveGuardQualifier qualifier =
  withEffectiveSpan (peelGuardQualifierSpan NoSourceSpan qualifier) $ do
    scope <- currentScope
    let qualifierSpan = peelGuardQualifierSpan NoSourceSpan qualifier
        wrap = GuardAnn (mkAnnotation qualifierSpan)
    case peelGuardQualifierAnn qualifier of
      GuardExpr expr -> do
        expr' <- resolveExpr expr
        pure (scope, wrap (GuardExpr expr'))
      GuardPat pat expr -> do
        expr' <- resolveExpr expr
        (patScope, pat') <- bindPattern pat
        pure (unionScope patScope scope, wrap (GuardPat pat' expr'))
      GuardLet decls -> do
        (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
        decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (unionScope localScope scope, wrap (GuardLet decls'))
      GuardAnn _ _ -> pure (scope, qualifier)

resolveExpr :: Expr -> ResolveM Expr
resolveExpr expr =
  case expr of
    EAnn ann inner ->
      EAnn ann <$> withPushedSpan ann (resolveExpr inner)
    EVar name ->
      EVar <$> resolveTermUse name
    -- An implicit parameter has no lexical binder. The type checker
    -- connects the use to a binding through constraint solving.
    EImplicitParam _ -> pure expr
    ETypeSyntax form ty -> ETypeSyntax form <$> resolveType ty
    EInt _ TInteger _ -> resolveIntegerLiteral expr
    EInt _ numericType _ -> resolvePrimitiveLiteralType numericType expr
    EFloat _ floatType _ ->
      maybe (resolveFractionalLiteral expr) (`resolvePrimitiveLiteralTypeName` expr) (primitiveFloatTypeName floatType)
    EChar {} -> pure expr
    ECharHash {} -> resolvePrimitiveLiteralTypeName "Char#" expr
    EString {} -> resolveStringLiteral expr
    EStringHash {} -> resolvePrimitiveLiteralTypeName "Addr#" expr
    EOverloadedLabel {} -> pure expr
    EIf cond trueBranch falseBranch -> do
      resolved <- EIf <$> resolveExpr cond <*> resolveExpr trueBranch <*> resolveExpr falseBranch
      annotateRebindableIf resolved
    EMultiWayIf guardedRhss ->
      EMultiWayIf <$> mapM resolveGuardedRhs guardedRhss
    ELambdaPats pats body -> do
      (patScope, pats') <- bindPatterns pats
      body' <- extendScope patScope (resolveExpr body)
      pure (ELambdaPats pats' body')
    ELambdaCase alts ->
      ELambdaCase <$> mapM resolveCaseAlt alts
    ELambdaCases alts ->
      ELambdaCases <$> mapM resolveLambdaCaseAlt alts
    EInfix {} ->
      resolveInfixExpr expr
    -- The parser only builds this for the view-pattern arrow, so it reaches
    -- the resolver only when a view pattern appears where a pattern cannot.
    EViewPat lhs rhs ->
      EViewPat <$> resolveExpr lhs <*> resolveExpr rhs
    ENegate inner ->
      annotateNegate . ENegate =<< resolveExpr inner
    ESectionL inner op ->
      ESectionL <$> resolveExpr inner <*> resolveTermUseAtName op
    ESectionR op inner ->
      ESectionR <$> resolveTermUseAtName op <*> resolveExpr inner
    ELetDecls decls body -> do
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      body' <- extendScope localScope (resolveExpr body)
      pure (ELetDecls decls' body')
    ECase scrutinee alts ->
      ECase <$> resolveExpr scrutinee <*> mapM resolveCaseAlt alts
    EArithSeq arithSeq ->
      EArithSeq <$> resolveArithSeq arithSeq
    ERecordCon name fields wildcard ->
      ERecordCon <$> resolveTermUse name <*> resolveRecordFields fields <*> pure wildcard
    ERecordUpd record fields ->
      ERecordUpd <$> resolveExpr record <*> resolveRecordFields fields
    EGetField record name ->
      EGetField <$> resolveExpr record <*> pure name
    EGetFieldProjection {} -> pure expr
    ETypeSig inner ty ->
      ETypeSig <$> resolveExpr inner <*> resolveType ty
    EParen inner ->
      EParen <$> resolveExpr inner
    EList items -> do
      items' <- mapM resolveExpr items
      sp <- currentSpan
      let annotation = ResolutionAnnotation sp IdentifierList ResolutionNamespaceTerm ResolvedSyntax
      pure (EAnn (mkAnnotation annotation) (EList items'))
    ETuple flavor items -> do
      items' <- mapM resolveMaybeExpr items
      sp <- currentSpan
      let identifier = IdentifierTuple flavor (length items)
          annotation = ResolutionAnnotation sp identifier ResolutionNamespaceTerm ResolvedSyntax
      pure (EAnn (mkAnnotation annotation) (ETuple flavor items'))
    EUnboxedSum alt arity inner ->
      EUnboxedSum alt arity <$> resolveExpr inner
    ETypeApp fun ty ->
      ETypeApp <$> resolveExpr fun <*> resolveType ty
    EApp fun arg ->
      EApp <$> resolveExpr fun <*> resolveExpr arg
    ETHSplice inner ->
      ETHSplice <$> resolveExpr inner
    ETHTypedSplice inner ->
      ETHTypedSplice <$> resolveExpr inner
    EPragma pragma inner ->
      EPragma pragma <$> resolveExpr inner
    EDo stmts flavor -> do
      (_, stmts') <- resolveDoStmts stmts
      pure (EDo stmts' flavor)
    EQuasiQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    EListComp body stmts -> do
      (scope, stmts') <- resolveCompStmts stmts
      body' <- withScope scope (resolveExpr body)
      pure (EListComp body' stmts')
    EListCompParallel {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    -- Template Haskell quotes compile to a runtime error. The quoted
    -- syntax stays unresolved because nothing consumes it.
    ETHExpQuote {} -> pure expr
    ETHTypedQuote {} -> pure expr
    ETHDeclQuote {} -> pure expr
    ETHTypeQuote {} -> pure expr
    ETHPatQuote {} -> pure expr
    ETHNameQuote {} -> pure expr
    ETHTypeNameQuote {} -> pure expr
    EProc {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr

-- | An overloaded integer literal applies fromInteger to an Integer.
--
-- The fromInteger annotation sits inside the Integer type annotation.
resolveIntegerLiteral :: Expr -> ResolveM Expr
resolveIntegerLiteral expr = do
  sp <- currentSpan
  maybeIntegerAnn <- integerTypeAnnotation sp
  annotated <- annotateSyntaxTerm "fromInteger" expr
  pure (maybe annotated (\integerAnn -> EAnn (mkAnnotation integerAnn) annotated) maybeIntegerAnn)

-- | An overloaded fractional literal applies fromRational to a Rational.
--
-- The type of the Rational comes from the type of the method, so the
-- literal gets only the fromRational term.
resolveFractionalLiteral :: Expr -> ResolveM Expr
resolveFractionalLiteral = annotateSyntaxTerm "fromRational"

-- | OverloadedStrings applies fromString to a String literal.
--
-- The argument type comes from the type of the method, so the literal gets
-- only the fromString term. Without the extension a string literal keeps
-- its [Char] type and gets no annotation.
resolveStringLiteral :: Expr -> ResolveM Expr
resolveStringLiteral expr = do
  info <- currentModuleInfo
  if OverloadedStrings `elem` moduleInfoExtensions info
    then annotateSyntaxTerm "fromString" expr
    else pure expr

-- | The resolution of the Integer type in the built-in scope.
--
-- The result is 'Nothing' when the built-in scope does not have Integer.
-- A module without Integer then gives the literal only the fromInteger term.
integerTypeAnnotation :: SourceSpan -> ResolveM (Maybe ResolutionAnnotation)
integerTypeAnnotation sp = do
  annotation <- primitiveLiteralTypeAnnotation sp "Integer"
  pure $ case resolutionTarget annotation of
    ResolvedError _ -> Nothing
    _ -> Just annotation

-- | Annotate an expression with the syntax term that its desugaring applies.
--
-- The term comes from the built-in scope.
-- RebindableSyntax takes the term from the lexical scope instead.
annotateSyntaxTerm :: Text -> Expr -> ResolveM Expr
annotateSyntaxTerm name expr = do
  sp <- currentSpan
  annotation <- syntaxTermAnnotation sp name
  pure (EAnn (mkAnnotation annotation) expr)

-- | Negation applies the negate syntax term to its operand.
annotateNegate :: Expr -> ResolveM Expr
annotateNegate = annotateSyntaxTerm "negate"

resolvePrimitiveLiteralType :: NumericType -> Expr -> ResolveM Expr
resolvePrimitiveLiteralType numericType expr =
  case primitiveNumericTypeName numericType of
    Just name -> resolvePrimitiveLiteralTypeName name expr
    Nothing -> resolveIntegerLiteral expr

resolvePrimitiveLiteralTypeName :: Text -> Expr -> ResolveM Expr
resolvePrimitiveLiteralTypeName name expr = do
  sp <- currentSpan
  annotation <- primitiveLiteralTypeAnnotation sp name
  pure (EAnn (mkAnnotation annotation) expr)

-- | The resolution annotation that names the type of a primitive literal.
primitiveLiteralTypeAnnotation :: SourceSpan -> Text -> ResolveM ResolutionAnnotation
primitiveLiteralTypeAnnotation sp name = do
  info <- currentModuleInfo
  let resolved = lookupType name (moduleInfoBuiltinScope info)
  pure (ResolutionAnnotation sp (IdentifierNamed name) ResolutionNamespaceType resolved)

-- | The type name of a primitive literal, or 'Nothing' for a boxed literal.
primitiveLiteralTypeName :: Literal -> Maybe Text
primitiveLiteralTypeName literal =
  case literal of
    LitAnn _ inner -> primitiveLiteralTypeName inner
    LitInt _ numericType _ -> primitiveNumericTypeName numericType
    LitFloat _ floatType _ -> primitiveFloatTypeName floatType
    LitChar {} -> Nothing
    LitCharHash {} -> Just "Char#"
    LitString {} -> Nothing
    LitStringHash {} -> Just "Addr#"

primitiveFloatTypeName :: FloatType -> Maybe Text
primitiveFloatTypeName floatType =
  case floatType of
    TFractional -> Nothing
    TFloatHash -> Just "Float#"
    TDoubleHash -> Just "Double#"

primitiveNumericTypeName :: NumericType -> Maybe Text
primitiveNumericTypeName numericType =
  case numericType of
    TInteger -> Nothing
    TIntHash -> Just "Int#"
    TWordHash -> Just "Word#"
    TInt8Hash -> Just "Int8#"
    TInt16Hash -> Just "Int16#"
    TInt32Hash -> Just "Int32#"
    TInt64Hash -> Just "Int64#"
    TWord8Hash -> Just "Word8#"
    TWord16Hash -> Just "Word16#"
    TWord32Hash -> Just "Word32#"
    TWord64Hash -> Just "Word64#"

-- | RebindableSyntax gives an if expression the in-scope ifThenElse.
-- An ordinary if expression uses the built-in Bool and gets no annotation.
annotateRebindableIf :: Expr -> ResolveM Expr
annotateRebindableIf expr = do
  info <- currentModuleInfo
  if RebindableSyntax `elem` moduleInfoExtensions info
    then annotateSyntaxTerm "ifThenElse" expr
    else pure expr

-- | Annotate a literal pattern with the names that the type checker needs.
--
-- An overloaded integer pattern gets the syntax terms that compare it.
-- A string pattern gets them only under OverloadedStrings.
-- A primitive literal pattern gets the resolution of its primitive type.
annotatePatternLiteral :: Pattern -> Literal -> ResolveM Pattern
annotatePatternLiteral pat lit = do
  sp <- literalSpan <$> currentSpan <*> pure lit
  case primitiveLiteralTypeName lit of
    Just typeName -> do
      typeAnn <- primitiveLiteralTypeAnnotation sp typeName
      pure (PAnn (mkAnnotation typeAnn) pat)
    Nothing ->
      case peelLiteralAnn lit of
        LitInt _ TInteger _ -> do
          maybeIntegerAnn <- integerTypeAnnotation sp
          methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromInteger")
          pure (foldr (PAnn . mkAnnotation) pat (maybe methodAnns (: methodAnns) maybeIntegerAnn))
        LitFloat _ TFractional _ -> do
          methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromRational")
          pure (foldr (PAnn . mkAnnotation) pat methodAnns)
        LitString _ _ -> do
          info <- currentModuleInfo
          if OverloadedStrings `elem` moduleInfoExtensions info
            then do
              methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromString")
              pure (foldr (PAnn . mkAnnotation) pat methodAnns)
            else pure pat
        _ -> pure pat
  where
    -- A negated literal pattern also negates the converted literal.
    overloadedPatternMethods conversion =
      case peelPatternAnn pat of
        PNegLit {} -> [conversion, "negate", "=="]
        _ -> [conversion, "=="]

literalSpan :: SourceSpan -> Literal -> SourceSpan
literalSpan ambient (LitAnn ann inner) = literalSpan (pushSpanFromAnn ambient ann) inner
literalSpan ambient _ = ambient

syntaxTermAnnotation :: SourceSpan -> Text -> ResolveM ResolutionAnnotation
syntaxTermAnnotation sp name = do
  resolved <- resolveSyntaxTerm name
  pure (ResolutionAnnotation sp (IdentifierNamed name) ResolutionNamespaceTerm resolved)

resolveSyntaxTerm :: Text -> ResolveM ResolvedName
resolveSyntaxTerm name = do
  scope <- currentScope
  info <- currentModuleInfo
  pure $
    if RebindableSyntax `elem` moduleInfoExtensions info
      then rebindableSyntaxTerm info scope name
      else builtinSyntaxTerm info name

builtinSyntaxTerm :: ModuleInfo -> Text -> ResolvedName
builtinSyntaxTerm info name =
  if name `elem` builtinSyntaxTermNames
    then lookupTerm name (moduleInfoBuiltinScope info)
    else ResolvedError "unknown built-in syntax term"
  where
    builtinSyntaxTermNames =
      [ "fromInteger",
        "fromRational",
        "fromString",
        "negate",
        "==",
        ">>=",
        ">>",
        "enumFrom",
        "enumFromThen",
        "enumFromTo",
        "enumFromThenTo"
      ]

rebindableSyntaxTerm :: ModuleInfo -> Scope -> Text -> ResolvedName
rebindableSyntaxTerm info scope name =
  case lookupTerm name scope of
    ResolvedTopLevel _ resolved
      | nameQualifier resolved == Just "Prelude",
        not (moduleInfoExplicitPreludeImport info) ->
          ResolvedError "unbound"
    resolved -> resolved

resolveMaybeExpr :: Maybe Expr -> ResolveM (Maybe Expr)
resolveMaybeExpr = traverse resolveExpr

resolveCompStmts :: [CompStmt] -> ResolveM (Scope, [CompStmt])
resolveCompStmts stmts = do
  scope <- currentScope
  go scope stmts
  where
    go scope stmts' =
      withScope scope $
        case stmts' of
          [] -> pure (scope, [])
          stmt : rest -> do
            (scope', stmt') <- resolveCompStmt scope stmt
            (scope'', rest') <- go scope' rest
            pure (scope'', stmt' : rest')

resolveCompStmt :: Scope -> CompStmt -> ResolveM (Scope, CompStmt)
resolveCompStmt scope stmt =
  case stmt of
    CompAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveCompStmt scope inner)
      pure (scope', CompAnn ann inner')
    CompGen pat src -> do
      src' <- resolveExpr src
      (patScope, pat') <- bindPattern pat
      pure (unionScope patScope scope, CompGen pat' src')
    CompGuard guard -> do
      guard' <- resolveExpr guard
      pure (scope, CompGuard guard')
    CompLetDecls decls -> do
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (unionScope localScope scope, CompLetDecls decls')
    CompThen expr -> do
      expr' <- resolveExpr expr
      pure (scope, CompThen expr')
    CompThenBy f byExpr -> do
      f' <- resolveExpr f
      byExpr' <- resolveExpr byExpr
      pure (scope, CompThenBy f' byExpr')
    CompGroupUsing expr -> do
      expr' <- resolveExpr expr
      pure (scope, CompGroupUsing expr')
    CompGroupByUsing byExpr usingExpr -> do
      byExpr' <- resolveExpr byExpr
      usingExpr' <- resolveExpr usingExpr
      pure (scope, CompGroupByUsing byExpr' usingExpr')

resolveCaseAlt :: CaseAlt Expr -> ResolveM (CaseAlt Expr)
resolveCaseAlt alt =
  withEffectiveSpan (sourceSpanFromAnns (caseAltAnns alt)) $ do
    (patScope, pat') <- bindPattern (caseAltPattern alt)
    rhs' <- extendScope patScope (resolveRhs (caseAltRhs alt))
    pure alt {caseAltPattern = pat', caseAltRhs = rhs'}

resolveLambdaCaseAlt :: LambdaCaseAlt -> ResolveM LambdaCaseAlt
resolveLambdaCaseAlt alt =
  withEffectiveSpan (sourceSpanFromAnns (lambdaCaseAltAnns alt)) $ do
    (patScope, pats') <- bindPatterns (lambdaCaseAltPats alt)
    rhs' <- extendScope patScope (resolveRhs (lambdaCaseAltRhs alt))
    pure alt {lambdaCaseAltPats = pats', lambdaCaseAltRhs = rhs'}

resolveRecordFields :: [RecordField Expr] -> ResolveM [RecordField Expr]
resolveRecordFields =
  mapM
    ( \field -> do
        value' <- resolveExpr (recordFieldValue field)
        pure field {recordFieldValue = value'}
    )

resolveDoStmts :: [DoStmt Expr] -> ResolveM (Scope, [DoStmt Expr])
resolveDoStmts stmts = do
  scope <- currentScope
  go scope stmts
  where
    go scope stmts' =
      withScope scope $
        case stmts' of
          [] -> pure (scope, [])
          stmt : rest -> do
            (scope', stmt') <- resolveDoStmt (null rest) stmt
            (scope'', rest') <- go scope' rest
            pure (scope'', stmt' : rest')

resolveDoStmt :: Bool -> DoStmt Expr -> ResolveM (Scope, DoStmt Expr)
resolveDoStmt isLast stmt =
  case stmt of
    DoAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveDoStmt isLast inner)
      pure (scope', DoAnn ann inner')
    DoExpr body -> do
      scope <- currentScope
      body' <- resolveExpr body
      stmt' <- annotateDoMethod isLast ">>" (DoExpr body')
      pure (scope, stmt')
    DoBind pat body -> do
      scope <- currentScope
      body' <- resolveExpr body
      (patScope, pat') <- bindPattern pat
      stmt' <- annotateDoMethod isLast ">>=" (DoBind pat' body')
      pure (unionScope patScope scope, stmt')
    DoLetDecls decls -> do
      scope <- currentScope
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (unionScope localScope scope, DoLetDecls decls')
    DoRecStmt stmts -> do
      scope <- currentScope
      (_, stmts') <- resolveDoStmts stmts
      pure (scope, DoRecStmt stmts')

-- | Annotate a do statement with the method that sequences it.
--
-- A bind statement uses @>>=@ and an expression statement uses @>>@.
-- The last statement is the result of the block and gets no method.
-- Ordinary do notation uses the built-in Monad methods.
-- RebindableSyntax uses lexical lookup instead.
annotateDoMethod :: Bool -> Text -> DoStmt Expr -> ResolveM (DoStmt Expr)
annotateDoMethod isLast name stmt
  | isLast = pure stmt
  | otherwise = do
      sp <- currentSpan
      methodAnn <- syntaxTermAnnotation sp name
      pure (DoAnn (mkAnnotation methodAnn) stmt)

resolveArithSeq :: ArithSeq -> ResolveM ArithSeq
resolveArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner ->
      ArithSeqAnn ann <$> withPushedSpan ann (resolveArithSeq inner)
    ArithSeqFrom from -> do
      resolved <- ArithSeqFrom <$> resolveExpr from
      annotateArithSeqMethod "enumFrom" resolved
    ArithSeqFromThen from then' -> do
      resolved <- ArithSeqFromThen <$> resolveExpr from <*> resolveExpr then'
      annotateArithSeqMethod "enumFromThen" resolved
    ArithSeqFromTo from to -> do
      resolved <- ArithSeqFromTo <$> resolveExpr from <*> resolveExpr to
      annotateArithSeqMethod "enumFromTo" resolved
    ArithSeqFromThenTo from then' to -> do
      resolved <- ArithSeqFromThenTo <$> resolveExpr from <*> resolveExpr then' <*> resolveExpr to
      annotateArithSeqMethod "enumFromThenTo" resolved

annotateArithSeqMethod :: Text -> ArithSeq -> ResolveM ArithSeq
annotateArithSeqMethod name arithSeq = do
  sp <- currentSpan
  annotation <- syntaxTermAnnotation sp name
  pure (ArithSeqAnn (mkAnnotation annotation) arithSeq)

resolveBoundDecls :: Map.Map Text ResolvedName -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBoundDecls binderTargets signatureScopes decls = do
  decls' <- markMixedImplicitParamGroup decls
  resolveBindingGroup (\name -> Map.lookup (renderUnqualifiedName name) binderTargets) signatureScopes decls'

-- | Mark each implicit-parameter binding in a group that also has other declarations.
--
-- GHC does not permit a @let@ or @where@ group with both kinds of binding.
markMixedImplicitParamGroup :: [Decl] -> ResolveM [Decl]
markMixedImplicitParamGroup decls
  | any isImplicitParamDecl decls && not (all isImplicitParamDecl decls) = mapM mark decls
  | otherwise = pure decls
  where
    isImplicitParamDecl decl =
      case snd (peelDeclSpan NoSourceSpan decl) of
        DeclImplicitParam {} -> True
        _ -> False
    mark decl =
      case peelDeclSpan NoSourceSpan decl of
        (declSpan, DeclImplicitParam name _ _) -> do
          ambient <- currentSpan
          let loc = spanStartNameSpan (effectiveResolutionSpan ambient declSpan) name
              resolution =
                ResolutionAnnotation
                  loc
                  (IdentifierNamed name)
                  ResolutionNamespaceTerm
                  (ResolvedError "implicit-parameter binding in a group with other bindings")
          pure (DeclAnn (mkAnnotation resolution) decl)
        _ -> pure decl

declSignatureScope :: Decl -> Map.Map Text Scope -> Maybe Scope
declSignatureScope decl signatureScopes =
  case declBinderCandidate decl of
    Just (_, name) -> Map.lookup (renderUnqualifiedName name) signatureScopes
    Nothing -> Nothing

bindPatterns :: [Pattern] -> ResolveM (Scope, [Pattern])
bindPatterns pats = do
  (scopes, pats') <- mapAndUnzipM bindPattern pats
  pure (foldr unionScope emptyScope scopes, pats')

bindPattern :: Pattern -> ResolveM (Scope, Pattern)
bindPattern pat =
  case pat of
    PAnn ann inner ->
      withPushedSpan ann $ do
        (scope, inner') <- bindPattern inner
        pure (scope, PAnn ann inner')
    PVar name -> do
      sp <- currentSpan
      resolvedName <- freshLocal name
      let key = renderUnqualifiedName name
          name' = resolveUnqualifiedNameTo sp ResolutionNamespaceTerm resolvedName name
      pure (termScope key resolvedName, PVar name')
    PTypeBinder binder -> do
      let binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
      resolvedName <- freshLocal binderName
      binder' <- traverseTyVarBinderKind binder
      let binderScope = Scope Map.empty (Map.singleton (tyVarBinderName binder) resolvedName) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
      pure (binderScope, PTypeBinder binder')
    PTypeSyntax form ty -> do
      ty' <- resolveType ty
      pure (emptyScope, PTypeSyntax form ty')
    PWildcard -> pure (emptyScope, pat)
    PLit lit -> do
      pat' <- annotatePatternLiteral (PLit lit) lit
      pure (emptyScope, pat')
    PTuple flavor pats -> do
      (scope, pats') <- bindPatterns pats
      pure (scope, PTuple flavor pats')
    PUnboxedSum alt arity inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PUnboxedSum alt arity inner')
    PList pats -> do
      (scope, pats') <- bindPatterns pats
      pure (scope, PList pats')
    PCon name typeArgs pats -> do
      name' <- resolveTermUseAtName name
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, PCon name' typeArgs' pats')
    -- A built-in constructor has no name for a scope to bind, so only the
    -- type arguments and the sub-patterns need resolving.
    PBuiltinCon builtin typeArgs pats -> do
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, PBuiltinCon builtin typeArgs' pats')
    PInfix {} -> do
      let (operands, names) = flattenInfixPattern pat
      bound <- mapM bindPattern operands
      names' <- mapM resolveTermUseAtName names
      let scope = foldr (\(operandScope, _) acc -> unionScope acc operandScope) emptyScope bound
      pat' <- reassociateResolvedInfixPattern (map snd bound) names'
      pure (scope, pat')
    PView expr inner -> do
      expr' <- resolveExpr expr
      (scope, inner') <- bindPattern inner
      pure (scope, PView expr' inner')
    PAs alias inner -> do
      here <- currentSpan
      let aliasKey = renderUnqualifiedName alias
      aliasResolved <- freshLocal alias
      let alias' = resolveUnqualifiedNameTo (spanStartNameSpan here aliasKey) ResolutionNamespaceTerm aliasResolved alias
          aliasScope = termScope aliasKey aliasResolved
      (innerScope, inner') <- bindPattern inner
      pure (unionScope innerScope aliasScope, PAs alias' inner')
    PStrict inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PStrict inner')
    PIrrefutable inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PIrrefutable inner')
    PParen inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PParen inner')
    PRecord name fields wildcard -> do
      name' <- resolveTermUseAtName name
      (fieldScopes, fields') <-
        mapAndUnzipM
          ( \field -> do
              (fieldScope, fieldPat') <- bindPattern (recordFieldValue field)
              pure (fieldScope, field {recordFieldValue = fieldPat'})
          )
          fields
      wildcardEntries <- bindRecordWildcardFields name fields wildcard
      ambient <- currentSpan
      let sp = effectiveResolutionSpan ambient (sourceSpanFromAnns (nameAnns name'))
      -- A record wildcard binds each remaining field to a variable with the
      -- field name. The pattern lists these fields as puns, so a later phase
      -- sees an ordinary record pattern.
      let wildcardScope = Scope (Map.fromList wildcardEntries) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
          wildcardFields =
            [ RecordField
                { recordFieldName = Name Nothing NameVarId fieldName [],
                  recordFieldValue = PVar (resolveUnqualifiedNameTo sp ResolutionNamespaceTerm resolvedName ((mkUnqualifiedName NameVarId fieldName) {unqualifiedNameAnns = [mkAnnotation sp]})),
                  recordFieldPun = True
                }
            | (fieldName, resolvedName) <- wildcardEntries
            ]
      pure (foldr unionScope wildcardScope fieldScopes, PRecord name' (fields' <> wildcardFields) False)
    PTypeSig inner ty -> do
      (scope, inner') <- bindPattern inner
      ty' <- resolveType ty
      pure (scope, PTypeSig inner' ty')
    PNegLit lit -> do
      pat' <- annotatePatternLiteral (PNegLit lit) lit
      pure (emptyScope, pat')
    PSplice expr -> do
      expr' <- resolveExpr expr
      pure (emptyScope, PSplice expr')
    PQuasiQuote {} -> do
      sp <- currentSpan
      pure (emptyScope, annotateUnhandledPattern sp pat)
  where
    traverseTyVarBinderKind binder = do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure binder {tyVarBinderKind = kind'}

termScope :: Text -> ResolvedName -> Scope
termScope key resolvedName =
  Scope (Map.singleton key resolvedName) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

resolvePatternDefinition :: TermDefinition -> Pattern -> ResolveM Pattern
resolvePatternDefinition termDefinition pat =
  case pat of
    PAnn ann inner ->
      PAnn ann <$> withPushedSpan ann (resolvePatternDefinition termDefinition inner)
    PVar name -> do
      sp <- currentSpan
      pure (PVar (resolveTermDefinitionAt sp termDefinition name))
    PTypeBinder binder -> do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure (PTypeBinder (binder {tyVarBinderKind = kind'}))
    PTypeSyntax form ty ->
      PTypeSyntax form <$> resolveType ty
    PWildcard -> pure pat
    PLit lit -> annotatePatternLiteral (PLit lit) lit
    PQuasiQuote {} -> annotateUnhandledPattern <$> currentSpan <*> pure pat
    PTuple flavor pats ->
      PTuple flavor <$> mapM (resolvePatternDefinition termDefinition) pats
    PUnboxedSum alt arity inner ->
      PUnboxedSum alt arity <$> resolvePatternDefinition termDefinition inner
    PList pats ->
      PList <$> mapM (resolvePatternDefinition termDefinition) pats
    PCon name typeArgs pats ->
      PCon <$> resolveTermUseAtName name <*> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PBuiltinCon builtin typeArgs pats ->
      PBuiltinCon builtin <$> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PInfix {} -> do
      let (operands, names) = flattenInfixPattern pat
      operands' <- mapM (resolvePatternDefinition termDefinition) operands
      names' <- mapM resolveTermUseAtName names
      reassociateResolvedInfixPattern operands' names'
    PView expr inner ->
      PView <$> withResetLocalSupply (resolveExpr expr) <*> resolvePatternDefinition termDefinition inner
    PAs alias inner -> do
      sp <- currentSpan
      PAs (resolveTermDefinitionAt sp termDefinition alias) <$> resolvePatternDefinition termDefinition inner
    PStrict inner ->
      PStrict <$> resolvePatternDefinition termDefinition inner
    PIrrefutable inner ->
      PIrrefutable <$> resolvePatternDefinition termDefinition inner
    PNegLit lit -> annotatePatternLiteral (PNegLit lit) lit
    PParen inner ->
      PParen <$> resolvePatternDefinition termDefinition inner
    PRecord name fields wildcard ->
      PRecord
        <$> resolveTermUseAtName name
        <*> mapM
          ( \field -> do
              value' <- resolvePatternDefinition termDefinition (recordFieldValue field)
              pure field {recordFieldValue = value'}
          )
          fields
        <*> pure wildcard
    PTypeSig inner ty ->
      PTypeSig <$> resolvePatternDefinition termDefinition inner <*> resolveType ty
    PSplice expr ->
      PSplice <$> withResetLocalSupply (resolveExpr expr)

bindRecordWildcardFields :: Name -> [RecordField Pattern] -> Bool -> ResolveM [(Text, ResolvedName)]
bindRecordWildcardFields conName fields wildcard
  | not wildcard = pure []
  | otherwise =
      mapM bindField =<< wildcardFields
  where
    wildcardFields = do
      scope <- currentScope
      let explicitFields = map (nameText . recordFieldName) fields
      pure
        ( filter (`notElem` explicitFields) $
            Map.findWithDefault [] (nameText conName) (scopeRecordFields scope)
        )
    bindField fieldName = do
      let binder = mkUnqualifiedName NameVarId fieldName
      resolvedName <- freshLocal binder
      pure (fieldName, resolvedName)

resolveDataDecl :: Text -> DataDecl -> ResolveM DataDecl
resolveDataDecl keyword dataDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan keyword declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case dataDeclHead dataDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  context' <- mapM resolveType (dataDeclContext dataDecl)
  kind' <- traverse resolveType (dataDeclKind dataDecl)
  constructors' <- mapM resolveDataConDecl (dataDeclConstructors dataDecl)
  deriving' <- mapM resolveDerivingClause (dataDeclDeriving dataDecl)
  pure
    dataDecl
      { dataDeclHead = head',
        dataDeclContext = context',
        dataDeclKind = kind',
        dataDeclConstructors = map (resolveDataConDefinitions scope) constructors',
        dataDeclDeriving = deriving'
      }

resolveTypeFamilyDecl :: TypeFamilyDecl -> ResolveM TypeFamilyDecl
resolveTypeFamilyDecl familyDecl = do
  (paramScope, params') <- bindTyVarBinders (typeFamilyDeclParams familyDecl)
  (head', resultSig', equations') <-
    extendScope paramScope $
      (,,)
        <$> resolveType (typeFamilyDeclHead familyDecl)
        <*> traverse resolveTypeFamilyResultSig (typeFamilyDeclResultSig familyDecl)
        <*> traverse (mapM resolveTypeFamilyEq) (typeFamilyDeclEquations familyDecl)
  pure
    familyDecl
      { typeFamilyDeclHead = head',
        typeFamilyDeclParams = params',
        typeFamilyDeclResultSig = resultSig',
        typeFamilyDeclEquations = equations'
      }

resolveTypeFamilyResultSig :: TypeFamilyResultSig -> ResolveM TypeFamilyResultSig
resolveTypeFamilyResultSig resultSig =
  case resultSig of
    TypeFamilyKindSig ty -> TypeFamilyKindSig <$> resolveType ty
    TypeFamilyTyVarSig binder -> TypeFamilyTyVarSig <$> resolveTyVarBinderKind binder
    TypeFamilyInjectiveSig binder injectivity ->
      TypeFamilyInjectiveSig <$> resolveTyVarBinderKind binder <*> pure injectivity

resolveTyVarBinderKind :: TyVarBinder -> ResolveM TyVarBinder
resolveTyVarBinderKind binder = do
  kind' <- traverse resolveType (tyVarBinderKind binder)
  pure binder {tyVarBinderKind = kind'}

resolveTypeFamilyEq :: TypeFamilyEq -> ResolveM TypeFamilyEq
resolveTypeFamilyEq equation = do
  (forallScope, forallBinders') <- bindTyVarBinders (typeFamilyEqForall equation)
  (lhs', rhs') <-
    extendScope forallScope $
      (,)
        <$> resolveType (typeFamilyEqLhs equation)
        <*> resolveType (typeFamilyEqRhs equation)
  pure
    equation
      { typeFamilyEqForall = forallBinders',
        typeFamilyEqLhs = lhs',
        typeFamilyEqRhs = rhs'
      }

resolveTypeFamilyInst :: TypeFamilyInst -> ResolveM TypeFamilyInst
resolveTypeFamilyInst familyInst = do
  (forallScope, forallBinders') <- bindTyVarBinders (typeFamilyInstForall familyInst)
  (lhs', rhs') <-
    extendScope forallScope $
      (,)
        <$> resolveType (typeFamilyInstLhs familyInst)
        <*> resolveType (typeFamilyInstRhs familyInst)
  pure
    familyInst
      { typeFamilyInstForall = forallBinders',
        typeFamilyInstLhs = lhs',
        typeFamilyInstRhs = rhs'
      }

resolveDataFamilyDecl :: DataFamilyDecl -> ResolveM DataFamilyDecl
resolveDataFamilyDecl familyDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "data family " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case dataFamilyDeclHead familyDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  kind' <- traverse resolveType (dataFamilyDeclKind familyDecl)
  pure familyDecl {dataFamilyDeclHead = head', dataFamilyDeclKind = kind'}

resolveDataFamilyInst :: DataFamilyInst -> ResolveM DataFamilyInst
resolveDataFamilyInst familyInst = do
  scope <- currentScope
  (forallScope, forallBinders') <- bindTyVarBinders (dataFamilyInstForall familyInst)
  (head', kind', constructors', deriving') <-
    extendScope forallScope $
      (,,,)
        <$> resolveType (dataFamilyInstHead familyInst)
        <*> traverse resolveType (dataFamilyInstKind familyInst)
        <*> mapM resolveDataConDecl (dataFamilyInstConstructors familyInst)
        <*> mapM resolveDerivingClause (dataFamilyInstDeriving familyInst)
  pure
    familyInst
      { dataFamilyInstForall = forallBinders',
        dataFamilyInstHead = head',
        dataFamilyInstKind = kind',
        dataFamilyInstConstructors = map (resolveDataConDefinitions scope) constructors',
        dataFamilyInstDeriving = deriving'
      }

resolveTypeSynDecl :: TypeSynDecl -> ResolveM TypeSynDecl
resolveTypeSynDecl typeSynDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "type " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case typeSynHead typeSynDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  body' <- resolveType (typeSynBody typeSynDecl)
  pure typeSynDecl {typeSynHead = head', typeSynBody = body'}

resolveNewtypeDecl :: NewtypeDecl -> ResolveM NewtypeDecl
resolveNewtypeDecl newtypeDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "newtype " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case newtypeDeclHead newtypeDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  kind' <- traverse resolveType (newtypeDeclKind newtypeDecl)
  constructor' <- traverse resolveDataConDecl (newtypeDeclConstructor newtypeDecl)
  deriving' <- mapM resolveDerivingClause (newtypeDeclDeriving newtypeDecl)
  pure
    newtypeDecl
      { newtypeDeclHead = head',
        newtypeDeclKind = kind',
        newtypeDeclConstructor = resolveDataConDefinitions scope <$> constructor',
        newtypeDeclDeriving = deriving'
      }

resolveDataConDecl :: DataConDecl -> ResolveM DataConDecl
resolveDataConDecl dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> DataConAnn ann <$> withPushedSpan ann (resolveDataConDecl inner)
    PrefixCon forallVars context name bangTypes -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        PrefixCon forallVars' <$> mapM resolveType context <*> pure name <*> mapM resolveBangType bangTypes
    InfixCon forallVars context lhs name rhs -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        InfixCon forallVars' <$> mapM resolveType context <*> resolveBangType lhs <*> pure name <*> resolveBangType rhs
    RecordCon forallVars context name fields -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        RecordCon forallVars' <$> mapM resolveType context <*> pure name <*> mapM resolveFieldDecl fields
    GadtCon forallVars context names body ->
      GadtCon forallVars <$> mapM resolveType context <*> pure names <*> resolveGadtBody body
    TupleCon forallVars context flavor fields -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        TupleCon forallVars' <$> mapM resolveType context <*> pure flavor <*> mapM resolveBangType fields
    UnboxedSumCon forallVars context pos arity field -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        UnboxedSumCon forallVars' <$> mapM resolveType context <*> pure pos <*> pure arity <*> resolveBangType field
    ListCon forallVars context -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        ListCon forallVars' <$> mapM resolveType context
  where
    resolveBangType bt = do
      ty' <- resolveType (bangType bt)
      pure bt {bangType = ty'}
    resolveFieldDecl fieldDecl = do
      fieldType' <- resolveBangType (fieldType fieldDecl)
      pure fieldDecl {fieldType = fieldType'}

resolveGadtBody :: GadtBody -> ResolveM GadtBody
resolveGadtBody body =
  case body of
    GadtPrefixBody bangTypes ty ->
      GadtPrefixBody <$> mapM resolveBangTypePair bangTypes <*> resolveType ty
    GadtRecordBody fields ty ->
      GadtRecordBody <$> mapM resolveFieldDecl fields <*> resolveType ty
  where
    resolveBangTypePair (bt, arrowKind) = do
      bt' <- resolveBangType bt
      pure (bt', arrowKind)
    resolveBangType bt = do
      ty' <- resolveType (bangType bt)
      pure bt {bangType = ty'}
    resolveFieldDecl fieldDecl = do
      fieldType' <- resolveBangType (fieldType fieldDecl)
      pure fieldDecl {fieldType = fieldType'}

resolveType :: Type -> ResolveM Type
resolveType ty =
  case ty of
    TAnn ann inner -> withPushedSpan ann (resolveType inner)
    TVar name ->
      TVar <$> resolveScopedTypeVariableUse name
    TCon name promoted ->
      TCon <$> resolveTypeConstructorUse promoted name <*> pure promoted
    TBuiltinCon {} -> pure ty
    TImplicitParam name inner ->
      TImplicitParam name <$> resolveType inner
    TTypeLit {} -> pure ty
    TStar {} -> pure ty
    TForall telescope inner -> do
      (binderScope, binders') <- withResetLocalSupply (bindTyVarBinders (forallTelescopeBinders telescope))
      inner' <- extendScope binderScope (resolveType inner)
      pure (TForall (telescope {forallTelescopeBinders = binders'}) inner')
    TApp left right ->
      TApp <$> resolveType left <*> resolveType right
    TTypeApp left right ->
      TTypeApp <$> resolveType left <*> resolveType right
    TInfix left name promoted right ->
      TInfix <$> resolveType left <*> resolveTypeConstructorUse promoted name <*> pure promoted <*> resolveType right
    TFun arrowKind left right ->
      TFun <$> resolveArrowKind arrowKind <*> resolveType left <*> resolveType right
    TTuple flavor promotion items -> do
      items' <- mapM resolveType items
      sp <- currentSpan
      let namespace = typePromotionNamespace promotion
          syntaxResolution = ResolutionAnnotation sp (IdentifierTuple flavor (length items)) namespace ResolvedSyntax
          tupleType = TTuple flavor promotion items'
      pure (annotateTypeSyntax sp syntaxResolution tupleType)
    TUnboxedSum items ->
      TUnboxedSum <$> mapM resolveType items
    TList promotion items -> do
      items' <- mapM resolveType items
      sp <- currentSpan
      let namespace = typePromotionNamespace promotion
          syntaxResolution = ResolutionAnnotation sp IdentifierList namespace ResolvedSyntax
          listType = TList promotion items'
      pure (annotateTypeSyntax sp syntaxResolution listType)
    TParen inner ->
      TParen <$> resolveType inner
    TKindSig inner kind ->
      TKindSig <$> resolveType inner <*> resolveType kind
    TContext constraints inner ->
      TContext <$> mapM resolveType constraints <*> resolveType inner
    TSplice expr ->
      TSplice <$> withResetLocalSupply (resolveExpr expr)
    TWildcard -> pure ty
    TQuasiQuote {} -> annotateUnhandledType <$> currentSpan <*> pure ty

resolveArrowKind :: ArrowKind -> ResolveM ArrowKind
resolveArrowKind arrowKind =
  case arrowKind of
    ArrowUnrestricted -> pure arrowKind
    ArrowLinear -> pure arrowKind
    ArrowExplicit ty -> ArrowExplicit <$> resolveType ty

resolveTypeSignature :: Type -> ResolveM (Scope, Type)
resolveTypeSignature ty =
  case ty of
    -- Type signatures may carry span-only 'TAnn' wrappers (see 'typeAnnSpan'); peel
    -- them so we still allocate scoped type variables and advance 'nextLocal'.
    TAnn ann sub -> withPushedSpan ann (resolveTypeSignature sub)
    TForall telescope inner -> do
      (binderScope, binders') <- bindTyVarBinders (forallTelescopeBinders telescope)
      inner' <- extendScope binderScope (resolveType inner)
      pure (binderScope, TForall (telescope {forallTelescopeBinders = binders'}) inner')
    _ -> do
      ty' <- resolveType ty
      pure (emptyScope, ty')

annotateTypeSyntax :: SourceSpan -> ResolutionAnnotation -> Type -> Type
annotateTypeSyntax sp resolution =
  TAnn (mkAnnotation resolution) . TAnn (mkAnnotation sp)

bindTyVarBinders :: [TyVarBinder] -> ResolveM (Scope, [TyVarBinder])
bindTyVarBinders =
  foldM step (emptyScope, [])
  where
    step (boundScope, acc) binder = do
      binder' <- extendScope boundScope (traverseTyVarBinderKind binder)
      let binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
      resolvedName <- freshLocal binderName
      let boundScope' = insertType (tyVarBinderName binder) resolvedName boundScope
      pure (boundScope', acc <> [binder'])
    traverseTyVarBinderKind binder = do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure binder {tyVarBinderKind = kind'}

allocateLocalDeclBinders :: [Decl] -> ResolveM (Map.Map Text ResolvedName, Scope)
allocateLocalDeclBinders =
  foldM step (Map.empty, emptyScope)
  where
    step acc decl = foldM addBinder acc (declBinderCandidates decl)
    addBinder (targets, scope) (_, name) = do
      resolvedName <- freshLocal name
      let key = renderUnqualifiedName name
      pure (Map.insert key resolvedName targets, insertTerm key resolvedName scope)

-- | Collect all term binders introduced by a declaration (handles tuple patterns etc.)
declBinderCandidates :: Decl -> [(SourceSpan, UnqualifiedName)]
declBinderCandidates decl =
  let (outerSp, innerDecl) = peelDeclSpan NoSourceSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = effectiveResolutionSpan outerSp NoSourceSpan
               in [(spanStartNameSpan loc (renderUnqualifiedName name), name)]
            PatternBind _ pat _ ->
              let loc = effectiveResolutionSpan outerSp (peelPatternSpan NoSourceSpan pat)
               in collectPatVarBinders loc pat
        DeclTypeSig [name] _ ->
          [(spanStartNameSpan outerSp (renderUnqualifiedName name), name)]
        _ -> []

declBinderCandidate :: Decl -> Maybe (SourceSpan, UnqualifiedName)
declBinderCandidate decl =
  let (outerSp, innerDecl) = peelDeclSpan NoSourceSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = effectiveResolutionSpan outerSp NoSourceSpan
               in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
            PatternBind _ pat _ ->
              case peelPatternAnn pat of
                PVar name ->
                  let loc =
                        effectiveResolutionSpan
                          (effectiveResolutionSpan outerSp NoSourceSpan)
                          (peelPatternSpan NoSourceSpan pat)
                   in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
                _ -> Nothing
        DeclTypeSig [name] _ ->
          Just (spanStartNameSpan outerSp (renderUnqualifiedName name), name)
        _ -> Nothing

topLevelTermDefinition :: Scope -> TermDefinition
topLevelTermDefinition scope name =
  Just (lookupTerm (renderUnqualifiedName name) scope)

resolveTermDefinitionAt :: SourceSpan -> TermDefinition -> UnqualifiedName -> UnqualifiedName
resolveTermDefinitionAt span' termDefinition name =
  case termDefinition name of
    Just resolved ->
      resolveUnqualifiedNameTo (spanStartNameSpan span' (renderUnqualifiedName name)) ResolutionNamespaceTerm resolved name
    Nothing -> name

resolveUnqualifiedNameTo :: SourceSpan -> ResolutionNamespace -> ResolvedName -> UnqualifiedName -> UnqualifiedName
resolveUnqualifiedNameTo span' namespace resolved name =
  name
    { unqualifiedNameAnns =
        mkAnnotation (ResolutionAnnotation span' (IdentifierNamed (renderUnqualifiedName name)) namespace resolved)
          : unqualifiedNameAnns name
    }

resolveNameTo :: SourceSpan -> ResolutionNamespace -> ResolvedName -> Name -> Name
resolveNameTo span' namespace resolved name =
  name
    { nameAnns =
        mkAnnotation (ResolutionAnnotation span' (IdentifierNamed (nameText name)) namespace resolved)
          : nameAnns name
    }

resolveTermUse :: Name -> ResolveM Name
resolveTermUse name = do
  sp <- currentSpan
  scope <- currentScope
  pure (resolveNameTo sp ResolutionNamespaceTerm (resolveTermName scope name) name)

resolveTermUseAtName :: Name -> ResolveM Name
resolveTermUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  pure (resolveNameTo (spanStartNameSpan sp (nameText name)) ResolutionNamespaceTerm (resolveTermName scope name) name)

data ResolvedInfixOp = ResolvedInfixOp
  { resolvedInfixIndex :: !Int,
    resolvedInfixName :: !Name,
    resolvedInfixFixity :: !OperatorFixity
  }

resolveInfixExpr :: Expr -> ResolveM Expr
resolveInfixExpr expr = do
  let (operands, names) = flattenInfixExpr expr
  operands' <- mapM resolveExpr operands
  names' <- mapM resolveTermUseAtName names
  let fallbackExpr = buildLeftInfixExpr expr operands' names'
  reassociateResolvedInfixExpr operands' names' fallbackExpr

reassociateResolvedInfixExpr :: [Expr] -> [Name] -> Expr -> ResolveM Expr
reassociateResolvedInfixExpr operands names fallbackExpr = do
  scope <- currentScope
  sp <- currentSpan
  let ops =
        [ ResolvedInfixOp index name (resolveFixityName scope name)
        | (index, name) <- zip [0 :: Int ..] names
        ]
  case ambiguousInfixOp ops of
    Just op ->
      pure (buildLeftInfixExpr fallbackExpr operands (replaceAt (resolvedInfixIndex op) (ambiguousFixityName sp op) names))
    Nothing ->
      pure (rebuildInfixExpr operands ops)

ambiguousFixityName :: SourceSpan -> ResolvedInfixOp -> Name
ambiguousFixityName ambient op =
  name
    { nameAnns =
        mkAnnotation
          ( ResolutionAnnotation
              (effectiveResolutionSpan (spanStartNameSpan ambient (nameText name)) (sourceSpanFromAnns (nameAnns name)))
              (IdentifierNamed (nameText name))
              ResolutionNamespaceTerm
              (ResolvedError "ambiguous fixity")
          )
          : filter (not . isResolutionAnnotation) (nameAnns name)
    }
  where
    name = resolvedInfixName op

isResolutionAnnotation :: Annotation -> Bool
isResolutionAnnotation ann =
  not (null (maybeToList (fromAnnotation ann :: Maybe ResolutionAnnotation)))

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index replacement =
  zipWith (\i value -> if i == index then replacement else value) [0 :: Int ..]

buildLeftInfixExpr :: Expr -> [Expr] -> [Name] -> Expr
buildLeftInfixExpr fallbackExpr [] _ = fallbackExpr
buildLeftInfixExpr _ (operand : operands) ops =
  List.foldl' (\left (op, right) -> EInfix left op right) operand (zip ops operands)

flattenInfixExpr :: Expr -> ([Expr], [Name])
flattenInfixExpr expr =
  case expr of
    EInfix left op right ->
      let (operands, ops) = flattenInfixExpr left
       in (operands <> [right], ops <> [op])
    _ -> ([expr], [])

ambiguousInfixOp :: [ResolvedInfixOp] -> Maybe ResolvedInfixOp
ambiguousInfixOp ops =
  listToMaybe
    [ right
    | (leftIndex, left) <- indexed,
      let leftPrec = infixPrecedence left,
      (rightIndex, right) <- drop (leftIndex + 1) indexed,
      infixPrecedence right == leftPrec,
      all ((> leftPrec) . infixPrecedence) [between | (index, between) <- indexed, index > leftIndex, index < rightIndex],
      incompatibleSamePrecedence left right
    ]
  where
    indexed = zip [0 :: Int ..] ops

incompatibleSamePrecedence :: ResolvedInfixOp -> ResolvedInfixOp -> Bool
incompatibleSamePrecedence left right =
  infixAssoc left /= infixAssoc right || infixAssoc left == Infix || infixAssoc right == Infix

infixAssoc :: ResolvedInfixOp -> FixityAssoc
infixAssoc = operatorFixityAssoc . resolvedInfixFixity

infixPrecedence :: ResolvedInfixOp -> Int
infixPrecedence = operatorFixityPrecedence . resolvedInfixFixity

rebuildInfixExpr :: [Expr] -> [ResolvedInfixOp] -> Expr
rebuildInfixExpr = rebuildInfix EInfix

-- | The operands and operators of a left-nested infix pattern chain, as
-- the parser gives it.
flattenInfixPattern :: Pattern -> ([Pattern], [Name])
flattenInfixPattern pat =
  case pat of
    PInfix left op right ->
      let (operands, ops) = flattenInfixPattern left
       in (operands <> [right], ops <> [op])
    _ -> ([pat], [])

-- | Rebuild a resolved infix pattern chain with the fixities of its
-- operators, like an infix expression. An ambiguous pair keeps the left
-- nesting and marks the operator.
reassociateResolvedInfixPattern :: [Pattern] -> [Name] -> ResolveM Pattern
reassociateResolvedInfixPattern operands names = do
  scope <- currentScope
  sp <- currentSpan
  let ops =
        [ ResolvedInfixOp index name (resolveFixityName scope name)
        | (index, name) <- zip [0 :: Int ..] names
        ]
  case ambiguousInfixOp ops of
    Just op ->
      pure (buildLeftInfixPattern operands (replaceAt (resolvedInfixIndex op) (ambiguousFixityName sp op) names))
    Nothing ->
      pure (rebuildInfix PInfix operands ops)

buildLeftInfixPattern :: [Pattern] -> [Name] -> Pattern
buildLeftInfixPattern [] _ = error "flattenInfixPattern returned no operands"
buildLeftInfixPattern (operand : operands) ops =
  List.foldl' (\left (op, right) -> PInfix left op right) operand (zip ops operands)

-- | Rebuild an infix chain by operator precedence and associativity. The
-- builder makes one infix node.
rebuildInfix :: (a -> Name -> a -> a) -> [a] -> [ResolvedInfixOp] -> a
rebuildInfix build (operand : operands) ops =
  let (result, _, _) = parseInfix build 0 operand operands ops
   in result
rebuildInfix _ [] _ = error "an infix chain has no operands"

parseInfix :: (a -> Name -> a -> a) -> Int -> a -> [a] -> [ResolvedInfixOp] -> (a, [a], [ResolvedInfixOp])
parseInfix build minPrec lhs operands ops =
  case ops of
    op : restOps
      | infixPrecedence op >= minPrec,
        rhsOperand : restOperands <- operands ->
          let nextMinPrec =
                case infixAssoc op of
                  InfixR -> infixPrecedence op
                  Infix -> infixPrecedence op + 1
                  InfixL -> infixPrecedence op + 1
              (rhs, operands', ops') = parseInfix build nextMinPrec rhsOperand restOperands restOps
           in parseInfix build minPrec (build lhs (resolvedInfixName op) rhs) operands' ops'
    _ -> (lhs, operands, ops)

resolveTypeConstructorUse :: TypePromotion -> Name -> ResolveM Name
resolveTypeConstructorUse promotion name =
  case promotion of
    Unpromoted -> do
      sp <- currentSpan
      scope <- currentScope
      pure (resolveNameTo sp ResolutionNamespaceType (resolveTypeName scope name) name)
    Promoted -> do
      sp <- currentSpan
      scope <- currentScope
      pure (resolveNameTo sp ResolutionNamespaceTerm (resolveTermName scope name) name)

typePromotionNamespace :: TypePromotion -> ResolutionNamespace
typePromotionNamespace promotion =
  case promotion of
    Unpromoted -> ResolutionNamespaceType
    Promoted -> ResolutionNamespaceTerm

resolveTypeUseAtName :: Name -> ResolveM Name
resolveTypeUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  let nameSpan = effectiveResolutionSpan (spanStartNameSpan sp (nameText name)) (sourceSpanFromAnns (nameAnns name))
  pure (resolveNameTo nameSpan ResolutionNamespaceType (resolveTypeName scope name) name)

resolveScopedTypeVariableUse :: UnqualifiedName -> ResolveM UnqualifiedName
resolveScopedTypeVariableUse name = do
  sp <- currentSpan
  scope <- currentScope
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
  pure $
    case resolved of
      ResolvedError _ -> name
      _ -> resolveUnqualifiedNameTo sp ResolutionNamespaceType resolved name

resolveDataConDefinitions :: Scope -> DataConDecl -> DataConDecl
resolveDataConDefinitions scope =
  go NoSourceSpan
  where
    go ambient current =
      case current of
        DataConAnn ann inner -> DataConAnn ann (go (pushSpanFromAnn ambient ann) inner)
        PrefixCon forallVars context name bangTypes ->
          PrefixCon forallVars context (resolveConstructor ambient name) bangTypes
        RecordCon forallVars context name fields ->
          RecordCon forallVars context (resolveConstructor ambient name) fields
        InfixCon forallVars context lhs name rhs ->
          InfixCon forallVars context lhs (resolveConstructor ambient name) rhs
        GadtCon forallVars context names body ->
          GadtCon forallVars context (map (resolveConstructor ambient) names) body
        TupleCon {} -> current
        UnboxedSumCon {} -> current
        ListCon {} ->
          let resolution = ResolutionAnnotation ambient IdentifierList ResolutionNamespaceTerm ResolvedSyntax
           in DataConAnn (mkAnnotation resolution) current

    resolveConstructor span' name =
      let rendered = renderUnqualifiedName name
       in resolveUnqualifiedNameTo
            (spanStartNameSpan span' rendered)
            ResolutionNamespaceTerm
            (lookupTerm rendered scope)
            name
