{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Convert a checked module into System FC types, axioms, and values.
module Aihc.Fc.Desugar
  ( desugarModuleFc,
    DesugarConfig (..),
    moduleDesugarConfig,
    allPublicDesugarConfig,
    FcDesugarResult (..),
  )
where

import Aihc.Fc.Convert
import Aihc.Fc.Desugar.Value (desugarValues, prepareValueInterface)
import Aihc.Fc.Imports (emptyImports, importsForProgramLookup)
import Aihc.Fc.Name
import Aihc.Fc.Normalize (normalizeProgram)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgramWithTidiedImports, tidyTypeEnv)
import Aihc.Fc.TypeOf qualified as TypeOf
import Aihc.Fc.Wired (ghcTypesModule)
import Aihc.Parser.Syntax
  ( DataDecl (..),
    Module (..),
    TypeFamilyDecl (..),
    TypeSynDecl (..),
    UnqualifiedName,
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    nameQualifier,
    peelDeclAnn,
    tyVarBinderName,
    unqualifiedNameAnns,
    unqualifiedNameText,
  )
import Aihc.Parser.Syntax qualified as Syn
import Aihc.Resolve (ModuleExports, Package (..), PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..), exportedLocalNames)
import Aihc.Tc
  ( AssociatedTypeInfo (..),
    ClassInfo (..),
    DataConFieldInfo (..),
    DataConInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    TcBindingResult (..),
    TcInterface (..),
    TcTermKey (..),
    TyConFlavor (..),
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    defaultMethodName,
    tcInterfaceClasses,
    tcInterfaceDataFamilyInstances,
    tcInterfaceDataTypes,
    tcInterfaceInstances,
    tcInterfaceTerms,
    tcInterfaceTyCons,
    tcInterfaceTypeFamilyInstances,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typeFamilyAxiomKey,
  )
import Aihc.Tc.Annotations (TcInstanceAnnotation (..))
import Aihc.Tc.Env (DataConSourceForm (..), TypeSynonymInfo (..))
import Aihc.Tc.Types
  ( Pred (..),
    TcAxiomKey (..),
    TcType (..),
    TcTypeKey,
    TyVarId,
    TypeScheme (..),
    Unique (..),
    isEqualityTyCon,
    tyConKey,
    tyConModuleName,
    tyConName,
    tyConNamespace,
    tyConPackageId,
    typeSchemeBody,
    pattern KConstraint,
    pattern KFun,
    pattern KType,
  )
import Control.Monad (zipWithM)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data FcDesugarResult = FcDesugarResult
  { dsProgram :: !Program,
    dsSuccess :: !Bool,
    dsErrors :: ![String]
  }
  deriving (Show)

data DesugarConfig = DesugarConfig
  { primPackageId :: PackageId,
    -- | The visible top-level names of the module, as
    -- 'Aihc.Resolve.exportedLocalNames' gives them. A name outside the set
    -- is private, so no other module can name it and the backend need not
    -- give it a symbol.
    exportedNames :: !(Maybe (Set (ResolutionNamespace, Text)))
  }
  deriving (Eq, Show)

-- | The desugaring configuration of one module, with the visibility of its
-- top-level names taken from the resolver export scope. A module that the
-- scope map does not mention keeps every name public.
moduleDesugarConfig :: PackageId -> Package -> Text -> ModuleExports -> DesugarConfig
moduleDesugarConfig prim package moduleName' exports =
  DesugarConfig
    { primPackageId = prim,
      exportedNames = Set.union (compilerVisibleNames moduleName') <$> exportedLocalNames package moduleName' exports
    }

-- | The configuration of a caller that knows of no export list, and so
-- keeps every top-level name public.
allPublicDesugarConfig :: PackageId -> DesugarConfig
allPublicDesugarConfig prim = DesugarConfig {primPackageId = prim, exportedNames = Nothing}

-- | The names of one module that the compiler builds references to on its
-- own, from whatever module it is desugaring, and that the export list of
-- the module therefore cannot hide.
--
-- @Type.Reflection@ keeps the representation of @TypeRep@ out of its export
-- list, yet 'Aihc.Fc.Desugar.Value.desugarTypeRepresentation' builds one for
-- every derived @Typeable@ instance, so the constructor belongs to the
-- interface of the module whatever the list says. This is the whole of the
-- set, not an excerpt: every other name that the Typeable and literal
-- machinery reaches for is one that its own module already exports.
--
-- Getting this wrong is quiet. A name that belongs here and is missing
-- becomes an undefined symbol at link time, not a compiler diagnostic.
compilerVisibleNames :: Text -> Set (ResolutionNamespace, Text)
compilerVisibleNames moduleName'
  | moduleName' == "Type.Reflection" = Set.singleton (ResolutionNamespaceTerm, "TypeRep")
  | otherwise = Set.empty

data HeaderSource
  = HeaderTerm !(TcTermKey, TypeScheme)
  | HeaderTyCon !TyConInfo
  | HeaderDataType !DataTypeInfo
  | HeaderClass !ClassInfo
  | HeaderDataCon !DataConInfo
  | HeaderSynonym !TyConInfo
  | HeaderNewtype !DataTypeInfo
  | HeaderInstance !InstanceInfo
  | HeaderDefaultMethod !Name !TypeScheme
  | HeaderFamilyEquation !TypeFamilyInstanceInfo
  | -- | A type family with its known equations. The equations come with
    -- the family, so a use of the family can reduce its applications.
    HeaderFamily !TyConInfo ![TypeFamilyInstanceInfo]
  | HeaderDataFamily !DataFamilyInstanceInfo

withConversionContext :: String -> Either String a -> Either String a
withConversionContext context =
  either (Left . ((context <> ": ") <>)) Right

interfaceConvertEnv :: DesugarConfig -> TcInterface -> ConvertEnv
interfaceConvertEnv config interface =
  withKindEnv
    (Map.fromList [(tyConKey (tciTyCon info), tciKindScheme info) | info <- tcInterfaceTyCons interface])
    ( withClassTyCons
        (map (tyConKey . ciTyCon) (tcInterfaceClasses interface))
        (withExportedNames (exportedNames config) (emptyConvertEnv (primPackageId config)))
    )

convertTyConHeader :: ConvertEnv -> TyConInfo -> Either String (Name, Type)
convertTyConHeader env info = do
  converted <- convertKindScheme env (tciKindScheme info)
  pure (tyConNameFc env (tciTyCon info), converted)

convertTermHeader :: ConvertEnv -> (TcTermKey, TypeScheme) -> Either String (Maybe (Name, Type))
convertTermHeader env (key, scheme) =
  case key of
    TcTermGlobal package moduleName' identifier -> do
      converted <-
        either
          (\message -> Left (T.unpack moduleName' <> "." <> T.unpack identifier <> ": " <> message))
          Right
          (convertTypeScheme env scheme)
      pure (Just (Name identifier SortValue (OriginTop package moduleName'), converted))
    TcTermLocal {} -> pure Nothing

convertInstanceHeader :: ConvertEnv -> InstanceInfo -> Either String (Name, Type)
convertInstanceHeader env info = do
  converted <- convertType env (iiDictType info)
  let (package, moduleName') = iiDictOrigin info
  pure (Name (iiDictName info) SortValue (OriginTop (PackageId package) moduleName'), converted)

convertKindScheme :: ConvertEnv -> TypeScheme -> Either String Type
convertKindScheme env (ForAll tyVars predicates body) = do
  let bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  convertedPredicates <- mapM (convertPred bindersEnv) predicates
  convertedBody <- convertKind bindersEnv body
  pure (foldr TyForAll (evidenceArrows bindersEnv body convertedPredicates convertedBody) binders)

convertTypeScheme :: ConvertEnv -> TypeScheme -> Either String Type
convertTypeScheme env (ForAll tyVars predicates body) = do
  let bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  convertedPredicates <- mapM (convertPred bindersEnv) predicates
  convertedBody <- convertType bindersEnv body
  pure (foldr TyForAll (evidenceArrows bindersEnv body convertedPredicates convertedBody) binders)

desugarModuleFc :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> FcDesugarResult
desugarModuleFc config bindings interface checked =
  if not (tcModuleSuccess checked)
    then failedDesugar (map show (tcModuleDiagnostics checked))
    else case desugarFromInterface config bindings interface checked of
      Left message -> failedDesugar [message]
      Right program ->
        FcDesugarResult
          { dsProgram = program,
            dsSuccess = True,
            dsErrors = []
          }

desugarFromInterface :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> Either String Program
desugarFromInterface config moduleBindings interface checked = do
  let convertEnv = interfaceConvertEnv config interface
      (packageId, currentModule) = resolvedModuleOrigin checked
      moduleOrigin = (packageId, currentModule)
      dataTypes = Map.fromList [(dataTypeSourceKey info, info) | info <- tcInterfaceDataTypes interface]
      tyCons = Map.fromList [(tyConSourceKey info, info) | info <- tcInterfaceTyCons interface]
      classes = Map.fromList [(classSourceKey info, info) | info <- tcInterfaceClasses interface]
      typeFamilyInstances = Map.fromList [(typeFamilyAxiomKey info, info) | info <- tcInterfaceTypeFamilyInstances interface]
      bindings = Map.union (localBindingMap packageId currentModule moduleBindings) (bindingsFromInterface interface)
      headers = headerIndex convertEnv interface
  typeDecls <-
    concat
      <$> mapM
        (dsDecl convertEnv packageId currentModule dataTypes tyCons classes typeFamilyInstances bindings)
        (Syn.moduleDecls checked)
  valueDecls <- desugarValues convertEnv moduleBindings (prepareValueInterface interface) moduleOrigin checked
  let decls = typeDecls <> valueDecls
      baseProgram = Program emptyScopeTable emptyImports decls
  imports <-
    importsForProgramLookup
      (primPackageId config)
      (lookupHeader convertEnv bindings headers)
      baseProgram
  let scopes = buildScopes (primPackageId config) moduleOrigin imports decls
  pure (tidyProgramWithTidiedImports (normalizeProgram (primPackageId config) (Program scopes imports decls)))

headerIndex :: ConvertEnv -> TcInterface -> Map.Map Name HeaderSource
headerIndex convertEnv interface =
  Map.fromList
    ( termFacts
        <> tyConFacts
        <> dataTypeFacts
        <> synonymFacts
        <> classFacts
        <> dataConFacts
        <> newtypeFacts
        <> instanceFacts
        <> defaultMethodFacts
        <> familyFacts
        <> dataFamilyFacts
    )
  where
    termFacts =
      [ (Name identifier SortValue (OriginTop package moduleName'), HeaderTerm (key, scheme))
      | (key@(TcTermGlobal package moduleName' identifier), scheme) <- tcInterfaceTerms interface,
        Set.notMember key familyConstructorKeys
      ]
    familyConstructorKeys =
      Set.fromList
        [ TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon) constructorName
        | info <- tcInterfaceDataFamilyInstances interface,
          let tyCon = dfiiRepresentationTyCon info,
          constructorName <- dfiiConstructorNames info
        ]
    tyConFacts =
      [ (tyConNameFc convertEnv (tciTyCon info), tyConHeader info)
      | info <- tcInterfaceTyCons interface,
        tciFlavor info `notElem` [SynonymTyCon, ClassTyCon]
      ]
    tyConHeader info
      | tciFlavor info == TypeFamilyTyCon = HeaderFamily info (familyEquations (tciTyCon info))
      | otherwise = HeaderTyCon info
    familyEquations tyCon =
      [ info
      | info <- tcInterfaceTypeFamilyInstances interface,
        TcTyCon family _ <- [tfiiLeft info],
        family == tyCon
      ]
    dataTypeFacts =
      [ (tyConNameFc convertEnv (dtiTyCon info), HeaderDataType info)
      | info <- tcInterfaceDataTypes interface,
        dtiFlavor info == DataTyCon
      ]
    synonymFacts =
      [ (Name (tciName info) SortSynonym (OriginTop (tyConPackageId (tciTyCon info)) (tyConModuleName (tciTyCon info))), HeaderSynonym info)
      | info <- tcInterfaceTyCons interface,
        tciFlavor info == SynonymTyCon
      ]
    classFacts =
      concat
        [ [ (classDictTypeName (ciTyCon info), HeaderClass info),
            (classDictConName (ciTyCon info), HeaderClass info)
          ]
        | info <- tcInterfaceClasses interface,
          not (isEqualityTyCon (ciTyCon info))
        ]
    dataConFacts =
      [ (Name (dciName constructor) SortDataConstructor (OriginTop package moduleName'), HeaderDataCon constructor)
      | dataType <- tcInterfaceDataTypes interface,
        constructor <- dtiConstructors dataType,
        let (package, moduleName') = dciOrigin constructor
      ]
    newtypeFacts =
      concat
        [ [ (tyConNameFc convertEnv (dtiTyCon info), HeaderNewtype info),
            (Name ("$ax$" <> dtiName info) SortAxiom (OriginTop (tyConPackageId (dtiTyCon info)) (tyConModuleName (dtiTyCon info))), HeaderNewtype info)
          ]
        | info <- tcInterfaceDataTypes interface,
          dtiFlavor info == NewtypeTyCon
        ]
    instanceFacts =
      [ (Name (iiDictName info) SortValue (OriginTop (PackageId package) moduleName'), HeaderInstance info)
      | info <- tcInterfaceInstances interface,
        let (package, moduleName') = iiDictOrigin info
      ]
    defaultMethodFacts =
      [ (workerName, HeaderDefaultMethod workerName workerScheme)
      | info <- tcInterfaceClasses interface,
        Just (package, moduleName') <- [ciOrigin info],
        methodName <- ciDefaultMethods info,
        Just methodScheme <- [lookup methodName (ciMethods info)],
        let workerName = Name (defaultMethodName methodName) SortValue (OriginTop (PackageId package) moduleName')
            workerScheme = maybe methodScheme (defaultWorkerScheme methodScheme) (lookup methodName (ciDefaultSignatures info))
      ]
    familyFacts =
      [ (lookupAxiomName (typeFamilyAxiomKey info), HeaderFamilyEquation info)
      | info <- tcInterfaceTypeFamilyInstances interface
      ]
    dataFamilyFacts =
      concat
        [ (Name (dfiiAxiomName info) SortAxiom origin, HeaderDataFamily info)
            : (Name ("$ax$" <> T.drop 1 (tyConName tyCon)) SortAxiom origin, HeaderDataFamily info)
            : (tyConNameFc convertEnv tyCon, HeaderDataFamily info)
            : [ (Name constructorName SortDataConstructor origin, HeaderDataFamily info)
              | constructorName <- dfiiConstructorNames info
              ]
        | info <- tcInterfaceDataFamilyInstances interface,
          let tyCon = dfiiRepresentationTyCon info
              origin = OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)
        ]
    defaultWorkerScheme ordinaryScheme (ForAll variables predicates body) =
      case ordinaryScheme of
        ForAll _ (classPredicate : _) _ -> ForAll variables (classPredicate : predicates) body
        _ -> ForAll variables predicates body

lookupHeader :: ConvertEnv -> Map.Map TcTermKey TcBindingResult -> Map.Map Name HeaderSource -> Name -> Either String (Maybe TypeOf.TypeEnv)
lookupHeader convertEnv bindings headers name =
  case Map.lookup name headers of
    Nothing -> Right Nothing
    Just source -> Just . tidyTypeEnv <$> convertHeader convertEnv bindings source

convertHeader :: ConvertEnv -> Map.Map TcTermKey TcBindingResult -> HeaderSource -> Either String TypeOf.TypeEnv
convertHeader convertEnv bindings source =
  case source of
    HeaderTerm keyScheme -> do
      converted <- convertTermHeader convertEnv keyScheme
      case converted of
        Nothing -> Right (TypeOf.emptyTypeEnv (cePrimPackage convertEnv))
        Just (headerName, headerType') -> Right (headerOnly (cePrimPackage convertEnv) headerName headerType')
    HeaderTyCon info -> do
      (headerName, headerType') <- convertTyConHeader convertEnv info
      Right (headerOnly (cePrimPackage convertEnv) headerName headerType')
    HeaderDataType info ->
      declsEnv convertEnv . (: []) =<< convertDataType convertEnv info
    HeaderClass info ->
      declsEnv convertEnv . (: []) =<< convertClass convertEnv info
    HeaderDataCon info -> do
      constructor <- convertConstructor convertEnv info
      Right (headerOnly (cePrimPackage convertEnv) (conName constructor) (conType constructor))
    HeaderSynonym info ->
      declsEnv convertEnv =<< convertSynonym convertEnv info
    HeaderNewtype info ->
      declsEnv convertEnv =<< convertNewtype convertEnv info
    HeaderInstance info -> do
      (headerName, headerType') <- convertInstanceHeader convertEnv info
      Right (headerOnly (cePrimPackage convertEnv) headerName headerType')
    HeaderDefaultMethod headerName scheme -> do
      converted <- convertTypeScheme convertEnv scheme
      Right (headerOnly (cePrimPackage convertEnv) headerName converted)
    HeaderFamilyEquation info ->
      declsEnv convertEnv . (: []) =<< convertTypeFamilyEquation convertEnv info
    HeaderFamily info equations -> do
      (headerName, headerType') <- convertTyConHeader convertEnv info
      equationDecls <- mapM (convertTypeFamilyEquation convertEnv) equations
      equationEnv <- declsEnv convertEnv equationDecls
      Right (TypeOf.unionTypeEnv (headerOnly (cePrimPackage convertEnv) headerName headerType') equationEnv)
    HeaderDataFamily info ->
      let tyCon = dfiiRepresentationTyCon info
       in declsEnv convertEnv =<< convertDataFamilyInst convertEnv (tyConPackageId tyCon) (tyConModuleName tyCon) bindings info

headerOnly :: PackageId -> Name -> Type -> TypeOf.TypeEnv
headerOnly primPackage headerName headerType' =
  (TypeOf.emptyTypeEnv primPackage) {TypeOf.teHeaders = Map.singleton headerName headerType'}

declsEnv :: ConvertEnv -> [Decl] -> Either String TypeOf.TypeEnv
declsEnv convertEnv declarations =
  Right (TypeOf.typeEnvFromProgram (cePrimPackage convertEnv) (Program emptyScopeTable emptyImports declarations))

bindingsFromInterface :: TcInterface -> Map.Map TcTermKey TcBindingResult
bindingsFromInterface interface =
  Map.fromList (termBindings <> instanceBindings <> defaultMethodBindings)
  where
    termBindings =
      [ (key, TcBindingResult identifier identifier (interfaceSchemeType scheme))
      | (key@(TcTermGlobal _ _ identifier), scheme) <- tcInterfaceTerms interface
      ]
    instanceBindings =
      [ (TcTermGlobal (PackageId package) moduleName' (iiDictName info), TcBindingResult (iiDictName info) (iiDictName info) (iiDictType info))
      | info <- tcInterfaceInstances interface,
        let (package, moduleName') = iiDictOrigin info
      ]
    defaultMethodBindings =
      [ (TcTermGlobal (PackageId package) moduleName' workerName, TcBindingResult workerName workerName (interfaceSchemeType workerScheme))
      | info <- tcInterfaceClasses interface,
        Just (package, moduleName') <- [ciOrigin info],
        methodName <- ciDefaultMethods info,
        Just methodScheme <- [lookup methodName (ciMethods info)],
        let workerName = defaultMethodName methodName
            workerScheme = maybe methodScheme (defaultWorkerScheme methodScheme) (lookup methodName (ciDefaultSignatures info))
      ]
    defaultWorkerScheme ordinaryScheme (ForAll variables predicates body) =
      case ordinaryScheme of
        ForAll _ (classPredicate : _) _ -> ForAll variables (classPredicate : predicates) body
        _ -> ForAll variables predicates body

interfaceSchemeType :: TypeScheme -> TcType
interfaceSchemeType (ForAll [] [] ty) = ty
interfaceSchemeType (ForAll variables [] ty) = foldr TcForAllTy ty variables
interfaceSchemeType (ForAll [] predicates ty) = TcQualTy predicates ty
interfaceSchemeType (ForAll variables predicates ty) = foldr TcForAllTy (TcQualTy predicates ty) variables

failedDesugar :: [String] -> FcDesugarResult
failedDesugar messages =
  FcDesugarResult
    { dsProgram = Program emptyScopeTable emptyImports [],
      dsSuccess = False,
      dsErrors = messages
    }

localBindingMap :: PackageId -> Text -> [TcBindingResult] -> Map.Map TcTermKey TcBindingResult
localBindingMap package moduleName' =
  Map.fromList
    . map (\binding -> (TcTermGlobal package moduleName' (tbName binding), binding))

dsDecl ::
  ConvertEnv ->
  PackageId ->
  Text ->
  Map.Map TcTypeKey DataTypeInfo ->
  Map.Map TcTypeKey TyConInfo ->
  Map.Map TcTypeKey ClassInfo ->
  Map.Map TcAxiomKey TypeFamilyInstanceInfo ->
  Map.Map TcTermKey TcBindingResult ->
  Syn.Decl ->
  Either String [Decl]
dsDecl env package moduleName' dataTypes tyCons classes typeFamilyInstances bindings decl =
  case decl of
    Syn.DeclAnn ann inner
      | Just familyInfo <- fromAnnotation ann ->
          convertDataFamilyInst env package moduleName' bindings familyInfo
      | Just familyEquation <- fromAnnotation ann ->
          (: []) <$> convertTypeFamilyEquation env familyEquation
      | Just instanceAnnotation <- fromAnnotation ann,
        Syn.DeclInstance {} <- peelDeclAnn inner ->
          -- The associated type family equations of an instance, explicit
          -- ones and instantiated class defaults, become axioms.
          mapM (convertTypeFamilyEquation env) (tcInstanceAssociatedTypes instanceAnnotation)
      | otherwise ->
          dsDecl env package moduleName' dataTypes tyCons classes typeFamilyInstances bindings inner
    _ ->
      case peelDeclAnn decl of
        Syn.DeclData dataDecl -> do
          info <- lookupDataType DataTyCon package moduleName' (unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))) dataTypes
          (: []) <$> convertDataType env info
        Syn.DeclTypeSyn synonymDecl -> do
          info <- lookupSynonym package moduleName' (unqualifiedNameText (binderHeadName (typeSynHead synonymDecl))) tyCons
          convertSynonym env info
        Syn.DeclClass classDecl -> do
          info <- lookupClassInfo package moduleName' (unqualifiedNameText (binderHeadName (Syn.classDeclHead classDecl))) classes
          -- Nominal equality uses coercions instead of a class dictionary.
          classDecls <-
            if isEqualityTyCon (ciTyCon info)
              then pure []
              else (: []) <$> convertClass env info
          -- Each associated type family of the class is an empty family
          -- type, the same as a top-level family declaration.
          families <-
            mapM
              ( \associated -> do
                  let familyName = tyConName (atiTyCon associated)
                  familyInfo <- lookupTyConFlavor TypeFamilyTyCon package moduleName' familyName tyCons
                  convertEmptyFamily env (associatedFamilyParamNames familyName classDecl) Nominal familyInfo
              )
              (ciAssociatedTypes info)
          pure (classDecls <> families)
        Syn.DeclNewtype newtypeDecl ->
          convertNewtype env
            =<< lookupDataType NewtypeTyCon package moduleName' (unqualifiedNameText (binderHeadName (Syn.newtypeDeclHead newtypeDecl))) dataTypes
        Syn.DeclDataFamilyDecl familyDecl -> do
          info <- lookupTyConFlavor DataFamilyTyCon package moduleName' (unqualifiedNameText (binderHeadName (Syn.dataFamilyDeclHead familyDecl))) tyCons
          (: []) <$> convertEmptyFamily env (map tyVarBinderName (binderHeadParams (Syn.dataFamilyDeclHead familyDecl))) Nominal info
        Syn.DeclTypeFamilyDecl familyDecl -> do
          let familyName = typeFamilyDeclName familyDecl
          info <- lookupTyConFlavor TypeFamilyTyCon package moduleName' familyName tyCons
          typeDecl <- convertEmptyFamily env (map tyVarBinderName (typeFamilyDeclParams familyDecl)) Nominal info
          axioms <-
            mapM
              (convertTypeFamilyEquation env)
              [ equation
              | equation <- Map.elems typeFamilyInstances,
                tfiiClosed equation,
                tfiiFamilyName equation == familyName,
                let (originPackage, originModule) = tfiiOrigin equation,
                originPackage == package,
                originModule == moduleName'
              ]
          pure (typeDecl : axioms)
        Syn.DeclForeign foreignDecl ->
          case Syn.foreignCallConv foreignDecl of
            Syn.CPrim -> Right []
            Syn.CCall -> Right []
            callConv -> Left ("unsupported System FC foreign calling convention: " <> show callConv)
        _ -> Right []

-- | The parameter names of an associated type family, as written in the
-- class body.
associatedFamilyParamNames :: Text -> Syn.ClassDecl -> [Text]
associatedFamilyParamNames familyName classDecl =
  concat
    [ map tyVarBinderName (Syn.typeFamilyDeclParams familyDecl)
    | item <- Syn.classDeclItems classDecl,
      Syn.ClassItemTypeFamilyDecl familyDecl <- [Syn.peelClassDeclItemAnn item],
      typeFamilyDeclName familyDecl == familyName
    ]

sourceTyConKey :: PackageId -> Text -> Text -> TcTypeKey
sourceTyConKey package moduleName' name =
  (package, moduleName', ResolutionNamespaceType, name)

dataTypeSourceKey :: DataTypeInfo -> TcTypeKey
dataTypeSourceKey info =
  let tyCon = dtiTyCon info
   in (tyConPackageId tyCon, tyConModuleName tyCon, tyConNamespace tyCon, dtiName info)

tyConSourceKey :: TyConInfo -> TcTypeKey
tyConSourceKey info =
  let tyCon = tciTyCon info
   in (tyConPackageId tyCon, tyConModuleName tyCon, tyConNamespace tyCon, tciName info)

classSourceKey :: ClassInfo -> TcTypeKey
classSourceKey info =
  let tyCon = ciTyCon info
   in (tyConPackageId tyCon, tyConModuleName tyCon, tyConNamespace tyCon, ciName info)

lookupDataType :: TyConFlavor -> PackageId -> Text -> Text -> Map.Map TcTypeKey DataTypeInfo -> Either String DataTypeInfo
lookupDataType flavor package moduleName' name dataTypes =
  case Map.lookup (sourceTyConKey package moduleName' name) dataTypes of
    Just info
      | dtiFlavor info == flavor -> Right info
    _ -> Left ("missing checked data type " <> T.unpack moduleName' <> "." <> T.unpack name)

lookupClassInfo :: PackageId -> Text -> Text -> Map.Map TcTypeKey ClassInfo -> Either String ClassInfo
lookupClassInfo package moduleName' name classes =
  case Map.lookup (sourceTyConKey package moduleName' name) classes of
    Just info -> Right info
    Nothing -> Left ("missing checked class " <> T.unpack moduleName' <> "." <> T.unpack name)

lookupTyConFlavor :: TyConFlavor -> PackageId -> Text -> Text -> Map.Map TcTypeKey TyConInfo -> Either String TyConInfo
lookupTyConFlavor flavor package moduleName' name tyCons =
  case Map.lookup (sourceTyConKey package moduleName' name) tyCons of
    Just info
      | tciFlavor info == flavor -> Right info
    _ -> Left ("missing checked type constructor " <> T.unpack moduleName' <> "." <> T.unpack name)

typeFamilyDeclName :: TypeFamilyDecl -> Text
typeFamilyDeclName familyDecl =
  fromMaybe "<type-family>" (familyHeadName (typeFamilyDeclHead familyDecl))

familyHeadName :: Syn.Type -> Maybe Text
familyHeadName ty =
  case Syn.peelTypeHead ty of
    Syn.TCon name _ -> Just (Syn.nameText name)
    Syn.TInfix _ name _ _ -> Just (Syn.nameText name)
    Syn.TApp function _ -> familyHeadName function
    Syn.TTypeApp function _ -> familyHeadName function
    _ -> Nothing

convertClass :: ConvertEnv -> ClassInfo -> Either String Decl
convertClass env info = do
  let tyVars = ciKindTyVars info <> ciTyVars info
      bindersEnv = withTyVars tyVars env
      dictName = classDictTypeName (ciTyCon info)
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  result <- convertKind bindersEnv KType
  superFields <- mapM (convertType bindersEnv) (ciSuperClassTypes info)
  methodFields <- mapM (convertMethodField bindersEnv (ciName info) tyVars) (ciMethods info)
  let dictApp = foldl TyApp (TyCon dictName) (map (TyVar . binderName) binders)
      body = foldr (funType bindersEnv) dictApp (superFields <> methodFields)
      constructorType = foldr TyForAll body binders
      -- The dictionary type and its constructor carry a made-up name that no
      -- export list mentions. Any module that can write the class solves its
      -- constraints, so both follow the class itself.
      vis = exportedVis env ResolutionNamespaceType (ciName info)
  pure
    ( DeclType
        TypeDecl
          { typeVis = vis,
            typeName = dictName,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) Representational,
            typeCons = [ConDecl vis (classDictConName (ciTyCon info)) constructorType]
          }
    )

convertMethodField :: ConvertEnv -> Text -> [TyVarId] -> (Text, TypeScheme) -> Either String Type
convertMethodField env className classTyVars (_methodName, scheme) = do
  fieldType <- classMethodFieldType className classTyVars scheme
  convertType env fieldType

classMethodFieldType :: Text -> [TyVarId] -> TypeScheme -> Either String TcType
classMethodFieldType className classTyVars (ForAll methodTyVars predicates body) = do
  remaining <- removeClassPredicate className predicates
  let extraTyVars = filter (`notElem` classTyVars) methodTyVars
      qualifiedBody =
        if null remaining
          then body
          else TcQualTy remaining body
  Right (foldr TcForAllTy qualifiedBody extraTyVars)

removeClassPredicate :: Text -> [Pred] -> Either String [Pred]
removeClassPredicate className predicates =
  case predicates of
    [] -> Left ("class method lacks its class predicate for " <> T.unpack className)
    ClassPred tyCon _ : rest
      | tyConName tyCon == className -> Right rest
    predicate : rest -> (predicate :) <$> removeClassPredicate className rest

convertNewtype :: ConvertEnv -> DataTypeInfo -> Either String [Decl]
convertNewtype env info = do
  let tyCon = dtiTyCon info
  kindVars <- extraKindVars env tyCon (dtiTyVars info)
  let tyVars = kindVars <> dtiTyVars info
      bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  result <- convertKind bindersEnv (dtiResultKind info)
  representation <-
    case dtiConstructors info of
      [constructor]
        | [field] <- dciFields constructor ->
            convertType bindersEnv (dcfiType field)
      _ -> Left ("newtype " <> T.unpack (dtiName info) <> " does not have exactly one checked field")
  let typeName = tyConNameFc env tyCon
      lhs = foldl TyApp (TyCon typeName) (map (TyVar . binderName) binders)
      axiomName = Name ("$ax$" <> dtiName info) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
  pure
    [ DeclType
        TypeDecl
          { typeVis = exportedVis env ResolutionNamespaceType (dtiName info),
            typeName = typeName,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) Representational,
            typeCons = []
          },
      DeclAxiom
        AxiomDecl
          { axiomVis = Private,
            axiomName = axiomName,
            axiomBinders = binders,
            axiomRole = Representational,
            axiomLeft = lhs,
            axiomRight = representation
          }
    ]

convertEmptyFamily :: ConvertEnv -> [Text] -> Role -> TyConInfo -> Either String Decl
convertEmptyFamily env paramNames roles info = do
  let tyCon = tciTyCon info
      constructorKind = typeSchemeBody (tciKindScheme info)
      argKinds = take (tciArity info) (visibleArgKinds constructorKind)
      names =
        if length paramNames == length argKinds
          then paramNames
          else ["a" <> T.pack (show index) | index <- [1 .. length argKinds]]
  binders <- zipWithM (kindBinder env) names argKinds
  result <- convertKind env (dropKindParams (length binders) constructorKind)
  pure
    ( DeclType
        TypeDecl
          { typeVis = exportedVis env ResolutionNamespaceType (tciName info),
            typeName = tyConNameFc env tyCon,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) roles,
            typeCons = []
          }
    )

kindBinder :: ConvertEnv -> Text -> TcType -> Either String Binder
kindBinder env name kind = do
  converted <- convertKind env kind
  pure (Binder (Name name SortTypeVariable (OriginLocal (Unique 0))) converted)

visibleArgKinds :: TcType -> [TcType]
visibleArgKinds kind =
  case kind of
    KFun argument result -> argument : visibleArgKinds result
    _ -> []

dropKindParams :: Int -> TcType -> TcType
dropKindParams remaining kind
  | remaining <= 0 = kind
dropKindParams remaining (KFun _ result) = dropKindParams (remaining - 1) result
dropKindParams _ kind = kind

convertDataFamilyInst :: ConvertEnv -> PackageId -> Text -> Map.Map TcTermKey TcBindingResult -> DataFamilyInstanceInfo -> Either String [Decl]
convertDataFamilyInst env package moduleName' bindings info = do
  let tyVars = dfiiTyVars info
      bindersEnv = withTyVars tyVars env
      representationTyCon = dfiiRepresentationTyCon info
      representationName = tyConNameFc env representationTyCon
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  representationKind <- typeKindInEnv bindersEnv (TcTyCon representationTyCon (map TcTyVar tyVars))
  result <- convertKind bindersEnv representationKind
  familyType <- convertType bindersEnv (dfiiFamilyType info)
  let representationType = foldl TyApp (TyCon representationName) (map (TyVar . binderName) binders)
      familyAxiom =
        DeclAxiom
          AxiomDecl
            { axiomVis = Private,
              axiomName = Name (dfiiAxiomName info) SortAxiom (OriginTop package moduleName'),
              axiomBinders = binders,
              axiomRole = Nominal,
              axiomLeft = familyType,
              axiomRight = representationType
            }
  if dfiiIsNewtype info
    then do
      fieldType <-
        case dfiiConstructorNames info of
          constructorName : _ -> do
            constructorType <- lookupBindingType bindings package moduleName' constructorName
            converted <- convertType bindersEnv constructorType
            constructorFieldType converted
          [] -> Left "newtype family instance has no constructor"
      let representationAxiomName =
            Name ("$ax$" <> T.drop 1 (tyConName representationTyCon)) SortAxiom (OriginTop package moduleName')
      pure
        [ DeclType
            TypeDecl
              { typeVis = Private,
                typeName = representationName,
                typeBinders = binders,
                typeResult = result,
                typeRoles = replicate (length binders) Representational,
                typeCons = []
              },
          DeclAxiom
            AxiomDecl
              { axiomVis = Private,
                axiomName = representationAxiomName,
                axiomBinders = binders,
                axiomRole = Representational,
                axiomLeft = representationType,
                axiomRight = fieldType
              },
          familyAxiom
        ]
    else do
      constructors <- mapM (convertFamilyConstructor bindersEnv bindings package moduleName' representationType) (dfiiConstructorNames info)
      pure
        [ DeclType
            TypeDecl
              { typeVis = Private,
                typeName = representationName,
                typeBinders = binders,
                typeResult = result,
                typeRoles = replicate (length binders) Representational,
                typeCons = constructors
              },
          familyAxiom
        ]

convertFamilyConstructor :: ConvertEnv -> Map.Map TcTermKey TcBindingResult -> PackageId -> Text -> Type -> Text -> Either String ConDecl
convertFamilyConstructor bindersEnv bindings package moduleName' representationType constructorName = do
  constructorType <- lookupBindingType bindings package moduleName' constructorName
  converted <- convertType bindersEnv constructorType
  replaced <- replaceResultType converted representationType
  pure
    ConDecl
      { conVis = Private,
        conName = Name constructorName SortDataConstructor (OriginTop package moduleName'),
        conType = replaced
      }

lookupBindingType :: Map.Map TcTermKey TcBindingResult -> PackageId -> Text -> Text -> Either String TcType
lookupBindingType bindings package moduleName' name =
  case Map.lookup (TcTermGlobal package moduleName' name) bindings of
    Just binding -> Right (tbType binding)
    Nothing -> Left ("missing checked constructor type " <> T.unpack moduleName' <> "." <> T.unpack name)

replaceResultType :: Type -> Type -> Either String Type
replaceResultType ty result =
  case ty of
    TyForAll binder body -> TyForAll binder <$> replaceResultType body result
    TyFun r1 r2 argument body -> TyFun r1 r2 argument <$> replaceResultType body result
    _ -> Right result

constructorFieldType :: Type -> Either String Type
constructorFieldType ty =
  case ty of
    TyForAll _ body -> constructorFieldType body
    TyFun _ _ argument _ -> Right argument
    _ -> Left "newtype family constructor is not a function"

convertTypeFamilyEquation :: ConvertEnv -> TypeFamilyInstanceInfo -> Either String Decl
convertTypeFamilyEquation env info = do
  let bindersEnv = withTyVars (tfiiTyVars info) env
      TcAxiomKey package moduleName' axiomName = typeFamilyAxiomKey info
  binders <- mapM (tyVarBinder bindersEnv) (tfiiTyVars info)
  left <- convertType bindersEnv (tfiiLeft info)
  right <- convertType bindersEnv (tfiiRight info)
  pure
    ( DeclAxiom
        AxiomDecl
          { axiomVis = Private,
            axiomName = Name axiomName SortAxiom (OriginTop package moduleName'),
            axiomBinders = binders,
            axiomRole = Nominal,
            axiomLeft = left,
            axiomRight = right
          }
    )

lookupSynonym :: PackageId -> Text -> Text -> Map.Map TcTypeKey TyConInfo -> Either String TyConInfo
lookupSynonym = lookupTyConFlavor SynonymTyCon

convertDataType :: ConvertEnv -> DataTypeInfo -> Either String Decl
convertDataType env info = do
  let tyCon = dtiTyCon info
  kindVars <- extraKindVars env tyCon (dtiTyVars info)
  let tyVars = kindVars <> dtiTyVars info
      bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  result <- convertKind bindersEnv (dtiResultKind info)
  constructors <- mapM (convertConstructor env) (dtiConstructors info)
  pure
    ( DeclType
        TypeDecl
          { typeVis = exportedVis env ResolutionNamespaceType (dtiName info),
            typeName = tyConNameFc env tyCon,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) Representational,
            typeCons = constructors
          }
    )

convertConstructor :: ConvertEnv -> DataConInfo -> Either String ConDecl
convertConstructor env info = do
  let tyVars = dciUnivTyVars info <> dciExTyVars info
      bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  predicates <- mapM (convertPred bindersEnv) (dciTheta info)
  fields <- mapM (convertType bindersEnv . dcfiType) (dciFields info)
  result <- convertType bindersEnv (dciResTy info)
  body <-
    constructorFun
      bindersEnv
      (replicate (length predicates) Nothing <> map (Just . dcfiType) (dciFields info))
      (predicates <> fields)
      (dciResTy info)
      result
  let constructorType = foldr TyForAll body binders
      (package, moduleName') = dciOrigin info
      -- Built-in syntax such as @(,)@ or @[]@ has no name that an export
      -- list could mention, and the compiler references it from any
      -- module, so it stays public.
      constructorVis =
        case dciSourceForm info of
          SyntaxDataCon -> Pub
          _ -> exportedVis env ResolutionNamespaceTerm (dciName info)
  pure
    ConDecl
      { conVis = constructorVis,
        conName = Name (dciName info) SortDataConstructor (OriginTop package moduleName'),
        conType = constructorType
      }

constructorFun :: ConvertEnv -> [Maybe TcType] -> [Type] -> TcType -> Type -> Either String Type
constructorFun env fieldTys convertedFields resultTy convertedResult =
  go (zip fieldTys convertedFields)
  where
    go [] = Right convertedResult
    go ((maybeField, converted) : rest) = do
      restType <- go rest
      r1 <- maybe (Right (liftedRepType env)) (typeRepOrLifted env) maybeField
      r2 <-
        if null rest
          then typeRepOrLifted env resultTy
          else Right (liftedRepType env)
      Right (TyFun r1 r2 converted restType)

typeRepOrLifted :: ConvertEnv -> TcType -> Either String Type
typeRepOrLifted env ty =
  case typeRep env ty of
    Right representation -> Right representation
    Left _ -> Right (liftedRepType env)

-- | Convert a type synonym.
--
-- A constraint synonym has no System FC declaration. The type checker expands
-- it in every predicate before desugaring.
convertSynonym :: ConvertEnv -> TyConInfo -> Either String [Decl]
convertSynonym env info =
  case tciTypeSynonym info of
    Just synonym
      | Just {} <- tsiBody synonym,
        KConstraint <- synonymResultKind (tciKindScheme info) (tsiParams synonym) ->
          Right []
      | Just body <- tsiBody synonym -> do
          kindVars <- extraKindVars env (tciTyCon info) (tsiParams synonym)
          let tyVars = kindVars <> tsiParams synonym
              bindersEnv = withTyVars tyVars env
              bodyKind = synonymResultKind (tciKindScheme info) (tsiParams synonym)
          binders <- withConversionContext "binders" (mapM (tyVarBinder bindersEnv) tyVars)
          result <- withConversionContext "result" (synonymResult bindersEnv (tciKindScheme info) (tsiParams synonym))
          convertedBody <- withConversionContext "body" (convertTypeWithExpectedKind bindersEnv (Just bodyKind) body)
          pure
            [ DeclSynonym
                SynonymDecl
                  { synVis = exportedVis env ResolutionNamespaceType (tciName info),
                    synName = Name (tciName info) SortSynonym (OriginTop (tyConPackageId (tciTyCon info)) (tyConModuleName (tciTyCon info))),
                    synBinders = binders,
                    synResult = result,
                    synBody = convertedBody
                  }
            ]
      | otherwise -> Left ("type synonym " <> T.unpack (tciName info) <> " has no body")
    Nothing -> Left ("type synonym " <> T.unpack (tciName info) <> " has no synonym info")

-- | The result kind of a synonym. An eta-reduced synonym such as
-- @type RDoc = Doc@ keeps arrows after its parameters. They become the
-- binders that a type constructor kind has, so the kind of the body
-- matches the declared result.
synonymResult :: ConvertEnv -> TypeScheme -> [TyVarId] -> Either String Type
synonymResult env scheme params =
  residualKind (0 :: Int) (synonymResultKind scheme params)
  where
    residualKind index kind =
      case kind of
        KFun argument result -> do
          binder <- kindBinder env (T.pack ("eta" <> show index)) argument
          converted <- residualKind (index + 1) result
          pure (TyForAll binder converted)
        _ -> convertKind env kind

synonymResultKind :: TypeScheme -> [TyVarId] -> TcType
synonymResultKind scheme params =
  dropParams (length params) (typeSchemeBody scheme)
  where
    dropParams remaining kind
      | remaining <= 0 = kind
    dropParams remaining (KFun _ result) = dropParams (remaining - 1) result
    dropParams _ kind = kind

buildScopes :: PackageId -> (PackageId, Text) -> Imports -> [Decl] -> ScopeTable
buildScopes primPackage moduleOrigin imports decls =
  foldl
    ( \table (index, (package, moduleName')) ->
        insertScope index package moduleName' table
    )
    emptyScopeTable
    (zip [1 ..] origins)
  where
    origins =
      sort
        ( nub
            ( [moduleOrigin]
                <> [(primPackage, ghcTypesModule)]
                <> importsOrigins imports
                <> concatMap declOrigins decls
            )
        )

importsOrigins :: Imports -> [(PackageId, Text)]
importsOrigins imports =
  concatMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importHeaders imports))
    <> concatMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importSynonyms imports))
    <> concatMap (\(name, declaration) -> nameOriginPair name <> axiomOrigins declaration) (Map.toList (importAxioms imports))
    <> concatMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importBinders imports))

axiomOrigins :: AxiomDecl -> [(PackageId, Text)]
axiomOrigins declaration =
  concatMap binderOrigins (axiomBinders declaration)
    <> typeOrigins (axiomLeft declaration)
    <> typeOrigins (axiomRight declaration)

declOrigins :: Decl -> [(PackageId, Text)]
declOrigins decl =
  case decl of
    DeclType typeDecl ->
      nameOriginPair (typeName typeDecl)
        <> concatMap binderOrigins (typeBinders typeDecl)
        <> typeOrigins (typeResult typeDecl)
        <> concatMap conOrigins (typeCons typeDecl)
    DeclSynonym synonymDecl ->
      nameOriginPair (synName synonymDecl)
        <> concatMap binderOrigins (synBinders synonymDecl)
        <> typeOrigins (synResult synonymDecl)
        <> typeOrigins (synBody synonymDecl)
    DeclAxiom axiomDecl ->
      nameOriginPair (axiomName axiomDecl)
        <> concatMap binderOrigins (axiomBinders axiomDecl)
        <> typeOrigins (axiomLeft axiomDecl)
        <> typeOrigins (axiomRight axiomDecl)
    DeclVal valDecl ->
      nameOriginPair (valName valDecl)
        <> typeOrigins (valType valDecl)
        <> exprOrigins (valBody valDecl)

foreignImportDependencyOrigins :: ForeignImportDependency -> [(PackageId, Text)]
foreignImportDependencyOrigins dependency =
  case dependency of
    ForeignAxiom name -> nameOriginPair name
    ForeignConstructor name -> nameOriginPair name

conOrigins :: ConDecl -> [(PackageId, Text)]
conOrigins constructor =
  nameOriginPair (conName constructor) <> typeOrigins (conType constructor)

binderOrigins :: Binder -> [(PackageId, Text)]
binderOrigins binder = typeOrigins (binderType binder)

exprOrigins :: Expr -> [(PackageId, Text)]
exprOrigins expr =
  case expr of
    ExVar name -> nameOriginPair name
    ExLit literal -> literalOrigins literal
    ExApp function argument -> exprOrigins function <> exprOrigins argument
    ExTyApp function ty -> exprOrigins function <> typeOrigins ty
    ExLam binder body -> binderOrigins binder <> exprOrigins body
    ExTyLam binder body -> binderOrigins binder <> exprOrigins body
    ExLet bind body -> bindOrigins bind <> exprOrigins body
    ExRec binds body -> concatMap bindOrigins binds <> exprOrigins body
    ExCase scrutinee binder resultType alts ->
      exprOrigins scrutinee <> binderOrigins binder <> typeOrigins resultType <> concatMap altOrigins alts
    ExCoercion proof -> coercionOrigins proof
    ExCast inner coercion -> exprOrigins inner <> coercionOrigins coercion
    ExForeignCall call types arguments ->
      nameOriginPair (foreignCallName call)
        <> concatMap foreignImportDependencyOrigins (foreignCallDependencies call)
        <> typeOrigins (foreignCallType call)
        <> concatMap typeOrigins types
        <> concatMap exprOrigins arguments

bindOrigins :: Bind -> [(PackageId, Text)]
bindOrigins bind = binderOrigins (bindBinder bind) <> exprOrigins (bindRhs bind)

altOrigins :: Alt -> [(PackageId, Text)]
altOrigins alternative =
  altConOrigins (altCon alternative)
    <> concatMap binderOrigins (altTypeBinders alternative)
    <> concatMap binderOrigins (altBinders alternative)
    <> exprOrigins (altRhs alternative)

altConOrigins :: AltCon -> [(PackageId, Text)]
altConOrigins alternative =
  case alternative of
    AltData name -> nameOriginPair name
    AltLit literal -> literalOrigins literal
    AltDefault -> []

literalOrigins :: Literal -> [(PackageId, Text)]
literalOrigins literal =
  case literal of
    LitInt representation _ -> typeOrigins representation
    LitChar representation _ -> typeOrigins representation
    LitAddr representation _ -> typeOrigins representation

coercionOrigins :: Coercion -> [(PackageId, Text)]
coercionOrigins coercion =
  case coercion of
    CoVar name -> nameOriginPair name
    CoRefl ty -> typeOrigins ty
    CoSym inner -> coercionOrigins inner
    CoTrans left right -> coercionOrigins left <> coercionOrigins right
    CoTyConApp name arguments -> nameOriginPair name <> concatMap coercionOrigins arguments
    CoAxiom name arguments -> nameOriginPair name <> concatMap typeOrigins arguments

typeOrigins :: Type -> [(PackageId, Text)]
typeOrigins ty =
  case ty of
    TyVar name -> nameOriginPair name
    TyCon name -> nameOriginPair name
    TyApp function argument -> typeOrigins function <> typeOrigins argument
    TyFun r1 r2 argument result ->
      typeOrigins r1 <> typeOrigins r2 <> typeOrigins argument <> typeOrigins result
    TyForAll binder body -> binderOrigins binder <> typeOrigins body
    TyEq left right -> typeOrigins left <> typeOrigins right

nameOriginPair :: Name -> [(PackageId, Text)]
nameOriginPair name =
  case nameOrigin name of
    OriginTop package moduleName' -> [(package, moduleName')]
    OriginLocal {} -> []

resolvedModuleOrigin :: Module -> (PackageId, Text)
resolvedModuleOrigin resolvedModule =
  fromMaybe ("", fromMaybe "Main" (Syn.moduleName resolvedModule)) $ do
    resolved <- listToMaybe (mapMaybe definitionResolution (Syn.moduleDecls resolvedModule))
    case resolutionTarget resolved of
      ResolvedTopLevel packageId name ->
        pure (packageId, fromMaybe (fromMaybe "Main" (Syn.moduleName resolvedModule)) (nameQualifier name))
      _ -> Nothing

definitionResolution :: Syn.Decl -> Maybe ResolutionAnnotation
definitionResolution declaration =
  case peelDeclAnn declaration of
    Syn.DeclValue (Syn.FunctionBind name _) -> nameResolution name
    Syn.DeclValue (Syn.PatternBind _ pattern' _) -> patternResolution pattern'
    Syn.DeclData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration))
    Syn.DeclTypeSyn synonymDeclaration -> nameResolution (binderHeadName (typeSynHead synonymDeclaration))
    Syn.DeclNewtype newtypeDeclaration -> nameResolution (binderHeadName (Syn.newtypeDeclHead newtypeDeclaration))
    Syn.DeclClass classDeclaration -> nameResolution (binderHeadName (Syn.classDeclHead classDeclaration))
    Syn.DeclDataFamilyDecl familyDeclaration -> nameResolution (binderHeadName (Syn.dataFamilyDeclHead familyDeclaration))
    Syn.DeclForeign foreignDecl -> nameResolution (Syn.foreignName foreignDecl)
    _ -> Nothing

patternResolution :: Syn.Pattern -> Maybe ResolutionAnnotation
patternResolution pattern' =
  case pattern' of
    Syn.PVar name -> nameResolution name
    Syn.PAnn _ inner -> patternResolution inner
    Syn.PParen inner -> patternResolution inner
    Syn.PStrict inner -> patternResolution inner
    Syn.PIrrefutable inner -> patternResolution inner
    Syn.PAs name _ -> nameResolution name
    Syn.PTypeSig inner _ -> patternResolution inner
    _ -> Nothing

nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns
