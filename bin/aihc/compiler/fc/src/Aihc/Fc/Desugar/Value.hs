{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Direct value desugaring from checked source syntax to System FC.
module Aihc.Fc.Desugar.Value
  ( desugarValues,
    emptyPreparedValueInterface,
    mergePreparedValueInterfaces,
    prepareValueInterface,
    PreparedValueInterface,
  )
where

import Aihc.Fc.Convert
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf qualified as TypeOf
import Aihc.Parser.Syntax qualified as Syn
import Aihc.Resolve
  ( Identifier (..),
    PackageId (..),
    ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolvedName (..),
    displayIdentifier,
    packageIdText,
  )
import Aihc.Tc
  ( DataConFieldInfo (..),
    DataConInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    PatSynInfo (..),
    TcBindingResult (..),
    TcInterface (..),
    TcTermKey (..),
    TyConFlavor (..),
    defaultMethodName,
    patSynKey,
    tcInterfaceDataFamilyInstances,
    tcInterfaceDataTypes,
    tcInterfaceForeignImports,
    tcInterfaceInstances,
    tcInterfacePatSyns,
    tcInterfaceTerms,
  )
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcForeignAbiType (..),
    TcForeignEffect (..),
    TcForeignImportAnnotation (..),
    TcForeignImportInfo (..),
    TcForeignMarshal (..),
    TcForeignSafety (..),
    TcForeignTarget (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    TcPatSynAnnotation (..),
  )
import Aihc.Tc.Evidence qualified as Ev
import Aihc.Tc.Solve.Dict (matchTypes)
import Aihc.Tc.Types
  ( Pred (..),
    TcType (..),
    TyCon,
    TyVarId,
    TypeScheme (..),
    Unique (..),
    applySubst,
    applySubstPred,
    constraintTypeToPred,
    isUnliftedTypeInEnv,
    mkTyConWithOrigin,
    runtimeRepOfTypeInEnv,
    tvUnique,
    tyConModuleName,
    tyConName,
    tyConPackageId,
    typeKindType,
    pattern AddrRep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern Int8Rep,
    pattern IntRep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern Word8Rep,
    pattern WordRep,
  )
import Control.Applicative ((<|>))
import Control.Monad (unless, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper)
import Data.Foldable (foldrM)
import Data.Graph qualified as Graph
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Ratio (denominator, numerator)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data ValueState = ValueState
  { vsNextUnique :: !Int,
    vsModuleOrigin :: !(PackageId, Text),
    vsConvertEnv :: !ConvertEnv,
    vsBindingTypes :: !(Map TcTermKey TcType),
    vsLocals :: !(Map TcTermKey (Binder, TcType)),
    vsDictionaries :: !(Map Text Binder),
    -- | The evidence that the current binding shares, when it has a place to
    -- put the bindings. 'Nothing' turns sharing off.
    vsEvidenceScope :: !(Maybe EvidenceScope),
    vsConstructors :: !(Map Text [Name]),
    vsConstructorInfos :: !(Map Text [DataConInfo]),
    vsNewtypeConstructors :: !(Map TcTermKey DataTypeInfo),
    vsFamilyConstructors :: !(Map TcTermKey DataFamilyInstanceInfo),
    vsStrictConstructors :: !(Map TcTermKey [Bool]),
    vsPatSyns :: !(Map TcTermKey PatSynInfo),
    -- | The checked calling convention of each foreign import in scope.
    vsForeignImports :: !(Map TcTermKey TcForeignImportInfo)
  }

data PreparedValueInterface = PreparedValueInterface
  { preparedTypes :: !(Map TcTermKey TcType),
    preparedConstructors :: !(Map Text [Name]),
    preparedConstructorInfos :: !(Map Text [DataConInfo]),
    preparedNewtypeConstructors :: !(Map TcTermKey DataTypeInfo),
    preparedFamilyConstructors :: !(Map TcTermKey DataFamilyInstanceInfo),
    -- | Strict field flags of each data constructor that has one strict
    -- field or more. The list gives one flag for each source field.
    preparedStrictConstructors :: !(Map TcTermKey [Bool]),
    preparedPatSyns :: !(Map TcTermKey PatSynInfo),
    preparedForeignImports :: !(Map TcTermKey TcForeignImportInfo)
  }

type ValueM = StateT ValueState (Either String)

type MatchWork = (Syn.Match, [(TcTermKey, (Binder, TcType))])

data ValueGroup
  = FunctionGroup !TcTermKey !Text ![Syn.Match] !TcType
  | PatternGroup !TcTermKey !Text !(Syn.Rhs Syn.Expr) !TcType

-- | A top-level pattern binding with several binders, such as
-- @(low, high) = range x@, with the checked type of its right-hand side.
data TopPatternGroup = TopPatternGroup !Syn.Pattern !(Syn.Rhs Syn.Expr) !TcType

data LocalValueGroup
  = LocalNamedGroup !ValueGroup
  | LocalPatternGroup !Syn.Pattern !(Syn.Rhs Syn.Expr) !TcType !Bool
  | -- | @?x = e@ with the checked type of @e@.
    LocalImplicitParamGroup !Text !(Syn.Rhs Syn.Expr) !TcType

data LocalAllocation
  = LocalNamedAllocation !TcTermKey !Binder !TcType !ValueGroup
  | LocalPatternAllocation !Syn.Pattern !(Syn.Rhs Syn.Expr) !Binder !TcType ![(TcTermKey, Binder, TcType)] !Bool
  | LocalImplicitParamAllocation !Text !(Syn.Rhs Syn.Expr) !Binder !TcType

data TopValue = TopValue
  { topCoreName :: !Name,
    topType :: !TcType,
    topGroup :: !ValueGroup
  }

data Dictionary = Dictionary
  { dictionaryPredicate :: !Pred,
    dictionaryBinder :: !Binder
  }

-- | The superclass projections that one binding body shares. A projection is
-- a chain of cases on a dictionary, so a body that selects the same
-- superclass many times becomes much smaller when each chain has a name.
--
-- The scope holds only projections that the enclosing body can bind. A
-- construct that brings a new dictionary or type variable into scope opens
-- its own scope, so a projection never leaves the scope of its binder.
data EvidenceScope = EvidenceScope
  { evidenceCache :: !(Map Ev.EvTerm Binder),
    evidenceBindsRev :: ![Bind]
  }

emptyPreparedValueInterface :: PreparedValueInterface
emptyPreparedValueInterface =
  PreparedValueInterface
    { preparedTypes = Map.empty,
      preparedConstructors = Map.empty,
      preparedConstructorInfos = Map.empty,
      preparedNewtypeConstructors = Map.empty,
      preparedFamilyConstructors = Map.empty,
      preparedStrictConstructors = Map.empty,
      preparedPatSyns = Map.empty,
      preparedForeignImports = Map.empty
    }

prepareValueInterface :: TcInterface -> PreparedValueInterface
prepareValueInterface interface =
  PreparedValueInterface
    { preparedTypes = termTypes,
      preparedConstructors = constructors,
      preparedConstructorInfos = constructorInfos,
      preparedNewtypeConstructors = newtypes,
      preparedFamilyConstructors = familyConstructors,
      preparedStrictConstructors = strictConstructors,
      preparedPatSyns = Map.fromList [(patSynKey info, info) | info <- tcInterfacePatSyns interface],
      preparedForeignImports = Map.fromList (tcInterfaceForeignImports interface)
    }
  where
    termTypes =
      Map.fromList
        ( [ (key, schemeType scheme)
          | (key@(TcTermGlobal {}), scheme) <- tcInterfaceTerms interface
          ]
            <> [ (TcTermGlobal (PackageId package) moduleName' (iiDictName info), iiDictType info)
               | info <- tcInterfaceInstances interface,
                 let (package, moduleName') = iiDictOrigin info
               ]
        )
    constructors =
      Map.fromListWith
        (<>)
        [ (dciName constructor, [Name (dciName constructor) SortDataConstructor (OriginTop package moduleName')])
        | dataType <- tcInterfaceDataTypes interface,
          constructor <- dtiConstructors dataType,
          let (package, moduleName') = dciOrigin constructor
        ]
    schemeType (ForAll [] [] ty) = ty
    schemeType (ForAll variables [] ty) = foldr TcForAllTy ty variables
    schemeType (ForAll [] predicates ty) = TcQualTy predicates ty
    schemeType (ForAll variables predicates ty) = foldr TcForAllTy (TcQualTy predicates ty) variables

    constructorInfos =
      Map.fromListWith
        (<>)
        [ (dciName constructor, [constructor])
        | dataType <- tcInterfaceDataTypes interface,
          constructor <- dtiConstructors dataType
        ]
    newtypes =
      Map.fromList
        [ (TcTermGlobal package moduleName' (dciName constructor), dataType)
        | dataType <- tcInterfaceDataTypes interface,
          dtiFlavor dataType == NewtypeTyCon,
          constructor <- dtiConstructors dataType,
          let (package, moduleName') = dciOrigin constructor
        ]
    familyConstructors =
      Map.fromList
        [ (TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon) constructorName, info)
        | info <- tcInterfaceDataFamilyInstances interface,
          let tyCon = dfiiRepresentationTyCon info,
          constructorName <- dfiiConstructorNames info
        ]
    strictConstructors =
      Map.fromList
        [ (TcTermGlobal package moduleName' (dciName constructor), flags)
        | dataType <- tcInterfaceDataTypes interface,
          dtiFlavor dataType /= NewtypeTyCon,
          constructor <- dtiConstructors dataType,
          let flags = map dcfiStrict (dciFields constructor),
          or flags,
          let (package, moduleName') = dciOrigin constructor
        ]

mergePreparedValueInterfaces :: [PreparedValueInterface] -> PreparedValueInterface
mergePreparedValueInterfaces interfaces =
  PreparedValueInterface
    { preparedTypes = Map.unions (map preparedTypes interfaces),
      preparedConstructors = Map.unionsWith mergeCandidates (map preparedConstructors interfaces),
      preparedConstructorInfos = Map.unionsWith mergeCandidates (map preparedConstructorInfos interfaces),
      preparedNewtypeConstructors = Map.unions (map preparedNewtypeConstructors interfaces),
      preparedFamilyConstructors = Map.unions (map preparedFamilyConstructors interfaces),
      preparedStrictConstructors = Map.unions (map preparedStrictConstructors interfaces),
      preparedPatSyns = Map.unions (map preparedPatSyns interfaces),
      preparedForeignImports = Map.unions (map preparedForeignImports interfaces)
    }
  where
    mergeCandidates left right = List.nub (left <> right)

desugarValues :: ConvertEnv -> [TcBindingResult] -> PreparedValueInterface -> (PackageId, Text) -> Syn.Module -> Either String [Decl]
desugarValues convertEnv bindings interface moduleOrigin checked = do
  let (package, moduleName') = moduleOrigin
      localTypes =
        Map.fromList
          [ (TcTermGlobal package moduleName' (tbName binding), tbType binding)
          | binding <- bindings
          ]
      initialState =
        ValueState
          { vsNextUnique = 1000,
            vsModuleOrigin = moduleOrigin,
            vsConvertEnv = convertEnv,
            vsBindingTypes = Map.union localTypes (preparedTypes interface),
            vsLocals = Map.empty,
            vsDictionaries = Map.empty,
            vsEvidenceScope = Nothing,
            vsConstructors = preparedConstructors interface,
            vsConstructorInfos = preparedConstructorInfos interface,
            vsNewtypeConstructors = preparedNewtypeConstructors interface,
            vsFamilyConstructors = preparedFamilyConstructors interface,
            vsStrictConstructors = preparedStrictConstructors interface,
            vsPatSyns = preparedPatSyns interface,
            vsForeignImports = preparedForeignImports interface
          }
  fst <$> runStateT (desugarModuleValues checked) initialState

desugarModuleValues :: Syn.Module -> ValueM [Decl]
desugarModuleValues checked = do
  phaseOne <- concat <$> mapM desugarEarlyDecl (Syn.moduleDecls checked)
  instances <- concat <$> mapM desugarInstanceDecl (Syn.moduleDecls checked)
  patSyns <- concat <$> mapM desugarPatSynDecl (Syn.moduleDecls checked)
  (groups, patternGroups) <- groupValues (Syn.moduleDecls checked)
  tops <- mapM allocateTopValue groups
  values <- mapM desugarTopValue tops
  patternValues <- concat <$> mapM desugarTopPatternGroup patternGroups
  pure (phaseOne <> instances <> patSyns <> map DeclVal (values <> patternValues))

-- | Make the matcher and the builder of a pattern synonym. The matcher
-- @$mP@ takes the scrutinee, a success continuation with the argument
-- values, and a failure value. The builder @$bP@ is an ordinary function
-- from the checked builder equations. The type checker registers both
-- types.
desugarPatSynDecl :: Syn.Decl -> ValueM [Decl]
desugarPatSynDecl declaration =
  case patSynDeclAnnotation declaration of
    Just (annotation, patSyn) -> do
      moduleOrigin <- gets vsModuleOrigin
      key <- requiredBinderKey (Syn.patSynDeclName patSyn)
      patSyns <- gets vsPatSyns
      info <-
        case Map.lookup key patSyns of
          Just info -> pure info
          Nothing -> failValue ("pattern synonym does not have checked information: " <> T.unpack (Syn.unqualifiedNameText (Syn.patSynDeclName patSyn)))
      matcher <- desugarPatSynHelper moduleOrigin "$m" info [tcPatSynMatcher annotation]
      builder <- traverse (desugarPatSynHelper moduleOrigin "$b" info) (tcPatSynBuilder annotation)
      selectors <- mapM (desugarPatSynSelector moduleOrigin) (tcPatSynSelectors annotation)
      pure (matcher : maybeToList builder <> selectors)
    Nothing -> pure []

patSynDeclAnnotation :: Syn.Decl -> Maybe (TcPatSynAnnotation, Syn.PatSynDecl)
patSynDeclAnnotation = go Nothing
  where
    go found declaration =
      case declaration of
        Syn.DeclAnn annotation inner -> go ((Syn.fromAnnotation annotation :: Maybe TcPatSynAnnotation) <|> found) inner
        Syn.DeclPatSyn patSyn -> (,patSyn) <$> found
        _ -> Nothing

-- | The matcher or the builder of a pattern synonym as an ordinary
-- top-level function. The type checker registers its type.
desugarPatSynHelper :: (PackageId, Text) -> Text -> PatSynInfo -> [Syn.Match] -> ValueM Decl
desugarPatSynHelper moduleOrigin prefix info matches = do
  helperType <- lookupBindingType (patSynHelperKey moduleOrigin prefix info)
  body <- desugarMatches helperType matches
  ty <- convertCheckedType helperType
  pure (DeclVal (ValDecl Pub (topName moduleOrigin (patSynHelperName prefix info)) ty body))

-- | The field selector of a record pattern synonym as an ordinary
-- top-level function. The type checker checks its equation against the
-- pattern of the synonym and registers its type.
desugarPatSynSelector :: (PackageId, Text) -> (Text, Syn.Match) -> ValueM Decl
desugarPatSynSelector moduleOrigin (label, match) = do
  let (package, moduleName') = moduleOrigin
  selectorType <- lookupBindingType (TcTermGlobal package moduleName' label)
  body <- desugarMatches selectorType [match]
  ty <- convertCheckedType selectorType
  pure (DeclVal (ValDecl Pub (topName moduleOrigin label) ty body))

patSynHelperName :: Text -> PatSynInfo -> Text
patSynHelperName prefix info = prefix <> psiName info

patSynHelperKey :: (PackageId, Text) -> Text -> PatSynInfo -> TcTermKey
patSynHelperKey (package, moduleName') prefix info = TcTermGlobal package moduleName' (patSynHelperName prefix info)

-- | The pattern synonym that a constructor pattern uses, with the checked
-- annotation of the pattern.
patternPatSyn :: Syn.Pattern -> ValueM (Maybe (PatSynInfo, TcAnnotation))
patternPatSyn pattern' = do
  patSyns <- gets vsPatSyns
  pure $ do
    name <- patternConstructorSourceName pattern'
    key <- nameTermKey name
    info <- Map.lookup key patSyns
    annotation <- patternAnnotation pattern'
    pure (info, annotation)

patternAnnotation :: Syn.Pattern -> Maybe TcAnnotation
patternAnnotation pattern' =
  case pattern' of
    Syn.PAnn annotation inner -> Syn.fromAnnotation annotation <|> patternAnnotation inner
    Syn.PParen inner -> patternAnnotation inner
    Syn.PStrict inner -> patternAnnotation inner
    Syn.PIrrefutable inner -> patternAnnotation inner
    Syn.PAs _ inner -> patternAnnotation inner
    Syn.PTypeSig inner _ -> patternAnnotation inner
    _ -> Nothing

firstPatternPatSyn :: Syn.Match -> ValueM (Maybe (PatSynInfo, TcAnnotation))
firstPatternPatSyn match =
  case Syn.matchPats match of
    pattern' : _ -> patternPatSyn pattern'
    [] -> pure Nothing

-- | The matcher of a pattern synonym applied to the type arguments of one
-- use. The matcher quantifies the pattern synonym variables and the result
-- variable. The use gives the pattern synonym arguments, and the result
-- type gives the result variable.
patSynMatcherReference :: PatSynInfo -> TcAnnotation -> TcType -> ValueM Expr
patSynMatcherReference info annotation resultType = do
  let (package, moduleName') = psiOrigin info
      matcherKey = TcTermGlobal package moduleName' (patSynHelperName "$m" info)
      ForAll patternVariables _ _ = psiScheme info
      typeArguments = tcAnnTypeArgs annotation
  matcherType <- lookupBindingType matcherKey
  unless (length patternVariables == length typeArguments) $
    failValue ("pattern synonym use does not have the type arguments of its declaration: " <> T.unpack (psiName info))
  -- The matcher quantifies the universal variables and then the result
  -- variable. The use gives the universal arguments and then the
  -- existential arguments.
  let (matcherVariables, _) = peelForAlls matcherType
      universalCount = length matcherVariables - 1
  unless (universalCount >= 0 && universalCount <= length typeArguments) $
    failValue ("pattern synonym matcher does not quantify the universal variables and the result: " <> T.unpack (psiName info))
  types <- mapM convertCheckedType (take universalCount typeArguments <> [resultType])
  pure (foldl ExTyApp (ExVar (Name (patSynHelperName "$m" info) SortValue (OriginTop package moduleName'))) types)

-- | The empty case that reports a failed match on one binder.
emptyCaseFailure :: TcType -> Binder -> ValueM Expr
emptyCaseFailure resultType binder = do
  resultType' <- convertCheckedType resultType
  failureBinder <- freshBinderFromType "_case_nomatch" (binderType binder)
  pure (ExCase (ExVar (binderName binder)) failureBinder resultType' [])

-- | Compile a row whose first pattern uses a pattern synonym. The matcher
-- gets the argument, a continuation over the fields, and the failure. The
-- fields become new match columns.
desugarPatSynPattern :: TcType -> Maybe Expr -> Binder -> [Binder] -> [TcType] -> MatchWork -> [MatchWork] -> PatSynInfo -> TcAnnotation -> ValueM Expr
desugarPatSynPattern resultType fallback argument arguments argumentTypes (match, locals) rest info annotation = do
  (ty, restTypes) <- requiredArgumentTypes argumentTypes
  pattern' <-
    case Syn.matchPats match of
      pattern' : _ -> pure pattern'
      [] -> failValue "pattern synonym row has no pattern"
  failure <-
    if null rest
      then maybe (emptyCaseFailure resultType argument) pure fallback
      else desugarMatchArguments resultType fallback (argument : arguments) argumentTypes rest
  shareFailure resultType (Just failure) $ \shared -> do
    failureExpression <- maybe (emptyCaseFailure resultType argument) pure shared
    extra <- patternMatchBindings pattern' argument ty
    let match' = match {Syn.matchPats = patternChildren pattern' <> drop 1 (Syn.matchPats match)}
    desugarPatSynCall info annotation resultType argument pattern' failureExpression $ \fields fieldTypes ->
      desugarMatchArguments resultType (Just failureExpression) (fields <> arguments) (fieldTypes <> restTypes) [(match', locals <> extra)]

-- | Match one binder against a pattern synonym pattern with a success
-- continuation.
desugarPatSynWithFailure :: TcType -> Binder -> Syn.Pattern -> PatSynInfo -> TcAnnotation -> ValueM Expr -> Maybe Expr -> ValueM Expr
desugarPatSynWithFailure resultType binder pattern' info annotation success failure = do
  failure' <- maybe (emptyCaseFailure resultType binder) pure failure
  shareFailure resultType (Just failure') $ \shared -> do
    failureExpression <- maybe (emptyCaseFailure resultType binder) pure shared
    desugarPatSynCall info annotation resultType binder pattern' failureExpression $ \fields fieldTypes ->
      desugarDoChildPatterns resultType (zip3 fields fieldTypes (patternChildren pattern')) success (Just failureExpression)

-- | Call the matcher of a pattern synonym on one binder. The matcher gets
-- the type arguments, the required dictionaries, the scrutinee, the
-- continuation, and the failure. The continuation binds the existential
-- type variables, the provided dictionaries, and the fields. The body
-- function gets the field binders and their types.
desugarPatSynCall :: PatSynInfo -> TcAnnotation -> TcType -> Binder -> Syn.Pattern -> Expr -> ([Binder] -> [TcType] -> ValueM Expr) -> ValueM Expr
desugarPatSynCall info annotation resultType scrutinee pattern' failureExpression body = do
  let children = patternChildren pattern'
      (requiredTerms, providedTerms) = splitAt (length (psiReqTheta info)) (tcAnnEvidenceTerms annotation)
      providedPredicates = [predicate | Ev.EvGiven predicate <- providedTerms]
      typeVariables = tcAnnTypeBinders annotation
  typeBinders <- convertTypeBinders typeVariables
  fieldTypes <- mapM requiredPatternType children
  fields <- zipWithM freshPatternBinder children fieldTypes
  dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] providedPredicates
  requiredArguments <- mapM desugarEvidence requiredTerms
  matcher <- patSynMatcherReference info annotation resultType
  body' <- withAlternativeScope (not (null typeBinders)) (zipWith Dictionary providedPredicates dictionaries) (body fields fieldTypes)
  let continuation = foldr ExTyLam (foldr ExLam (foldr ExLam body' fields) dictionaries) typeBinders
  pure (ExApp (ExApp (ExApp (foldl ExApp matcher requiredArguments) (ExVar (binderName scrutinee))) continuation) failureExpression)

desugarEarlyDecl :: Syn.Decl -> ValueM [Decl]
desugarEarlyDecl declaration =
  case annotatedForeignDecl declaration of
    Just (_, foreignPlan, foreignDecl) -> desugarForeign foreignPlan foreignDecl
    Nothing ->
      case declaration of
        Syn.DeclAnn annotation inner
          | Just classAnnotation <- Syn.fromAnnotation annotation,
            Syn.DeclClass classDecl <- Syn.peelDeclAnn inner ->
              (<>)
                <$> desugarClassSelectors classDecl classAnnotation
                <*> desugarClassDefaults classDecl
          | otherwise -> desugarEarlyDecl inner
        Syn.DeclData dataDecl -> desugarRecordSelectors (Syn.dataDeclConstructors dataDecl)
        Syn.DeclNewtype newtypeDecl -> desugarRecordSelectors (maybeToList (Syn.newtypeDeclConstructor newtypeDecl))
        _ -> pure []

-- | Make one selector function for each record label of a data or newtype
-- declaration. The type checker registers the selector type. A data
-- selector matches each constructor that has the label. A newtype selector
-- casts with the representation axiom.
desugarRecordSelectors :: [Syn.DataConDecl] -> ValueM [Decl]
desugarRecordSelectors declarations = do
  moduleOrigin <- gets vsModuleOrigin
  infos <- gets vsConstructorInfos
  let constructors =
        [ constructor
        | name <- concatMap recordConstructorNames declarations,
          constructor <- Map.findWithDefault [] name infos,
          dciOrigin constructor == moduleOrigin
        ]
      labels =
        List.nub
          [ label
          | constructor <- constructors,
            field <- dciFields constructor,
            Just label <- [dcfiLabel field]
          ]
  bindingTypes <- gets vsBindingTypes
  -- A field whose type mentions an existential variable has no selector,
  -- so the type checker gives it no binding type.
  let (package, moduleName') = moduleOrigin
      selectable label = Map.member (TcTermGlobal package moduleName' label) bindingTypes
  mapM (desugarRecordSelector constructors) (filter selectable labels)

recordConstructorNames :: Syn.DataConDecl -> [Text]
recordConstructorNames declaration =
  case declaration of
    Syn.DataConAnn _ inner -> recordConstructorNames inner
    Syn.RecordCon _ _ constructor _ -> [Syn.unqualifiedNameText constructor]
    Syn.GadtCon _ _ constructors (Syn.GadtRecordBody _ _) -> map Syn.unqualifiedNameText constructors
    _ -> []

desugarRecordSelector :: [DataConInfo] -> Text -> ValueM Decl
desugarRecordSelector constructors label = do
  moduleOrigin <- gets vsModuleOrigin
  let (package, moduleName') = moduleOrigin
  bindingTypes <- gets vsBindingTypes
  selectorType <-
    case Map.lookup (TcTermGlobal package moduleName' label) bindingTypes of
      Just ty -> pure ty
      Nothing -> failValue ("record selector does not have a checked type: " <> T.unpack label)
  let (typeVariables, afterForAlls) = peelForAlls selectorType
      (predicates, body) = peelConstraints afterForAlls
  (scrutineeType, fieldType) <-
    case body of
      TcFunTy argument result -> pure (argument, result)
      _ -> failValue ("record selector does not have a function type: " <> T.unpack label)
  withTypeVariables typeVariables $ do
    dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
    argument <- freshBinder "$record" scrutineeType
    selection <- desugarRecordSelection label scrutineeType fieldType argument constructors
    typeBinders <- convertTypeBinders typeVariables
    selectorType' <- convertCheckedType selectorType
    pure
      ( DeclVal
          ValDecl
            { valVis = Pub,
              valName = topName moduleOrigin label,
              valType = selectorType',
              valBody = foldr ExTyLam (foldr ExLam selection (dictionaries <> [argument])) typeBinders
            }
      )

desugarRecordSelection :: Text -> TcType -> TcType -> Binder -> [DataConInfo] -> ValueM Expr
desugarRecordSelection label scrutineeType fieldType argument constructors = do
  newtypes <- gets vsNewtypeConstructors
  let newtypeInfos =
        [ dataType
        | constructor <- constructors,
          let (package, moduleName') = dciOrigin constructor,
          Just dataType <- [Map.lookup (TcTermGlobal package moduleName' (dciName constructor)) newtypes]
        ]
  case newtypeInfos of
    dataType : _ -> do
      typeArguments <-
        case scrutineeType of
          TcTyCon _ arguments -> pure arguments
          _ -> failValue ("newtype record selector does not select from a type constructor: " <> T.unpack label)
      convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
      let tyCon = dtiTyCon dataType
          axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      pure (ExCast (ExVar (binderName argument)) (CoAxiom axiom convertedArguments))
    [] -> do
      caseBinder <- freshBinder "$record_scrut" scrutineeType
      fieldType' <- convertCheckedType fieldType
      alternatives <- concat <$> mapM (recordSelectorAlternative label) constructors
      pure (ExCase (ExVar (binderName argument)) caseBinder fieldType' alternatives)

recordSelectorAlternative :: Text -> DataConInfo -> ValueM [Alt]
recordSelectorAlternative label constructor =
  case List.findIndex ((== Just label) . dcfiLabel) (dciFields constructor) of
    Nothing -> pure []
    Just index -> do
      let (package, moduleName') = dciOrigin constructor
          existentials = dciExTyVars constructor
      typeBinders <- convertTypeBinders existentials
      withTypeVariables existentials $ do
        dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] (dciTheta constructor)
        fields <- zipWithM (freshIndexedBinder "$field") [0 :: Int ..] (map dcfiType (dciFields constructor))
        selected <-
          case drop index fields of
            field : _ -> pure field
            [] -> failValue ("record selector field index is out of range: " <> T.unpack label)
        let constructorName = Name (dciName constructor) SortDataConstructor (OriginTop package moduleName')
        pure [Alt (AltData constructorName) typeBinders (dictionaries <> fields) (ExVar (binderName selected))]

annotatedForeignDecl :: Syn.Decl -> Maybe (TcAnnotation, Maybe TcForeignImportAnnotation, Syn.ForeignDecl)
annotatedForeignDecl = go Nothing Nothing
  where
    go maybeType maybePlan declaration =
      case declaration of
        Syn.DeclAnn annotation inner ->
          go
            ((Syn.fromAnnotation annotation :: Maybe TcAnnotation) <|> maybeType)
            ((Syn.fromAnnotation annotation :: Maybe TcForeignImportAnnotation) <|> maybePlan)
            inner
        Syn.DeclForeign foreignDecl -> (,,foreignDecl) <$> maybeType <*> pure maybePlan
        _ -> Nothing

-- | A foreign import declares no System FC value. Each use of the import
-- desugars to a foreign call, so this only validates the declaration.
desugarForeign :: Maybe TcForeignImportAnnotation -> Syn.ForeignDecl -> ValueM [Decl]
desugarForeign foreignPlan foreignDecl =
  case Syn.foreignCallConv foreignDecl of
    Syn.CPrim -> do
      _ <- validatePrimitiveSeqOrigin foreignDecl
      pure []
    Syn.CCall -> do
      unless (Syn.foreignDirection foreignDecl == Syn.ForeignImport) (failValue "System FC does not accept foreign exports")
      unless (isJust foreignPlan) (failValue "missing checked foreign import plan")
      _ <- convertForeignSafety (Syn.foreignSafety foreignDecl)
      pure []
    callConv -> failValue ("unsupported System FC foreign calling convention: " <> show callConv)

-- | The System FC facts of a foreign import: its calling convention and the
-- axioms and constructors that its marshalling needs.
foreignCallFacts :: TcType -> TcForeignImportInfo -> ValueM (CallingConvention, [ForeignImportDependency])
foreignCallFacts ty info =
  case info of
    TcForeignPrimImport -> pure (Prim, [])
    TcForeignCCallImport safety plan -> do
      convertedSafety <- convertForeignSafetyMark safety
      dependencies <- foreignImportPlanDependencies ty plan
      let convention =
            CCall
              CCallSpec
                { ccallSymbol = tcForeignSymbol plan,
                  ccallTarget = convertForeignTarget (tcForeignTarget plan),
                  ccallSafety = convertedSafety,
                  ccallArgumentTypes = map (convertCAbiType . tcForeignAbiType) (tcForeignArguments plan),
                  ccallResultType = convertCAbiType (tcForeignAbiType (tcForeignResult plan)),
                  ccallEffect = convertForeignEffect (tcForeignEffect plan)
                }
      pure (convention, dependencies)

-- | Desugar a use of a foreign import. The use becomes a saturated foreign
-- call under one lambda for each argument, so a partial use is a function
-- and a full application reduces to the call.
desugarForeignReference :: Name -> TcTermKey -> TcForeignImportInfo -> [Type] -> [Expr] -> ValueM Expr
desugarForeignReference variable key info types evidence = do
  unless (null evidence) (failValue ("foreign import " <> T.unpack (nameText variable) <> " has unexpected evidence arguments"))
  ty <- lookupBindingType key
  foreignType <- convertCheckedType ty
  (convention, dependencies) <- foreignCallFacts ty info
  let call =
        ForeignCall
          { foreignCallName = variable,
            foreignCallConvention = convention,
            foreignCallDependencies = dependencies,
            foreignCallType = foreignType
          }
  instantiated <- instantiateForeignType variable foreignType types
  -- The declared type gives the arity. A type argument can be a function
  -- type, and the call must not take the arguments of that function.
  let arity = length (foreignArgumentTypes (foreignTypeBody foreignType))
  binders <- mapM (freshBinderFromType "_foreign_argument") (take arity (foreignArgumentTypes instantiated))
  pure (foldr ExLam (ExForeignCall call types (map (ExVar . binderName) binders)) binders)

-- | Substitute the type arguments of a use for the leading binders of the
-- foreign type.
instantiateForeignType :: Name -> Type -> [Type] -> ValueM Type
instantiateForeignType variable = go
  where
    go ty [] = pure ty
    go ty (argument : rest) =
      case ty of
        TyForAll binder body -> go (TypeOf.substType (binderName binder) argument body) rest
        _ -> failValue ("foreign import " <> T.unpack (nameText variable) <> " has too many type arguments")

-- | The type after the leading binders of a foreign type.
foreignTypeBody :: Type -> Type
foreignTypeBody ty =
  case ty of
    TyForAll _ body -> foreignTypeBody body
    _ -> ty

-- | The argument types of an instantiated foreign type, one for each arrow.
foreignArgumentTypes :: Type -> [Type]
foreignArgumentTypes ty =
  case ty of
    TyFun _ _ argument result -> argument : foreignArgumentTypes result
    _ -> []

convertForeignSafetyMark :: TcForeignSafety -> ValueM ForeignSafety
convertForeignSafetyMark safety =
  case safety of
    TcForeignUnsafe -> pure ForeignUnsafe
    TcForeignSafe -> pure ForeignSafe
    TcForeignInterruptible -> failValue "System FC does not accept interruptible foreign imports"

validatePrimitiveSeqOrigin :: Syn.ForeignDecl -> ValueM Bool
validatePrimitiveSeqOrigin foreignDecl =
  if Syn.unqualifiedNameText (Syn.foreignName foreignDecl) == "seq"
    then do
      moduleOrigin <- gets vsModuleOrigin
      primitivePackage <- gets (cePrimPackage . vsConvertEnv)
      unless (moduleOrigin == (primitivePackage, "GHC.Prim")) $
        failValue "System FC accepts a foreign primitive named seq only in the configured GHC.Prim module"
      pure True
    else pure False

foreignImportPlanDependencies :: TcType -> TcForeignImportAnnotation -> ValueM [ForeignImportDependency]
foreignImportPlanDependencies ty plan = do
  typeDependencies <- foreignTypeNewtypeDependencies ty
  marshalDependencies <- concat <$> mapM foreignMarshalDependencies (tcForeignArguments plan <> [tcForeignResult plan])
  pure (List.nub (typeDependencies <> marshalDependencies))

foreignTypeNewtypeDependencies :: TcType -> ValueM [ForeignImportDependency]
foreignTypeNewtypeDependencies ty = do
  newtypes <- List.nub . Map.elems <$> gets vsNewtypeConstructors
  pure (go newtypes ty)
  where
    go newtypes current =
      case current of
        TcTyVar {} -> []
        TcMetaTv {} -> []
        TcTyCon tyCon arguments ->
          [foreignNewtypeDependency dataType | dataType <- newtypes, dtiTyCon dataType == tyCon]
            <> concatMap (go newtypes) arguments
        TcFunTy argument result -> go newtypes argument <> go newtypes result
        TcForAllTy _ body -> go newtypes body
        TcQualTy _ body -> go newtypes body
        TcAppTy function argument -> go newtypes function <> go newtypes argument

-- | The constructors a foreign value marshals through, outermost first.  A
-- unary constructor continues to its field type; the nullary constructor of
-- a unit result ends the chain.
foreignMarshalDependencies :: TcForeignMarshal -> ValueM [ForeignImportDependency]
foreignMarshalDependencies marshal = go (tcForeignSourceType marshal) (tcForeignConstructors marshal)
  where
    go _ [] = pure []
    go sourceType (constructorName : rest) = do
      newtypes <- List.nub . Map.elems <$> gets vsNewtypeConstructors
      constructors <- Map.findWithDefault [] constructorName <$> gets vsConstructorInfos
      case [(dataType, constructor, fieldType) | dataType <- newtypes, constructor <- dtiConstructors dataType, dciName constructor == constructorName, Just fieldType <- [foreignConstructorField sourceType constructor]] of
        [(dataType, _, fieldType)] ->
          (foreignNewtypeDependency dataType :) <$> continue constructorName fieldType rest
        [] ->
          case [(constructor, fieldType) | constructor <- constructors, Just fieldType <- [foreignConstructorField sourceType constructor]] of
            [(constructor, fieldType)] ->
              let (package, moduleName) = dciOrigin constructor
                  dependency = ForeignConstructor (Name constructorName SortDataConstructor (OriginTop package moduleName))
               in (dependency :) <$> continue constructorName fieldType rest
            [] -> failValue ("missing checked foreign constructor " <> T.unpack constructorName)
            _ -> failValue ("ambiguous checked foreign constructor " <> T.unpack constructorName)
        _ -> failValue ("ambiguous checked foreign newtype constructor " <> T.unpack constructorName)
    continue _ (Just fieldType) rest = go fieldType rest
    continue _ Nothing [] = pure []
    continue constructorName Nothing _ = failValue ("checked foreign constructor " <> T.unpack constructorName <> " has no field to marshal through")

-- | The field type of a unary constructor at the source type, or 'Nothing'
-- inside for a nullary constructor.
foreignConstructorField :: TcType -> DataConInfo -> Maybe (Maybe TcType)
foreignConstructorField sourceType constructor = do
  substitution <- matchTypes [dciResTy constructor] [sourceType]
  case dciFields constructor of
    [field] -> pure (Just (applySubst substitution (dcfiType field)))
    [] -> pure Nothing
    _ -> Nothing

foreignNewtypeDependency :: DataTypeInfo -> ForeignImportDependency
foreignNewtypeDependency dataType =
  let tyCon = dtiTyCon dataType
   in ForeignAxiom (Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)))

-- | Whether the import calls the C symbol or takes its address
-- (@foreign import ccall "&sym"@).  The type checker reads the entity string
-- and gives this fact in the checked plan.
convertForeignTarget :: TcForeignTarget -> CCallTarget
convertForeignTarget target =
  case target of
    TcForeignCall -> CCallFunction
    TcForeignAddress -> CCallAddress

convertCAbiType :: TcForeignAbiType -> CAbiType
convertCAbiType abiType =
  case abiType of
    TcForeignInt -> CAbiInt
    TcForeignInt8 -> CAbiInt8
    TcForeignInt16 -> CAbiInt16
    TcForeignInt32 -> CAbiInt32
    TcForeignInt64 -> CAbiInt64
    TcForeignWord -> CAbiWord
    TcForeignWord8 -> CAbiWord8
    TcForeignWord16 -> CAbiWord16
    TcForeignWord32 -> CAbiWord32
    TcForeignWord64 -> CAbiWord64
    TcForeignFloat -> CAbiFloat
    TcForeignDouble -> CAbiDouble
    TcForeignAddr -> CAbiAddr
    TcForeignVoid -> CAbiVoid

-- | An omitted safety mark means @safe@, as in the Haskell report. The runtime
-- is single-threaded, so both marks lower to the same call. An @interruptible@
-- call needs asynchronous interruption, which the runtime does not have.
convertForeignSafety :: Maybe Syn.ForeignSafety -> ValueM ForeignSafety
convertForeignSafety safety =
  case safety of
    Just Syn.Unsafe -> pure ForeignUnsafe
    Just Syn.Safe -> pure ForeignSafe
    Nothing -> pure ForeignSafe
    Just Syn.Interruptible -> failValue "System FC does not accept interruptible foreign imports"

convertForeignEffect :: TcForeignEffect -> ForeignEffect
convertForeignEffect effect =
  case effect of
    TcForeignPure -> ForeignPure
    TcForeignRealWorld -> ForeignRealWorld

desugarClassSelectors :: Syn.ClassDecl -> TcClassAnnotation -> ValueM [Decl]
desugarClassSelectors classDecl classAnnotation = do
  let classTyVars = tcClassKindTyVars classAnnotation <> tcClassTyVars classAnnotation
      className = Syn.unqualifiedNameText (Syn.binderHeadName (Syn.classDeclHead classDecl))
  methodTypes <- mapM (methodFieldType className classTyVars) (tcClassMethods classAnnotation)
  let fieldTypes = map tcDictBinderType (tcClassSuperClasses classAnnotation) <> methodTypes
      superClassCount = length (tcClassSuperClasses classAnnotation)
  mapM (desugarSelector (tcClassTyCon classAnnotation) classTyVars fieldTypes superClassCount) (tcClassMethods classAnnotation)

desugarClassDefaults :: Syn.ClassDecl -> ValueM [Decl]
desugarClassDefaults classDecl =
  concat <$> mapM (defaultItem Nothing) (Syn.classDeclItems classDecl)
  where
    defaultItem maybeAnnotation item =
      case item of
        Syn.ClassItemAnn annotation inner ->
          defaultItem
            ((Syn.fromAnnotation annotation :: Maybe TcInstanceMethodAnnotation) <|> maybeAnnotation)
            inner
        Syn.ClassItemDefault valueDecl ->
          case maybeAnnotation of
            Just annotation -> (: []) <$> desugarDefaultWorker annotation valueDecl
            Nothing -> failValue "class default method does not have a checked annotation"
        _ -> pure []

desugarDefaultWorker :: TcInstanceMethodAnnotation -> Syn.ValueDecl -> ValueM Decl
desugarDefaultWorker annotation valueDecl = do
  let workerType = tcInstanceMethodType annotation
      methodName = tcInstanceMethodName annotation
      matches =
        case valueDecl of
          Syn.FunctionBind _ sourceMatches -> sourceMatches
          Syn.PatternBind _ _ rhs -> [emptyMatch rhs]
  body <- desugarMatches workerType matches
  convertedType <- convertCheckedType workerType
  moduleOrigin <- gets vsModuleOrigin
  pure
    ( DeclVal
        ValDecl
          { valVis = Pub,
            valName = topName moduleOrigin (defaultMethodName methodName),
            valType = convertedType,
            valBody = body
          }
    )

desugarSelector :: TyCon -> [TyVarId] -> [TcType] -> Int -> TcClassMethodAnnotation -> ValueM Decl
desugarSelector classTyCon classTyVars fieldTypes superClassCount method = do
  _ <- freshUnique
  let (typeVariables, afterForAlls) = peelForAlls (tcClassMethodType method)
      (predicates, resultType) = peelConstraints afterForAlls
  withTypeVariables typeVariables $ do
    dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
    classDictionary <-
      case dictionaries of
        dictionary : _ -> pure dictionary
        [] -> freshBinder "$d" (tcClassMethodDictType method)
    caseBinder <- freshBinder "$dict" (tcClassMethodDictType method)
    fields <- zipWithM (freshIndexedBinder "$method") [0 :: Int ..] fieldTypes
    selected <-
      case drop (superClassCount + tcClassMethodIndex method) fields of
        field : _ -> pure field
        [] -> failValue ("invalid class method index for " <> T.unpack (tcClassMethodName method))
    extraTypes <- mapM (convertCheckedType . TcTyVar) (filter (`notElem` classTyVars) (tcClassMethodTyVars method))
    resultType' <- convertCheckedType resultType
    let extraDictionaries = drop 1 dictionaries
        selectedExpr =
          foldl
            ExApp
            (foldl ExTyApp (ExVar (binderName selected)) extraTypes)
            (map (ExVar . binderName) extraDictionaries)
        selection =
          ExCase
            (ExVar (binderName classDictionary))
            caseBinder
            resultType'
            [Alt (AltData (classDictConName classTyCon)) [] fields selectedExpr]
    typeBinders <- convertTypeBinders typeVariables
    methodType' <- convertCheckedType (tcClassMethodType method)
    moduleOrigin <- gets vsModuleOrigin
    pure
      ( DeclVal
          ValDecl
            { valVis = Pub,
              valName = topName moduleOrigin (tcClassMethodName method),
              valType = methodType',
              valBody = foldr ExTyLam (foldr ExLam selection dictionaries) typeBinders
            }
      )

methodFieldType :: Text -> [TyVarId] -> TcClassMethodAnnotation -> ValueM TcType
methodFieldType className classTyVars method = do
  remaining <-
    case removeClassPredicate predicates of
      Just result -> pure result
      Nothing -> failValue ("class method lacks its class predicate for " <> T.unpack className)
  let extraVariables = filter (`notElem` classTyVars) methodVariables
      qualifiedBody = if null remaining then body else TcQualTy remaining body
  pure (foldr TcForAllTy qualifiedBody extraVariables)
  where
    (methodVariables, afterForAlls) = peelForAlls (tcClassMethodType method)
    (predicates, body) = peelConstraints afterForAlls
    removeClassPredicate [] = Nothing
    removeClassPredicate (predicate : rest) =
      case predicate of
        ClassPred predicateClass _
          | tyConName predicateClass == className -> Just rest
        _ -> (predicate :) <$> removeClassPredicate rest

desugarInstanceDecl :: Syn.Decl -> ValueM [Decl]
desugarInstanceDecl declaration =
  case declaration of
    Syn.DeclAnn annotation inner
      | Just instanceAnnotation <- Syn.fromAnnotation annotation,
        Syn.DeclInstance instanceDecl <- Syn.peelDeclAnn inner ->
          (: []) <$> desugarInstance instanceAnnotation instanceDecl
      | otherwise -> desugarInstanceDecl inner
    Syn.DeclInstance {} -> failValue "missing type-checker annotation for instance declaration"
    _ -> pure []

desugarInstance :: TcInstanceAnnotation -> Syn.InstanceDecl -> ValueM Decl
desugarInstance annotation instanceDecl = withTypeVariables (tcInstanceTyVars annotation) $ do
  let methods = Map.fromListWith appendMatches (instanceMethods instanceDecl)
  contextDictionaries <- zipWithM makeContextDictionary [0 :: Int ..] (tcInstanceContextDicts annotation)
  fields <- withDictionaries contextDictionaries $ do
    superClasses <- mapM (desugarEvidence . snd) (tcInstanceSuperClasses annotation)
    methodFields <- mapM (desugarInstanceMethod annotation contextDictionaries methods) (tcInstanceMethodOrder annotation)
    pure (superClasses <> methodFields)
  _ <- freshUnique
  _ <- freshUnique
  typeBinders <- convertTypeBinders (tcInstanceTyVars annotation)
  headTypes <- convertTyConApplicationArguments (tcInstanceClassTyCon annotation) (tcInstanceHeadTypes annotation)
  dictionaryType <- convertCheckedType (tcInstanceDictType annotation)
  let dictionaryBinders = map dictionaryBinder contextDictionaries
      constructor = foldl ExTyApp (ExVar (classDictConName (tcInstanceClassTyCon annotation))) headTypes
      body = foldr ExTyLam (foldr ExLam (foldl ExApp constructor fields) dictionaryBinders) typeBinders
  moduleOrigin <- gets vsModuleOrigin
  pure
    ( DeclVal
        ValDecl
          { valVis = Pub,
            valName = topName moduleOrigin (tcInstanceDictName annotation),
            valType = dictionaryType,
            valBody = body
          }
    )
  where
    appendMatches (newType, newMatches) (_, oldMatches) = (newType, oldMatches <> newMatches)

desugarInstanceMethod :: TcInstanceAnnotation -> [Dictionary] -> Map Text (TcType, [Syn.Match]) -> Text -> ValueM Expr
desugarInstanceMethod annotation dictionaries methods methodName =
  case Map.lookup methodName methods of
    Just (methodType, matches) -> withDictionaries dictionaries (desugarMatches methodType matches)
    Nothing
      | methodName `elem` tcInstanceDefaultMethods annotation -> desugarDefaultMethod annotation dictionaries methodName
      | otherwise -> failValue ("missing method " <> T.unpack methodName <> " in instance dictionary")

desugarDefaultMethod :: TcInstanceAnnotation -> [Dictionary] -> Text -> ValueM Expr
desugarDefaultMethod annotation dictionaries methodName = do
  method <-
    case [candidate | candidate <- tcInstanceClassMethods annotation, tcClassMethodName candidate == methodName] of
      candidate : _ -> pure candidate
      [] -> failValue ("missing checked class method layout for " <> T.unpack methodName)
  convertedHeadTypes <- convertTyConApplicationArguments (tcInstanceClassTyCon annotation) (tcInstanceHeadTypes annotation)
  convertedInstanceTypes <- mapM (convertCheckedType . TcTyVar) (tcInstanceTyVars annotation)
  let classTyVars = tcInstanceClassTyVars annotation
      extraTyVars = filter (`notElem` classTyVars) (tcClassMethodTyVars method)
      substitution = Map.fromList [(tvUnique tyVar, ty) | (tyVar, ty) <- zip classTyVars (tcInstanceHeadTypes annotation)]
      (_, methodAfterForAlls) = peelForAlls (tcClassMethodType method)
      (methodPredicates, _) = peelConstraints methodAfterForAlls
      extraPredicates = map (applySubstPred substitution) (dropClassPredicate (tcInstanceClassTyCon annotation) methodPredicates)
  extraTypeBinders <- convertTypeBinders extraTyVars
  convertedExtraTypes <- mapM (convertCheckedType . TcTyVar) extraTyVars
  extraDictionaries <- zipWithM (freshDictionaryBinder "$method_d") [0 :: Int ..] extraPredicates
  let workerOrigin =
        case tcInstanceClassOrigin annotation of
          Just (packageName, moduleName') -> OriginTop (PackageId packageName) moduleName'
          Nothing -> OriginLocal (Unique 0)
      worker = foldl ExTyApp (ExVar (Name (defaultMethodName methodName) SortValue workerOrigin)) (convertedHeadTypes <> convertedExtraTypes)
      dictionaryArguments = map (ExVar . binderName . dictionaryBinder) dictionaries
  moduleOrigin <- gets vsModuleOrigin
  let selfName = topName moduleOrigin (tcInstanceDictName annotation)
      self = foldl ExApp (foldl ExTyApp (ExVar selfName) convertedInstanceTypes) dictionaryArguments
      body = foldl ExApp (ExApp worker self) (map (ExVar . binderName) extraDictionaries)
  pure (foldr ExTyLam (foldr ExLam body extraDictionaries) extraTypeBinders)

dropClassPredicate :: TyCon -> [Pred] -> [Pred]
dropClassPredicate classTyCon predicates =
  case predicates of
    [] -> []
    ClassPred predicateClass _ : rest
      | predicateClass == classTyCon -> rest
    predicate : rest -> predicate : dropClassPredicate classTyCon rest

makeContextDictionary :: Int -> TcDictBinderAnnotation -> ValueM Dictionary
makeContextDictionary index annotation = do
  binder <- freshBinder ("$d" <> T.pack (show index)) (tcDictBinderType annotation)
  case constraintTypeToPred (tcDictBinderType annotation) of
    Just predicate -> pure (Dictionary predicate binder)
    Nothing -> failValue ("invalid checked class dictionary type: " <> show (tcDictBinderType annotation))

instanceMethods :: Syn.InstanceDecl -> [(Text, (TcType, [Syn.Match]))]
instanceMethods instanceDecl = concatMap itemMethods (Syn.instanceDeclItems instanceDecl)
  where
    itemMethods item =
      case item of
        Syn.InstanceItemAnn annotation inner
          | Just methodAnnotation <- Syn.fromAnnotation annotation -> methodItem methodAnnotation inner
          | otherwise -> itemMethods inner
        _ -> []
    methodItem methodAnnotation item =
      case item of
        Syn.InstanceItemAnn annotation inner ->
          methodItem
            (fromMaybe methodAnnotation (Syn.fromAnnotation annotation))
            inner
        Syn.InstanceItemBind (Syn.FunctionBind _ matches) ->
          [(tcInstanceMethodName methodAnnotation, (tcInstanceMethodType methodAnnotation, matches))]
        Syn.InstanceItemBind (Syn.PatternBind _ _ rhs) ->
          [(tcInstanceMethodName methodAnnotation, (tcInstanceMethodType methodAnnotation, [emptyMatch rhs]))]
        _ -> []

groupValues :: [Syn.Decl] -> ValueM ([ValueGroup], [TopPatternGroup])
groupValues [] = pure ([], [])
groupValues (declaration : rest) =
  case functionBinding declaration of
    Just (Just key, name, matches, Just checkedType) ->
      let (same, remaining) = span (sameFunction key) rest
          moreMatches = concatMap (maybe [] functionMatches . functionBinding) same
       in Bifunctor.first (FunctionGroup key name (matches <> moreMatches) checkedType :) <$> groupValues remaining
    Just (Nothing, name, _, _) -> failValue ("function " <> T.unpack name <> " does not have a resolved binder")
    Just (_, name, _, Nothing) -> failValue ("function " <> T.unpack name <> " does not have a checked type annotation")
    Nothing ->
      case patternBinding declaration of
        Just (Just key, name, rhs, Just checkedType) -> Bifunctor.first (PatternGroup key name rhs checkedType :) <$> groupValues rest
        Just (Nothing, name, _, _) -> failValue ("pattern binding " <> T.unpack name <> " does not have a resolved binder")
        Just (_, name, _, Nothing) -> failValue ("pattern binding " <> T.unpack name <> " does not have a checked type annotation")
        Nothing ->
          case Syn.peelDeclAnn declaration of
            Syn.DeclValue (Syn.PatternBind _ pattern' rhs)
              | Just checkedType <- declarationType declaration ->
                  Bifunctor.second (TopPatternGroup pattern' rhs checkedType :) <$> groupValues rest
              | otherwise -> failValue "top-level pattern binding does not have a checked type annotation"
            _ -> groupValues rest

groupLocalValues :: [Syn.Decl] -> ValueM [LocalValueGroup]
groupLocalValues [] = pure []
groupLocalValues (declaration : rest) =
  case functionBinding declaration of
    Just (Just key, name, matches, Just checkedType) -> do
      let (same, remaining) = span (sameFunction key) rest
          moreMatches = concatMap (maybe [] functionMatches . functionBinding) same
      (LocalNamedGroup (FunctionGroup key name (matches <> moreMatches) checkedType) :) <$> groupLocalValues remaining
    Just (Nothing, name, _, _) -> failValue ("function " <> T.unpack name <> " does not have a resolved binder")
    Just (_, name, _, Nothing) -> failValue ("function " <> T.unpack name <> " does not have a checked type annotation")
    Nothing ->
      case patternBinding declaration of
        Just (Just key, name, rhs, Just checkedType) ->
          (LocalNamedGroup (PatternGroup key name rhs checkedType) :) <$> groupLocalValues rest
        Just (Nothing, name, _, _) -> failValue ("pattern binding " <> T.unpack name <> " does not have a resolved binder")
        Just (_, name, _, Nothing) -> failValue ("pattern binding " <> T.unpack name <> " does not have a checked type annotation")
        Nothing ->
          case Syn.peelDeclAnn declaration of
            Syn.DeclValue (Syn.PatternBind _ pattern' rhs) -> do
              checkedType <- requiredPatternType pattern'
              (LocalPatternGroup pattern' rhs checkedType (patternIsStrict pattern') :) <$> groupLocalValues rest
            Syn.DeclImplicitParam name expr whereDecls ->
              case declarationType declaration of
                Just checkedType ->
                  (LocalImplicitParamGroup name (Syn.UnguardedRhs [] expr whereDecls) checkedType :) <$> groupLocalValues rest
                Nothing -> failValue ("implicit parameter binding " <> T.unpack name <> " does not have a checked type annotation")
            _ -> groupLocalValues rest

patternIsStrict :: Syn.Pattern -> Bool
patternIsStrict pattern' =
  case pattern' of
    Syn.PAnn _ inner -> patternIsStrict inner
    Syn.PParen inner -> patternIsStrict inner
    Syn.PStrict _ -> True
    _ -> False

patternBinderSpecs :: Syn.Pattern -> ValueM [(TcTermKey, Text, TcType)]
patternBinderSpecs pattern' =
  case pattern' of
    Syn.PVar name -> do
      key <- requiredBinderKey name
      ty <- requiredPatternType pattern'
      pure [(key, Syn.unqualifiedNameText name, ty)]
    Syn.PAnn _ inner -> patternBinderSpecs inner
    Syn.PParen inner -> patternBinderSpecs inner
    Syn.PAs name inner -> do
      key <- requiredBinderKey name
      ty <- requiredPatternType pattern'
      ((key, Syn.unqualifiedNameText name, ty) :) <$> patternBinderSpecs inner
    Syn.PStrict inner -> patternBinderSpecs inner
    Syn.PIrrefutable inner -> patternBinderSpecs inner
    Syn.PTypeSig inner _ -> patternBinderSpecs inner
    Syn.PCon _ _ children -> concat <$> mapM patternBinderSpecs children
    Syn.PInfix left _ right -> (<>) <$> patternBinderSpecs left <*> patternBinderSpecs right
    Syn.PList children -> concat <$> mapM patternBinderSpecs children
    Syn.PTuple _ children -> concat <$> mapM patternBinderSpecs children
    _ -> pure []

functionBinding :: Syn.Decl -> Maybe (Maybe TcTermKey, Text, [Syn.Match], Maybe TcType)
functionBinding declaration =
  case Syn.peelDeclAnn declaration of
    Syn.DeclValue (Syn.FunctionBind name matches) ->
      Just (binderTermKey name, Syn.unqualifiedNameText name, matches, declarationType declaration)
    _ -> Nothing

sameFunction :: TcTermKey -> Syn.Decl -> Bool
sameFunction key declaration = maybe False (\(value, _, _, _) -> value == Just key) (functionBinding declaration)

functionMatches :: (Maybe TcTermKey, Text, [Syn.Match], Maybe TcType) -> [Syn.Match]
functionMatches (_, _, matches, _) = matches

patternBinding :: Syn.Decl -> Maybe (Maybe TcTermKey, Text, Syn.Rhs Syn.Expr, Maybe TcType)
patternBinding declaration =
  case Syn.peelDeclAnn declaration of
    Syn.DeclValue (Syn.PatternBind _ pattern' rhs) -> do
      name <- barePatternBinder pattern'
      Just (binderTermKey name, Syn.unqualifiedNameText name, rhs, declarationType declaration)
    _ -> Nothing

declarationType :: Syn.Decl -> Maybe TcType
declarationType declaration =
  case declaration of
    Syn.DeclAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> declarationType inner
    _ -> Nothing

barePatternName :: Syn.Pattern -> Maybe Text
barePatternName pattern' = Syn.unqualifiedNameText <$> barePatternBinder pattern'

barePatternBinder :: Syn.Pattern -> Maybe Syn.UnqualifiedName
barePatternBinder pattern' =
  case pattern' of
    Syn.PVar name -> Just name
    Syn.PAnn _ inner -> barePatternBinder inner
    Syn.PParen inner -> barePatternBinder inner
    _ -> Nothing

allocateTopValue :: ValueGroup -> ValueM TopValue
allocateTopValue group = do
  _ <- freshUnique
  let name = groupName group
      ty = groupType group
  moduleOrigin <- gets vsModuleOrigin
  pure (TopValue (topName moduleOrigin name) ty group)

groupKey :: ValueGroup -> TcTermKey
groupKey group =
  case group of
    FunctionGroup key _ _ _ -> key
    PatternGroup key _ _ _ -> key

groupName :: ValueGroup -> Text
groupName group =
  case group of
    FunctionGroup _ name _ _ -> name
    PatternGroup _ name _ _ -> name

groupType :: ValueGroup -> TcType
groupType group =
  case group of
    FunctionGroup _ _ _ ty -> ty
    PatternGroup _ _ _ ty -> ty

desugarTopValue :: TopValue -> ValueM ValDecl
desugarTopValue top = do
  body <-
    case topGroup top of
      FunctionGroup _ _ matches _ -> desugarMatches (topType top) matches
      PatternGroup _ _ rhs _ -> desugarMatches (topType top) [emptyMatch rhs]
  ty <- convertCheckedType (topType top)
  pure
    ValDecl
      { valVis = Pub,
        valName = topCoreName top,
        valType = ty,
        valBody = body
      }

-- | A top-level pattern binding becomes one hidden value for the
-- right-hand side and one public value per binder that selects its part.
-- A binder is lazy, as in GHC: the right-hand side is a thunk that the
-- first selection forces.
desugarTopPatternGroup :: TopPatternGroup -> ValueM [ValDecl]
desugarTopPatternGroup (TopPatternGroup pattern' rhs rhsType) = do
  specs <- patternBinderSpecs pattern'
  moduleOrigin <- gets vsModuleOrigin
  let rhsName = topName moduleOrigin ("$pat$" <> T.intercalate "$" [name | (_, name, _) <- specs])
  rhsBody <- desugarMatches rhsType [emptyMatch rhs]
  convertedRhsType <- convertCheckedType rhsType
  selectors <- mapM (selector rhsName) specs
  pure
    ( ValDecl
        { valVis = Pub,
          valName = rhsName,
          valType = convertedRhsType,
          valBody = rhsBody
        }
        : selectors
    )
  where
    selector rhsName (key, name, ty) = do
      moduleOrigin <- gets vsModuleOrigin
      rhsBinder <- freshBinder "_pat_rhs" rhsType
      body <- desugarDoPattern ty rhsBinder rhsType pattern' $ do
        (field, _) <- lookupLocal key name
        pure (ExVar (binderName field))
      convertedType <- convertCheckedType ty
      pure
        ValDecl
          { valVis = Pub,
            valName = topName moduleOrigin name,
            valType = convertedType,
            valBody = ExLet (Bind rhsBinder (ExVar rhsName)) body
          }

emptyMatch :: Syn.Rhs Syn.Expr -> Syn.Match
emptyMatch rhs =
  Syn.Match
    { Syn.matchAnns = [],
      Syn.matchHeadForm = Syn.MatchHeadPrefix,
      Syn.matchPats = [],
      Syn.matchRhs = rhs
    }

desugarMatches :: TcType -> [Syn.Match] -> ValueM Expr
desugarMatches ty matches =
  case matches of
    [] -> failValue "value binding has no match"
    first : _ -> do
      let (typeVariables, afterForAlls) = peelForAlls ty
          (predicates, bodyType) = peelConstraints afterForAlls
          argumentCount = length (Syn.matchPats first)
          (argumentTypes, resultType) = peelFunctions argumentCount bodyType
      typeBinders <- convertTypeBinders typeVariables
      (dictionaries, arguments, body) <-
        withTypeVariables typeVariables $ do
          dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
          arguments <- zipWithM freshArgument [0 :: Int ..] argumentTypes
          body <-
            withDictionaryScope (zipWith Dictionary predicates dictionaries) (desugarMatchArguments resultType Nothing arguments argumentTypes (map emptyMatchWork matches))
          pure (dictionaries, arguments, body)
      pure (foldr ExTyLam (foldr ExLam (foldr ExLam body arguments) dictionaries) typeBinders)

withTypeVariables :: [TyVarId] -> ValueM a -> ValueM a
withTypeVariables variables action = do
  previous <- gets vsConvertEnv
  modify' $ \state -> state {vsConvertEnv = withTyVars variables previous}
  result <- action
  modify' $ \state -> state {vsConvertEnv = previous}
  pure result

-- | Compile match rows against argument binders. The fallback expression,
-- when given, runs when no row matches. Without a fallback, an empty case
-- reports the failure.
desugarMatchArguments :: TcType -> Maybe Expr -> [Binder] -> [TcType] -> [MatchWork] -> ValueM Expr
desugarMatchArguments resultType fallback [] _ ((match, locals) : rest) = do
  failure <- matchFailure resultType fallback [] [] (Syn.matchRhs match) rest
  withLocals locals (desugarRhsWithFailure resultType failure (Syn.matchRhs match))
desugarMatchArguments _ fallback [] _ [] = maybe (failValue "pattern match has no result") pure fallback
desugarMatchArguments resultType fallback binders@(argument : arguments) argumentTypes works
  | any (firstPatternIsOverloadedLiteral . fst) works =
      desugarOverloadedLiteralMatches resultType fallback binders argumentTypes works
  | (first, firstLocals) : rest <- works,
    let firstPatterns = Syn.matchPats first,
    length firstPatterns == length binders,
    all patternIsIrrefutable firstPatterns = do
      extra <- matchArgumentBindings binders argumentTypes first
      failure <- matchFailure resultType fallback binders argumentTypes (Syn.matchRhs first) rest
      withLocals (firstLocals <> extra) (desugarRhsWithFailure resultType failure (Syn.matchRhs first))
  | all (maybe False patternIsIrrefutable . listToMaybe . Syn.matchPats . fst) works = do
      (ty, restTypes) <- requiredArgumentTypes argumentTypes
      nextWorks <- mapM (extendMatchWork argument ty) works
      desugarMatchArguments resultType fallback arguments restTypes (map dropMatchWorkPattern nextWorks)
  | (prefix, viewWork : viewRest) <- break (firstPatternIsView . fst) works =
      if null prefix
        then desugarViewPattern resultType fallback argument arguments argumentTypes viewWork viewRest
        else do
          failure <- desugarMatchArguments resultType fallback binders argumentTypes (viewWork : viewRest)
          shareFailure resultType (Just failure) $ \shared ->
            desugarMatchArguments resultType shared binders argumentTypes prefix
  | otherwise = do
      patSynRows <- mapM (firstPatternPatSyn . fst) works
      case break (isJust . snd) (zip works patSynRows) of
        ([], (synWork, Just (info, annotation)) : synRest) ->
          desugarPatSynPattern resultType fallback argument arguments argumentTypes synWork (map fst synRest) info annotation
        (prefix, _ : _) -> do
          failure <- desugarMatchArguments resultType fallback binders argumentTypes (drop (length prefix) works)
          shareFailure resultType (Just failure) $ \shared ->
            desugarMatchArguments resultType shared binders argumentTypes (map fst prefix)
        _ -> do
          maybeFamily <- firstFamilyPattern (map fst works)
          maybeNewtype <- firstNewtypePattern (map fst works)
          case (maybeFamily, maybeNewtype) of
            (Just (pattern', info), _) -> desugarFamilyPatterns resultType fallback argument arguments argumentTypes works pattern' info
            (_, Just (pattern', dataType)) -> desugarNewtypePatterns resultType fallback argument arguments argumentTypes works pattern' dataType
            _ -> desugarDataPatterns resultType fallback argument arguments argumentTypes works

-- | Compile a row whose first pattern is a view pattern. The view function
-- is applied to the argument and the result is matched against the inner
-- pattern. The later rows are the failure of the inner match.
desugarViewPattern :: TcType -> Maybe Expr -> Binder -> [Binder] -> [TcType] -> MatchWork -> [MatchWork] -> ValueM Expr
desugarViewPattern resultType fallback argument arguments argumentTypes (match, locals) rest = do
  (ty, restTypes) <- requiredArgumentTypes argumentTypes
  (viewPattern, viewFunction, inner) <-
    case Syn.matchPats match of
      pattern' : _
        | Just (function, inner) <- patternView pattern' -> pure (pattern', function, inner)
      _ -> failValue "view pattern row does not start with a view pattern"
  failure <-
    if null rest
      then pure fallback
      else Just <$> desugarMatchArguments resultType fallback (argument : arguments) argumentTypes rest
  function <- desugarExpr viewFunction
  innerType <- requiredPatternType inner
  viewBinder <- freshPatternBinder inner innerType
  extra <- patternMatchBindings viewPattern argument ty
  let match' = match {Syn.matchPats = inner : drop 1 (Syn.matchPats match)}
  body <-
    shareFailure resultType failure $ \shared ->
      desugarMatchArguments resultType shared (viewBinder : arguments) (innerType : restTypes) [(match', locals <> extra)]
  pure (ExLet (Bind viewBinder (ExApp function (ExVar (binderName argument)))) body)

-- | Bind a failure expression once so that each use is a variable.
shareFailure :: TcType -> Maybe Expr -> (Maybe Expr -> ValueM Expr) -> ValueM Expr
shareFailure resultType failure body =
  case failure of
    Nothing -> body Nothing
    Just expression -> shareExpr resultType expression (body . Just)

-- | Bind an expression to a name so that the body can name it more than once.
--
-- The body gets a fresh variable. The binding stays only when the body names
-- it more than once, so a single use keeps the expression where it is and an
-- unused expression disappears. The binding is lazy, so the shared expression
-- does no work until the body needs it.
shareExpr :: TcType -> Expr -> (Expr -> ValueM Expr) -> ValueM Expr
shareExpr resultType expression body =
  case expression of
    ExVar _ -> body expression
    _ -> do
      binder <- freshBinder "_fail" resultType
      let name = binderName binder
      result <- body (ExVar name)
      pure $ case countUses name result of
        0 -> result
        1 -> substituteVar name expression result
        _ -> ExLet (Bind binder expression) result

-- | The number of times an expression names a variable, counted up to two.
countUses :: Name -> Expr -> Int
countUses name = go 0
  where
    go total expression
      | total >= 2 = total
      | otherwise =
          case expression of
            ExVar other -> if other == name then total + 1 else total
            ExLit _ -> total
            ExApp function argument -> go (go total function) argument
            ExTyApp function _ -> go total function
            ExLam _ inner -> go total inner
            ExTyLam _ inner -> go total inner
            ExLet binding inner -> go (go total (bindRhs binding)) inner
            ExRec bindings inner -> go (foldl' go total (map bindRhs bindings)) inner
            ExCase scrutinee _ _ alternatives -> foldl' go (go total scrutinee) (map altRhs alternatives)
            ExCast inner _ -> go total inner
            ExForeignCall _ _ arguments -> foldl' go total arguments

-- | Replace every use of a variable with an expression. The name is fresh, so
-- no binder in the body shadows it.
substituteVar :: Name -> Expr -> Expr -> Expr
substituteVar name value = go
  where
    go expression =
      case expression of
        ExVar other -> if other == name then value else expression
        ExLit _ -> expression
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function ty -> ExTyApp (go function) ty
        ExLam binder inner -> ExLam binder (go inner)
        ExTyLam binder inner -> ExTyLam binder (go inner)
        ExLet binding inner -> ExLet binding {bindRhs = go (bindRhs binding)} (go inner)
        ExRec bindings inner -> ExRec [binding {bindRhs = go (bindRhs binding)} | binding <- bindings] (go inner)
        ExCase scrutinee binder ty alternatives ->
          ExCase (go scrutinee) binder ty [alternative {altRhs = go (altRhs alternative)} | alternative <- alternatives]
        ExCast inner coercion -> ExCast (go inner) coercion
        ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)

patternView :: Syn.Pattern -> Maybe (Syn.Expr, Syn.Pattern)
patternView pattern' =
  case peelPattern pattern' of
    Syn.PView function inner -> Just (function, inner)
    _ -> Nothing

firstPatternIsView :: Syn.Match -> Bool
firstPatternIsView match =
  case Syn.matchPats match of
    pattern' : _ -> isJust (patternView pattern')
    [] -> False

emptyMatchWork :: Syn.Match -> MatchWork
emptyMatchWork match = (match, [])

extendMatchWork :: Binder -> TcType -> MatchWork -> ValueM MatchWork
extendMatchWork binder ty (match, locals) = do
  extra <- firstPatternBindings binder ty match
  pure (match, locals <> extra)

dropMatchWorkPattern :: MatchWork -> MatchWork
dropMatchWorkPattern (match, locals) = (dropFirstPattern match, locals)

desugarOverloadedLiteralMatches :: TcType -> Maybe Expr -> [Binder] -> [TcType] -> [MatchWork] -> ValueM Expr
desugarOverloadedLiteralMatches resultType fallback arguments argumentTypes works =
  case works of
    [] -> maybe (overloadedPatternFailure resultType arguments) pure fallback
    work : rest -> do
      failure <- desugarOverloadedLiteralMatches resultType fallback arguments argumentTypes rest
      -- A row with several literal patterns tests each one in turn, and each
      -- test names the failure. Bind the failure once so that the later rows
      -- are not copied into every test.
      shareExpr resultType failure $ \shared ->
        desugarOverloadedLiteralMatch resultType arguments argumentTypes work shared

desugarOverloadedLiteralMatch :: TcType -> [Binder] -> [TcType] -> MatchWork -> Expr -> ValueM Expr
desugarOverloadedLiteralMatch resultType arguments argumentTypes (match, locals) failure =
  compile locals (zip3 arguments argumentTypes (Syn.matchPats match))
  where
    compile current [] = withLocals current (desugarRhsWithFailure resultType (Just failure) (Syn.matchRhs match))
    compile current ((argument, ty, pattern') : rest)
      | patternIsIrrefutable pattern' = do
          extra <- patternMatchBindings pattern' argument ty
          compile (current <> extra) rest
      | isOverloadedLiteralPattern pattern' = do
          test <- desugarOverloadedLiteralPatternTest (ExVar (binderName argument)) pattern'
          testType <- requiredPatternMethodResultType "==" pattern'
          testBinder <- freshBinder "_case_guard" testType
          resultType' <- convertCheckedType resultType
          trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
          falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
          extra <- patternMatchBindings pattern' argument ty
          success <- compile (current <> extra) rest
          pure
            ( ExCase
                test
                testBinder
                resultType'
                [ Alt (AltData trueName) [] [] success,
                  Alt (AltData falseName) [] [] failure
                ]
            )
      | otherwise =
          let remainingBinders = argument : [binder | (binder, _, _) <- rest]
              remainingTypes = ty : [remainingType | (_, remainingType, _) <- rest]
              remainingMatch = match {Syn.matchPats = pattern' : [remainingPattern | (_, _, remainingPattern) <- rest]}
           in desugarMatchArguments resultType (Just failure) remainingBinders remainingTypes [(remainingMatch, current)]

requiredArgumentTypes :: [TcType] -> ValueM (TcType, [TcType])
requiredArgumentTypes types =
  case types of
    ty : rest -> pure (ty, rest)
    [] -> failValue "pattern match does not have a checked argument type"

firstPatternIsOverloadedLiteral :: Syn.Match -> Bool
firstPatternIsOverloadedLiteral match =
  case Syn.matchPats match of
    pattern' : _ -> isOverloadedLiteralPattern pattern'
    [] -> False

overloadedPatternFailure :: TcType -> [Binder] -> ValueM Expr
overloadedPatternFailure resultType arguments = do
  resultType' <- convertCheckedType resultType
  case arguments of
    argument : _ -> do
      failureBinder <- freshBinderFromType "_case_nomatch" (binderType argument)
      pure (ExCase (ExVar (binderName argument)) failureBinder resultType' [])
    [] -> failValue "overloaded literal match has no argument"

-- | The test of an overloaded literal pattern. The literal converts with
-- fromInteger or fromRational, and the equality method compares it with
-- the scrutinee.
desugarOverloadedLiteralPatternTest :: Expr -> Syn.Pattern -> ValueM Expr
desugarOverloadedLiteralPatternTest scrutinee pattern' = do
  (value, negative) <-
    maybe
      (failValue ("invalid overloaded literal pattern: " <> take 80 (show pattern')))
      pure
      (overloadedPatternValue pattern')
  positive <-
    case value of
      OverloadedInteger integer ->
        ExApp <$> desugarPatternMethod "fromInteger" pattern' <*> desugarIntegerLiteral integer
      OverloadedRational rational ->
        ExApp <$> desugarPatternMethod "fromRational" pattern' <*> desugarRationalLiteral rational
  patternValue <-
    if negative
      then (`ExApp` positive) <$> desugarPatternMethod "negate" pattern'
      else pure positive
  equality <- desugarPatternMethod "==" pattern'
  pure (ExApp (ExApp equality scrutinee) patternValue)

desugarPatternMethod :: Text -> Syn.Pattern -> ValueM Expr
desugarPatternMethod name pattern' = do
  (annotation, resolution) <- requiredPatternOccurrence name pattern'
  desugarResolvedOccurrence annotation resolution

requiredPatternMethodResultType :: Text -> Syn.Pattern -> ValueM TcType
requiredPatternMethodResultType name pattern' = do
  (annotation, _) <- requiredPatternOccurrence name pattern'
  case applicationResultType (tcAnnType annotation) >>= applicationResultType of
    Just result -> pure result
    Nothing -> failValue ("invalid checked pattern method type for " <> T.unpack name)

requiredPatternOccurrence :: Text -> Syn.Pattern -> ValueM (TcAnnotation, ResolutionAnnotation)
requiredPatternOccurrence name pattern' =
  maybe
    (failValue ("missing checked " <> T.unpack name <> " occurrence for overloaded literal pattern"))
    pure
    (patternOccurrence name pattern')

patternOccurrence :: Text -> Syn.Pattern -> Maybe (TcAnnotation, ResolutionAnnotation)
patternOccurrence target = go Nothing
  where
    go currentType pattern' =
      case pattern' of
        Syn.PAnn annotation inner ->
          case (Syn.fromAnnotation annotation :: Maybe TcAnnotation, Syn.fromAnnotation annotation :: Maybe ResolutionAnnotation) of
            (Just checked, _) -> go (Just checked) inner
            (_, Just resolution)
              | resolutionIdentifier resolution == IdentifierNamed target,
                resolutionNamespace resolution == ResolutionNamespaceTerm ->
                  (,resolution) <$> currentType
            _ -> go currentType inner
        Syn.PParen inner -> go currentType inner
        Syn.PStrict inner -> go currentType inner
        Syn.PIrrefutable inner -> go currentType inner
        Syn.PAs _ inner -> go currentType inner
        Syn.PTypeSig inner _ -> go currentType inner
        _ -> Nothing

-- | The value of an overloaded literal pattern.
data OverloadedLiteral
  = OverloadedInteger Integer
  | OverloadedRational Rational

isOverloadedLiteralPattern :: Syn.Pattern -> Bool
isOverloadedLiteralPattern = isJust . overloadedPatternValue

-- | The literal of an overloaded literal pattern, with a flag for a negated literal.
overloadedPatternValue :: Syn.Pattern -> Maybe (OverloadedLiteral, Bool)
overloadedPatternValue pattern' =
  case pattern' of
    Syn.PAnn _ inner -> overloadedPatternValue inner
    Syn.PParen inner -> overloadedPatternValue inner
    Syn.PStrict inner -> overloadedPatternValue inner
    Syn.PIrrefutable inner -> overloadedPatternValue inner
    Syn.PAs _ inner -> overloadedPatternValue inner
    Syn.PTypeSig inner _ -> overloadedPatternValue inner
    Syn.PLit literal -> (,False) <$> overloadedLiteralValue literal
    Syn.PNegLit literal -> (,True) <$> overloadedLiteralValue literal
    _ -> Nothing

overloadedLiteralValue :: Syn.Literal -> Maybe OverloadedLiteral
overloadedLiteralValue literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitInt value Syn.TInteger _ -> Just (OverloadedInteger value)
    Syn.LitFloat value Syn.TFractional _ -> Just (OverloadedRational value)
    _ -> Nothing

desugarDataPatterns :: TcType -> Maybe Expr -> Binder -> [Binder] -> [TcType] -> [MatchWork] -> ValueM Expr
desugarDataPatterns resultType fallback argument arguments argumentTypes works = do
  caseBinder <- freshBinderFromType "_scrut" (binderType argument)
  desugarScrutineePatterns resultType fallback (ExVar (binderName argument)) caseBinder caseBinder arguments argumentTypes works

-- | Match a scrutinee expression against constructor patterns. The root
-- binder gets the variable bindings of the first pattern. It has the checked
-- scrutinee type. The case binder has the type of the scrutinee expression.
desugarScrutineePatterns :: TcType -> Maybe Expr -> Expr -> Binder -> Binder -> [Binder] -> [TcType] -> [MatchWork] -> ValueM Expr
desugarScrutineePatterns resultType fallback scrutinee caseBinder root arguments argumentTypes works = do
  (scrutineeType, restTypes) <- requiredArgumentTypes argumentTypes
  resultType' <- convertCheckedType resultType
  let keys = patternKeys (map fst works)
      defaultWorks = filter (firstPatternIsDefault . fst) works
  -- Every alternative can name the fallback, so bind it once outside the case
  -- instead of copying the later equations into each alternative.
  shareFailure resultType fallback $ \shared -> do
    constructorAlternatives <- mapM (desugarPatternGroup resultType shared arguments restTypes scrutineeType root works) keys
    defaultAlternatives <-
      case defaultWorks of
        [] -> pure [Alt AltDefault [] [] failure | Just failure <- [shared]]
        _ -> do
          updated <- mapM (extendMatchWork root scrutineeType) defaultWorks
          body <- desugarMatchArguments resultType shared arguments restTypes (map dropMatchWorkPattern updated)
          pure [Alt AltDefault [] [] body]
    pure (ExCase scrutinee caseBinder resultType' (constructorAlternatives <> defaultAlternatives))

firstFamilyPattern :: [Syn.Match] -> ValueM (Maybe (Syn.Pattern, DataFamilyInstanceInfo))
firstFamilyPattern matches = do
  families <- gets vsFamilyConstructors
  pure $ do
    pattern' <-
      listToMaybe
        [ candidate
        | match <- matches,
          candidate : _ <- [Syn.matchPats match],
          not (patternIsDefault candidate)
        ]
    name <- patternConstructorSourceName pattern'
    key <- nameTermKey name
    info <- Map.lookup key families
    pure (pattern', info)

-- | A data-family pattern matches the representation type. Cast the
-- scrutinee with the family axiom. A newtype instance also casts with the
-- representation axiom, and then binds the field.
desugarFamilyPatterns :: TcType -> Maybe Expr -> Binder -> [Binder] -> [TcType] -> [MatchWork] -> Syn.Pattern -> DataFamilyInstanceInfo -> ValueM Expr
desugarFamilyPatterns resultType fallback argument remaining argumentTypes works representative info = do
  (scrutineeType, restTypes) <- requiredArgumentTypes argumentTypes
  instanceArguments <- familyInstanceArguments info scrutineeType
  axiomArguments <- mapM convertCheckedType instanceArguments
  let familyCoercion = CoAxiom (familyAxiomName info) axiomArguments
      scrutinee = ExVar (binderName argument)
  if dfiiIsNewtype info
    then do
      child <-
        case patternChildren representative of
          [pattern'] -> pure pattern'
          _ -> failValue ("newtype family pattern does not have one field: " <> T.unpack (dfiiFamilyName info))
      childType <- requiredPatternType child
      field <- freshPatternBinder child childType
      rooted <- mapM (extendMatchWork argument scrutineeType) works
      expanded <- mapMaybeM (specializeMatchWork (patternKey representative) 1 [field] [childType]) rooted
      let unwrapped = ExCast scrutinee (CoTrans familyCoercion (CoAxiom (familyRepresentationAxiomName info) axiomArguments))
      body <- desugarMatchArguments resultType fallback (field : remaining) (childType : restTypes) expanded
      pure (ExLet (Bind field unwrapped) body)
    else do
      representationType <- convertCheckedType (TcTyCon (dfiiRepresentationTyCon info) instanceArguments)
      caseBinder <- freshBinderFromType "_scrut" representationType
      desugarScrutineePatterns resultType fallback (ExCast scrutinee familyCoercion) caseBinder argument remaining argumentTypes works

firstNewtypePattern :: [Syn.Match] -> ValueM (Maybe (Syn.Pattern, DataTypeInfo))
firstNewtypePattern matches = do
  newtypes <- gets vsNewtypeConstructors
  pure $ do
    pattern' <-
      listToMaybe
        [ candidate
        | match <- matches,
          candidate : _ <- [Syn.matchPats match],
          not (patternIsDefault candidate)
        ]
    name <- patternConstructorSourceName pattern'
    key <- nameTermKey name
    dataType <- Map.lookup key newtypes
    pure (pattern', dataType)

patternConstructorSourceName :: Syn.Pattern -> Maybe Syn.Name
patternConstructorSourceName pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> Just name
    Syn.PInfix _ name _ -> Just name
    _ -> Nothing

desugarNewtypePatterns :: TcType -> Maybe Expr -> Binder -> [Binder] -> [TcType] -> [MatchWork] -> Syn.Pattern -> DataTypeInfo -> ValueM Expr
desugarNewtypePatterns resultType fallback argument remaining argumentTypes works representative dataType = do
  (scrutineeType, restTypes) <- requiredArgumentTypes argumentTypes
  child <-
    case patternChildren representative of
      [pattern'] -> pure pattern'
      _ -> failValue ("newtype pattern does not have one field: " <> T.unpack (dtiName dataType))
  childType <- newtypeFieldType dataType scrutineeType child
  field <- freshPatternBinder child childType
  typeArguments <- newtypePatternArguments representative
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  rooted <- mapM (extendMatchWork argument scrutineeType) works
  expanded <- mapMaybeM (specializeMatchWork (patternKey representative) 1 [field] [childType]) rooted
  let tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      unwrapped = ExCast (ExVar (binderName argument)) (CoAxiom axiom convertedArguments)
  body <- desugarMatchArguments resultType fallback (field : remaining) (childType : restTypes) expanded
  pure (ExLet (Bind field unwrapped) body)

mapMaybeM :: (a -> ValueM (Maybe b)) -> [a] -> ValueM [b]
mapMaybeM action values = catMaybes <$> mapM action values

newtypeFieldType :: DataTypeInfo -> TcType -> Syn.Pattern -> ValueM TcType
newtypeFieldType dataType scrutineeType child =
  case dtiConstructors dataType of
    [constructor]
      | Just (Just fieldType) <- foreignConstructorField scrutineeType constructor ->
          pure fieldType
    _ -> requiredPatternType child

newtypePatternArguments :: Syn.Pattern -> ValueM [TcType]
newtypePatternArguments pattern' =
  case constructorResultType (length (patternChildren pattern')) (fromBinderType pattern') of
    TcTyCon _ arguments -> pure arguments
    ty -> failValue ("newtype pattern has an invalid checked type: " <> show ty)

constructorResultType :: Int -> TcType -> TcType
constructorResultType arity ty =
  case ty of
    TcForAllTy _ body -> constructorResultType arity body
    TcQualTy _ body -> constructorResultType arity body
    TcFunTy _ result
      | arity > 0 -> constructorResultType (arity - 1) result
    _ -> ty

desugarPatternGroup :: TcType -> Maybe Expr -> [Binder] -> [TcType] -> TcType -> Binder -> [MatchWork] -> Text -> ValueM Alt
desugarPatternGroup resultType fallback remaining restTypes scrutineeType caseBinder works key = do
  pattern' <-
    case [candidate | (match, _) <- works, candidate : _ <- [Syn.matchPats match], not (patternIsDefault candidate), patternKey candidate == key] of
      candidate : _ -> pure candidate
      [] -> failValue ("missing representative pattern for " <> T.unpack key)
  constructor <- patternConstructor pattern'
  let subpatterns = patternChildren pattern'
      predicates = patternGivenPredicates pattern'
      typeVariables = patternTypeVariables pattern'
  typeBinders <- convertTypeBinders typeVariables
  fieldTypes <- patternFieldTypes pattern' subpatterns
  fields <- zipWithM freshPatternBinder subpatterns fieldTypes
  dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] predicates
  rooted <- mapM (extendMatchWork caseBinder scrutineeType) works
  expanded <- mapMaybeM (specializeMatchWork key (length fields) fields fieldTypes) rooted
  body <-
    withAlternativeScope
      (not (null typeBinders))
      (zipWith Dictionary predicates dictionaries)
      (desugarMatchArguments resultType fallback (fields <> remaining) (fieldTypes <> restTypes) expanded)
  pure (Alt constructor typeBinders (dictionaries <> fields) body)

specializeMatchWork :: Text -> Int -> [Binder] -> [TcType] -> MatchWork -> ValueM (Maybe MatchWork)
specializeMatchWork key arity fields fieldTypes (match, locals) =
  case specializeMatch key arity match of
    Nothing -> pure Nothing
    Just specialized ->
      case Syn.matchPats match of
        pattern' : _
          | not (patternIsDefault pattern'),
            patternKey pattern' == key -> do
              extra <- concat <$> mapM (\(child, field, fieldType) -> patternMatchBindings child field fieldType) (zip3 (patternChildren pattern') fields fieldTypes)
              pure (Just (specialized, locals <> extra))
        _ -> pure (Just (specialized, locals))

patternGivenPredicates :: Syn.Pattern -> [Pred]
patternGivenPredicates = go
  where
    go pattern' =
      case pattern' of
        Syn.PAnn annotation inner -> annotationPredicates annotation <> go inner
        Syn.PParen inner -> go inner
        Syn.PStrict inner -> go inner
        Syn.PIrrefutable inner -> go inner
        Syn.PAs _ inner -> go inner
        Syn.PTypeSig inner _ -> go inner
        Syn.PCon name _ _ -> annotationsPredicates (Syn.nameAnns name)
        Syn.PInfix _ name _ -> annotationsPredicates (Syn.nameAnns name)
        _ -> []
    annotationPredicates annotation =
      maybe [] evidencePredicates (Syn.fromAnnotation annotation :: Maybe TcAnnotation)
    annotationsPredicates annotations =
      concat
        [ evidencePredicates checked
        | annotation <- annotations,
          Just checked <- [Syn.fromAnnotation annotation :: Maybe TcAnnotation]
        ]
    evidencePredicates checked = [predicate | Ev.EvGiven predicate <- tcAnnEvidenceTerms checked]

patternTypeVariables :: Syn.Pattern -> [TyVarId]
patternTypeVariables = go
  where
    go pattern' =
      case pattern' of
        Syn.PAnn annotation inner -> annotationTypeVariables annotation <> go inner
        Syn.PParen inner -> go inner
        Syn.PStrict inner -> go inner
        Syn.PIrrefutable inner -> go inner
        Syn.PAs _ inner -> go inner
        Syn.PTypeSig inner _ -> go inner
        _ -> []
    annotationTypeVariables annotation =
      maybe [] tcAnnTypeBinders (Syn.fromAnnotation annotation :: Maybe TcAnnotation)

patternKeys :: [Syn.Match] -> [Text]
patternKeys matches =
  List.nub
    [ patternKey pattern'
    | match <- matches,
      pattern' : _ <- [Syn.matchPats match],
      not (patternIsDefault pattern')
    ]

patternKey :: Syn.Pattern -> Text
patternKey pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> Syn.nameText name
    Syn.PInfix _ name _ -> Syn.nameText name
    Syn.PList [] -> "[]"
    Syn.PList (_ : _) -> ":"
    Syn.PTuple _ fields -> "(" <> T.replicate (max 0 (length fields - 1)) "," <> ")"
    Syn.PLit literal
      | isBoxedCharacterLiteral literal -> "C#"
      | otherwise -> T.pack (show (Syn.peelLiteralAnn literal))
    _ -> "_"

firstPatternIsDefault :: Syn.Match -> Bool
firstPatternIsDefault match =
  case Syn.matchPats match of
    pattern' : _ -> patternIsDefault pattern'
    [] -> False

firstPatternBindings :: Binder -> TcType -> Syn.Match -> ValueM [(TcTermKey, (Binder, TcType))]
firstPatternBindings binder ty match =
  case Syn.matchPats match of
    pattern' : _ -> patternMatchBindings pattern' binder ty
    [] -> pure []

matchArgumentBindings :: [Binder] -> [TcType] -> Syn.Match -> ValueM [(TcTermKey, (Binder, TcType))]
matchArgumentBindings binders types match =
  concat <$> mapM (\(pattern', binder, ty) -> patternMatchBindings pattern' binder ty) (zip3 (Syn.matchPats match) binders types)

dropFirstPattern :: Syn.Match -> Syn.Match
dropFirstPattern match = match {Syn.matchPats = drop 1 (Syn.matchPats match)}

specializeMatch :: Text -> Int -> Syn.Match -> Maybe Syn.Match
specializeMatch key arity match =
  case Syn.matchPats match of
    pattern' : rest
      | patternIsDefault pattern' -> Just match {Syn.matchPats = replicate arity Syn.PWildcard <> rest}
      | patternKey pattern' == key -> Just match {Syn.matchPats = patternChildren pattern' <> rest}
      | otherwise -> Nothing
    [] -> Nothing

patternIsIrrefutable :: Syn.Pattern -> Bool
patternIsIrrefutable pattern' =
  case pattern' of
    Syn.PAnn _ inner -> patternIsIrrefutable inner
    Syn.PParen inner -> patternIsIrrefutable inner
    Syn.PAs _ inner -> patternIsIrrefutable inner
    Syn.PIrrefutable {} -> True
    Syn.PTypeSig inner _ -> patternIsIrrefutable inner
    Syn.PVar {} -> True
    Syn.PWildcard -> True
    _ -> False

patternIsDefault :: Syn.Pattern -> Bool
patternIsDefault pattern' =
  case pattern' of
    Syn.PAnn _ inner -> patternIsDefault inner
    Syn.PParen inner -> patternIsDefault inner
    Syn.PAs _ inner -> patternIsDefault inner
    Syn.PStrict inner -> patternIsDefault inner
    Syn.PIrrefutable {} -> True
    Syn.PTypeSig inner _ -> patternIsDefault inner
    Syn.PVar {} -> True
    Syn.PWildcard -> True
    _ -> False

patternChildren :: Syn.Pattern -> [Syn.Pattern]
patternChildren pattern' =
  case peelPattern pattern' of
    Syn.PCon _ _ children -> children
    Syn.PInfix left _ right -> [left, right]
    Syn.PList [] -> []
    Syn.PList (item : items) ->
      let tailPattern = Syn.PList items
          checkedTail =
            case patternType pattern' of
              Just ty -> Syn.PAnn (Syn.mkAnnotation (TcAnnotation ty [] [] [] [] [])) tailPattern
              Nothing -> tailPattern
       in [item, checkedTail]
    Syn.PTuple _ children -> children
    Syn.PLit literal
      | Syn.LitChar value source <- Syn.peelLiteralAnn literal -> [Syn.PLit (Syn.LitCharHash value source)]
    _ -> []

patternConstructor :: Syn.Pattern -> ValueM AltCon
patternConstructor pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> AltData <$> resolvedTermName name
    Syn.PInfix _ name _ -> AltData <$> resolvedTermName name
    Syn.PList [] -> AltData <$> primitiveName "GHC.Types" "[]" SortDataConstructor
    Syn.PList (_ : _) -> AltData <$> primitiveName "GHC.Types" ":" SortDataConstructor
    Syn.PTuple flavor fields ->
      let arity = length fields
          constructor =
            case flavor of
              Syn.Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
              Syn.Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"
          moduleName' =
            case flavor of
              Syn.Boxed -> "GHC.Tuple"
              Syn.Unboxed -> "GHC.Types"
       in AltData <$> primitiveName moduleName' constructor SortDataConstructor
    Syn.PLit literal
      | isBoxedCharacterLiteral literal -> AltData <$> boxedCharConstructor
      | otherwise -> AltLit <$> patternLiteral literal
    Syn.PWildcard -> pure AltDefault
    Syn.PVar {} -> pure AltDefault
    unsupported -> failValue ("unsupported System FC pattern: " <> take 80 (show unsupported))

patternLiteral :: Syn.Literal -> ValueM Literal
patternLiteral literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitInt value numericType _ -> LitInt <$> convertRuntimeRep (numericRepresentation numericType) <*> pure value
    Syn.LitChar value _ -> LitChar <$> convertRuntimeRep WordRep <*> pure value
    Syn.LitCharHash value _ -> LitChar <$> convertRuntimeRep WordRep <*> pure value
    unsupported -> failValue ("unsupported System FC pattern literal: " <> show unsupported)

isBoxedCharacterLiteral :: Syn.Literal -> Bool
isBoxedCharacterLiteral literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitChar {} -> True
    _ -> False

patternFieldTypes :: Syn.Pattern -> [Syn.Pattern] -> ValueM [TcType]
patternFieldTypes parent children
  | Syn.PLit literal <- peelPattern parent,
    isBoxedCharacterLiteral literal =
      (: []) <$> boxedCharFieldType
  | otherwise = mapM requiredPatternType children

freshPatternBinder :: Syn.Pattern -> TcType -> ValueM Binder
freshPatternBinder pattern' = freshBinder (fromMaybe "_pat" (barePatternName pattern'))

patternMatchBindings :: Syn.Pattern -> Binder -> TcType -> ValueM [(TcTermKey, (Binder, TcType))]
patternMatchBindings pattern' binder ty =
  case pattern' of
    Syn.PAnn _ inner -> patternMatchBindings inner binder ty
    Syn.PParen inner -> patternMatchBindings inner binder ty
    Syn.PStrict inner -> patternMatchBindings inner binder ty
    Syn.PIrrefutable inner -> patternMatchBindings inner binder ty
    Syn.PTypeSig inner _ -> patternMatchBindings inner binder ty
    Syn.PVar name -> binderEntry name binder ty
    Syn.PAs name inner -> (<>) <$> binderEntry name binder ty <*> patternMatchBindings inner binder ty
    _ -> pure []

binderEntry :: Syn.UnqualifiedName -> Binder -> TcType -> ValueM [(TcTermKey, (Binder, TcType))]
binderEntry name binder ty = do
  key <- requiredBinderKey name
  pure [(key, (binder, ty))]

-- | The constructor pattern under the wrappers of a pattern. A string literal
-- pattern peels to the list pattern of its characters.
peelPattern :: Syn.Pattern -> Syn.Pattern
peelPattern pattern' = go pattern'
  where
    go inner =
      case inner of
        Syn.PAnn _ next -> go next
        Syn.PParen next -> go next
        Syn.PStrict next -> go next
        Syn.PIrrefutable next -> go next
        Syn.PAs _ next -> go next
        Syn.PTypeSig next _ -> go next
        Syn.PLit literal
          | Just expanded <- stringLiteralListPattern (patternType pattern') literal -> expanded
        _ -> inner

-- | A string literal pattern as the list pattern of its characters. The
-- element type comes from the checked list type of the pattern, so every
-- character pattern carries the type that the match compiler needs.
stringLiteralListPattern :: Maybe TcType -> Syn.Literal -> Maybe Syn.Pattern
stringLiteralListPattern patternTy literal =
  case (Syn.peelLiteralAnn literal, patternTy) of
    (Syn.LitString value source, Just (TcTyCon tyCon [elementType]))
      | tyConName tyCon == "[]" ->
          Just (Syn.PList [characterPattern elementType source char | char <- T.unpack value])
    _ -> Nothing
  where
    characterPattern elementType source char =
      Syn.PAnn (Syn.mkAnnotation (TcAnnotation elementType [] [] [] [] [])) (Syn.PLit (Syn.LitChar char source))

requiredPatternType :: Syn.Pattern -> ValueM TcType
requiredPatternType pattern' =
  case patternType pattern' of
    Just ty -> pure ty
    Nothing -> failValue ("missing checked pattern type: " <> take 80 (show pattern'))

patternType :: Syn.Pattern -> Maybe TcType
patternType pattern' =
  case pattern' of
    Syn.PVar name -> nameTcType name
    Syn.PAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> patternType inner
    Syn.PLit literal -> literalType literal
    Syn.PNegLit literal -> literalType literal
    Syn.PParen inner -> patternType inner
    Syn.PStrict inner -> patternType inner
    Syn.PIrrefutable inner -> patternType inner
    Syn.PAs name inner -> nameTcType name <|> patternType inner
    Syn.PTypeSig inner _ -> patternType inner
    _ -> Nothing

literalType :: Syn.Literal -> Maybe TcType
literalType literal =
  case literal of
    Syn.LitAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> literalType inner
    _ -> Nothing

fromBinderType :: Syn.Pattern -> TcType
fromBinderType pattern' = fromMaybe typeKindType (patternType pattern')

nameTcType :: Syn.UnqualifiedName -> Maybe TcType
nameTcType name =
  tcAnnType <$> listToMaybe (mapMaybe Syn.fromAnnotation (Syn.unqualifiedNameAnns name))

-- | Desugar a right-hand side with the checked result type. The failure
-- expression runs when each guard of a guarded right-hand side fails.
-- Without a failure expression, a failed guard ends in an empty case on its
-- own test value.
desugarRhsWithFailure :: TcType -> Maybe Expr -> Syn.Rhs Syn.Expr -> ValueM Expr
desugarRhsWithFailure resultType failure rhs =
  case rhs of
    Syn.UnguardedRhs _ expression Nothing -> desugarExpr expression
    Syn.UnguardedRhs _ expression (Just declarations) ->
      desugarLocalDecls declarations (requiredExprType expression) (desugarExpr expression)
    Syn.GuardedRhss _ alternatives maybeDecls ->
      let body = desugarGuardedRhss resultType failure alternatives
       in case maybeDecls of
            Nothing -> body
            Just declarations -> desugarLocalDecls declarations (pure resultType) body

rhsHasGuards :: Syn.Rhs Syn.Expr -> Bool
rhsHasGuards rhs =
  case rhs of
    Syn.GuardedRhss {} -> True
    Syn.UnguardedRhs {} -> False

-- | The expression that the remaining equations give when every guard of
-- the current equation fails. An equation without guards does not need it.
matchFailure :: TcType -> Maybe Expr -> [Binder] -> [TcType] -> Syn.Rhs Syn.Expr -> [MatchWork] -> ValueM (Maybe Expr)
matchFailure resultType fallback binders argumentTypes rhs rest
  | rhsHasGuards rhs && not (null rest) = Just <$> desugarMatchArguments resultType fallback binders argumentTypes rest
  | rhsHasGuards rhs = pure fallback
  | otherwise = pure Nothing

-- | Desugar guarded alternatives in order. A later alternative is the
-- failure expression of the alternative before it. Each guard of an
-- alternative names that failure expression.
desugarGuardedRhss :: TcType -> Maybe Expr -> [Syn.GuardedRhs Syn.Expr] -> ValueM Expr
desugarGuardedRhss resultType failure alternatives = do
  resultType' <- convertCheckedType resultType
  result <- foldr (step resultType') (pure failure) alternatives
  maybe (failValue "guarded right-hand side has no alternative") pure result
  where
    -- Each guard that can fail names the failure, so bind it once.
    step resultType' alternative rest = do
      next <- rest
      let guards = Syn.guardedRhsGuards alternative
      Just
        <$> shareFailure
          resultType
          next
          ( \shared ->
              desugarGuardQualifiers
                resultType
                resultType'
                shared
                guards
                (desugarExpr (Syn.guardedRhsBody alternative))
          )

desugarGuardQualifiers :: TcType -> Type -> Maybe Expr -> [Syn.GuardQualifier] -> ValueM Expr -> ValueM Expr
desugarGuardQualifiers resultType resultType' next qualifiers success =
  case qualifiers of
    [] -> success
    Syn.GuardAnn _ inner : rest -> desugarGuardQualifiers resultType resultType' next (inner : rest) success
    Syn.GuardExpr condition : rest -> do
      condition' <- desugarExpr condition
      conditionType <- requiredExprType condition
      binder <- freshBinder "_guard" conditionType
      trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
      falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
      body <- desugarGuardQualifiers resultType resultType' next rest success
      failure <- guardFailure resultType' next binder
      pure
        ( ExCase
            condition'
            binder
            resultType'
            [ Alt (AltData trueName) [] [] body,
              Alt (AltData falseName) [] [] failure
            ]
        )
    Syn.GuardPat pattern' scrutinee : rest -> do
      scrutinee' <- desugarExpr scrutinee
      scrutineeType <- requiredExprType scrutinee
      binder <- freshBinder "_guard_pat" scrutineeType
      failure <- guardFailure resultType' next binder
      body <-
        desugarPatternWithFailure
          resultType
          binder
          scrutineeType
          pattern'
          (desugarGuardQualifiers resultType resultType' next rest success)
          (Just failure)
      pure (ExLet (Bind binder scrutinee') body)
    Syn.GuardLet declarations : rest ->
      desugarLocalDecls declarations (pure resultType) (desugarGuardQualifiers resultType resultType' next rest success)

-- | The expression that runs when a guard fails. Without a later
-- alternative, an empty case on the guard value reports the failure.
guardFailure :: Type -> Maybe Expr -> Binder -> ValueM Expr
guardFailure resultType' next binder =
  case next of
    Just failure -> pure failure
    Nothing -> do
      failureBinder <- freshBinderFromType "_guard_nomatch" (binderType binder)
      pure (ExCase (ExVar (binderName binder)) failureBinder resultType' [])

desugarExpr :: Syn.Expr -> ValueM Expr
desugarExpr expression =
  case expression of
    Syn.EAnn annotation inner
      | Just tcAnnotation <- Syn.fromAnnotation annotation -> desugarAnnotatedExpr tcAnnotation inner
      | Just resolution <- Syn.fromAnnotation annotation,
        isIfThenElseResolution resolution ->
          failValue "rebindable if expression is missing its checked ifThenElse method"
      | otherwise -> desugarExpr inner
    Syn.EVar name -> desugarVariable Nothing name
    Syn.EApp function argument -> desugarApplication function argument
    Syn.EInfix left operator right
      | isApplicationOperator operator ->
          -- The type checker gives f $ x the type of the application f x,
          -- so the operator node has no type arguments.
          ExApp <$> desugarExpr left <*> desugarExpr right
      | otherwise -> do
          operator' <- desugarInfixOperator operator
          (ExApp . ExApp operator' <$> desugarExpr left) <*> desugarExpr right
    Syn.EParen inner -> desugarExpr inner
    Syn.ETypeSig inner _ -> desugarExpr inner
    Syn.ETypeApp function _ -> desugarExpr function
    Syn.ELambdaPats patterns body -> desugarLambda Nothing patterns body
    Syn.ELambdaCase alternatives -> desugarLambdaCaseMatches (map caseAlternativeMatch alternatives)
    Syn.ELambdaCases alternatives -> desugarLambdaCaseMatches (map lambdaCaseAltMatch alternatives)
    Syn.EIf condition thenExpression elseExpression -> do
      resultType <- requiredExprType thenExpression
      desugarIf resultType condition thenExpression elseExpression
    Syn.ECase {} -> failValue "case expression does not have a checked result type"
    Syn.ELetDecls declarations body ->
      desugarLocalDecls declarations (requiredExprType body) (desugarExpr body)
    unsupported -> failValue ("unsupported System FC expression: " <> take 80 (show unsupported))

desugarAnnotatedExpr :: TcAnnotation -> Syn.Expr -> ValueM Expr
desugarAnnotatedExpr annotation inner = do
  let evidencePredicates = [predicate | Ev.EvGiven predicate <- tcAnnEvidenceBinders annotation]
  evidenceBinders <- zipWithM (freshDictionaryBinder "$higher_rank_d") [0 :: Int ..] evidencePredicates
  body <-
    withAlternativeScope (not (null (tcAnnTypeBinders annotation))) (zipWith Dictionary evidencePredicates evidenceBinders) $
      case inner of
        _
          | not (null (tcAnnTypeBinders annotation)) || not (null evidenceBinders) -> desugarExpr inner
        expression
          | Just name <- annotatedVariable expression -> do
              desugarVariable (Just annotation) name
        Syn.EAnn resolutionAnnotation (Syn.EInt value Syn.TInteger _)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation,
            resolutionNamespace resolution == ResolutionNamespaceTerm,
            resolutionIdentifier resolution == IdentifierNamed "fromInteger" ->
              desugarOverloadedInteger annotation resolution value
        Syn.EAnn resolutionAnnotation (Syn.EFloat value Syn.TFractional _)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation,
            resolutionNamespace resolution == ResolutionNamespaceTerm,
            resolutionIdentifier resolution == IdentifierNamed "fromRational" ->
              desugarOverloadedRational annotation resolution value
        Syn.EAnn resolutionAnnotation (Syn.EIf condition thenExpression elseExpression)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation,
            isIfThenElseResolution resolution ->
              desugarRebindableIf annotation resolution condition thenExpression elseExpression
        Syn.EAnn resolutionAnnotation (Syn.ENegate operand)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation,
            resolutionNamespace resolution == ResolutionNamespaceTerm,
            resolutionIdentifier resolution == IdentifierNamed "negate" -> do
              method <- desugarResolvedOccurrence annotation resolution
              ExApp method <$> desugarExpr operand
        Syn.EAnn resolutionAnnotation primitiveLiteral
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation,
            resolutionNamespace resolution == ResolutionNamespaceType,
            isPrimitiveLiteral primitiveLiteral ->
              desugarAnnotatedExpr annotation primitiveLiteral
        Syn.EInt value numericType _
          | numericType /= Syn.TInteger -> do
              representation <- convertRuntimeRep (numericRepresentation numericType)
              pure (ExLit (LitInt representation value))
        Syn.EChar value _ -> do
          constructor <- boxedCharConstructor
          representation <- convertRuntimeRep WordRep
          pure (ExApp (ExVar constructor) (ExLit (LitChar representation value)))
        Syn.ECharHash value _ -> do
          representation <- convertRuntimeRep WordRep
          pure (ExLit (LitChar representation value))
        Syn.EString value _ -> desugarString annotation value
        _
          | isTemplateHaskellQuote inner -> desugarTemplateHaskellQuote annotation
        Syn.EStringHash value _ -> do
          representation <- convertRuntimeRep AddrRep
          pure (ExLit (LitAddr representation (BS.pack (map (fromIntegral . fromEnum) (T.unpack value)))))
        Syn.EList elements -> desugarList annotation elements
        Syn.EListComp expression statements -> desugarListComp annotation expression statements
        Syn.EArithSeq arithSeq -> desugarArithSeq arithSeq
        Syn.ETuple flavor elements -> desugarTuple annotation flavor elements
        Syn.ESectionL operand operator -> desugarSectionL annotation operand operator
        Syn.ESectionR operator operand -> desugarSectionR annotation operator operand
        Syn.EDo statements _ -> desugarDo statements
        Syn.EIf condition thenExpression elseExpression ->
          desugarIf (tcAnnType annotation) condition thenExpression elseExpression
        -- A multi-way if is a guarded right-hand side. A failed last guard
        -- ends in an empty case on its own test value.
        Syn.EMultiWayIf alternatives ->
          desugarGuardedRhss (tcAnnType annotation) Nothing alternatives
        Syn.ECase scrutinee alternatives -> desugarCase (tcAnnType annotation) scrutinee alternatives
        Syn.EImplicitParam name ->
          case tcAnnEvidenceTerms annotation of
            [evidence] -> desugarEvidence evidence
            _ -> failValue ("implicit parameter " <> T.unpack name <> " does not have exactly one evidence term")
        Syn.ELambdaPats patterns lambdaBody -> desugarLambda (Just (tcAnnType annotation)) patterns lambdaBody
        Syn.ELambdaCase alternatives -> desugarMatches (tcAnnType annotation) (map caseAlternativeMatch alternatives)
        Syn.ELambdaCases alternatives -> desugarMatches (tcAnnType annotation) (map lambdaCaseAltMatch alternatives)
        -- An application with a polymorphic type is instantiated where it
        -- is applied. The annotation gives the type arguments and the
        -- evidence.
        _
          | isApplicationExpression inner,
            not (null (tcAnnTypeArgs annotation)) || not (null (tcAnnEvidenceTerms annotation)) -> do
              inner' <- desugarExpr inner
              types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
              evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
              pure (foldl ExApp (foldl ExTyApp inner' types) evidence)
        _ -> desugarExpr inner
  typeBinders <- convertTypeBinders (tcAnnTypeBinders annotation)
  pure (foldr ExTyLam (foldr ExLam body evidenceBinders) typeBinders)

isIfThenElseResolution :: ResolutionAnnotation -> Bool
isIfThenElseResolution resolution =
  resolutionNamespace resolution == ResolutionNamespaceTerm
    && resolutionIdentifier resolution == IdentifierNamed "ifThenElse"

-- | RebindableSyntax applies the checked ifThenElse method to the
-- condition and the two branches.
desugarRebindableIf :: TcAnnotation -> ResolutionAnnotation -> Syn.Expr -> Syn.Expr -> Syn.Expr -> ValueM Expr
desugarRebindableIf annotation resolution condition thenExpression elseExpression = do
  method <- desugarResolvedOccurrence annotation resolution
  arguments <- mapM desugarExpr [condition, thenExpression, elseExpression]
  pure (foldl ExApp method arguments)

desugarIf :: TcType -> Syn.Expr -> Syn.Expr -> Syn.Expr -> ValueM Expr
desugarIf resultType condition thenExpression elseExpression = do
  condition' <- desugarExpr condition
  conditionType <- requiredExprType condition
  binder <- freshBinder "_if" conditionType
  resultType' <- convertCheckedType resultType
  trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
  falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
  thenExpression' <- desugarExpr thenExpression
  elseExpression' <- desugarExpr elseExpression
  pure
    ( ExCase
        condition'
        binder
        resultType'
        [ Alt (AltData trueName) [] [] thenExpression',
          Alt (AltData falseName) [] [] elseExpression'
        ]
    )

annotatedVariable :: Syn.Expr -> Maybe Syn.Name
annotatedVariable expression =
  case expression of
    Syn.EAnn _ inner -> annotatedVariable inner
    Syn.EParen inner -> annotatedVariable inner
    Syn.EVar name -> Just name
    _ -> Nothing

localOccurrenceTypeArguments :: Syn.Name -> TcAnnotation -> ValueM [TcType]
localOccurrenceTypeArguments name annotation
  | not (null (tcAnnTypeArgs annotation)) = pure (tcAnnTypeArgs annotation)
  | otherwise = do
      local <- maybe (pure Nothing) (\key -> Map.lookup key <$> gets vsLocals) (nameTermKey name)
      pure
        ( fromMaybe [] $ do
            (_, declaredType) <- local
            let (typeVariables, bodyType) = peelForAlls declaredType
            substitution <- matchTypes [bodyType] [tcAnnType annotation]
            mapM (\typeVariable -> Map.lookup (tvUnique typeVariable) substitution) typeVariables
        )

desugarInfixOperator :: Syn.Name -> ValueM Expr
desugarInfixOperator operator = do
  let maybeAnnotation = listToMaybe (mapMaybe Syn.fromAnnotation (Syn.nameAnns operator))
  maybeStrict <- strictConstructorData operator
  case (maybeStrict, maybeAnnotation) of
    (Just strictFlags, Just annotation) ->
      desugarStrictConstructor operator annotation strictFlags
    (Just strictFlags, Nothing) -> do
      constructorType <- lookupBindingType =<< requiredNameTermKey operator
      let annotation = TcAnnotation constructorType [] [] [] [] []
      desugarStrictConstructor operator annotation strictFlags
    (Nothing, Just annotation) -> do
      variable <- resolvedTermName operator
      types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
      evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
      desugarTermReference variable types evidence (seqTermArgumentTypes annotation)
    (Nothing, Nothing) -> do
      variable <- resolvedTermName operator
      desugarTermReference variable [] [] []

desugarSectionL :: TcAnnotation -> Syn.Expr -> Syn.Name -> ValueM Expr
desugarSectionL annotation operand operator = do
  binder <- freshBinder "_section" =<< sectionArgumentType annotation
  operator' <- desugarInfixOperator operator
  operand' <- desugarExpr operand
  pure (ExLam binder (ExApp (ExApp operator' operand') (ExVar (binderName binder))))

desugarSectionR :: TcAnnotation -> Syn.Name -> Syn.Expr -> ValueM Expr
desugarSectionR annotation operator operand = do
  binder <- freshBinder "_section" =<< sectionArgumentType annotation
  operator' <- desugarInfixOperator operator
  operand' <- desugarExpr operand
  pure (ExLam binder (ExApp (ExApp operator' (ExVar (binderName binder))) operand'))

sectionArgumentType :: TcAnnotation -> ValueM TcType
sectionArgumentType annotation =
  case tcAnnTermArgTypes annotation of
    [argumentType] -> pure argumentType
    argumentTypes -> failValue ("operator section has " <> show (length argumentTypes) <> " checked argument types")

desugarApplication :: Syn.Expr -> Syn.Expr -> ValueM Expr
desugarApplication function argument = do
  argument' <- desugarExpr argument
  ExApp <$> desugarExpr function <*> pure argument'

desugarVariable :: Maybe TcAnnotation -> Syn.Name -> ValueM Expr
desugarVariable maybeAnnotation name = do
  maybeFamily <- familyConstructorData name
  maybeNewtype <- newtypeConstructorData name
  maybeStrict <- strictConstructorData name
  case (maybeFamily, maybeNewtype, maybeStrict) of
    (Just info, _, _) -> do
      annotation <- constructorAnnotation
      desugarFamilyConstructor name annotation info
    (_, Just dataType, _) -> do
      annotation <- constructorAnnotation
      desugarNewtypeConstructor annotation dataType
    (_, _, Just strictFlags) -> do
      annotation <- constructorAnnotation
      desugarStrictConstructor name annotation strictFlags
    _ -> do
      variable <- patSynBuilderName =<< resolvedTermName name
      case maybeAnnotation of
        Nothing -> desugarTermReference variable [] [] []
        Just annotation -> do
          inferredTypes <- localOccurrenceTypeArguments name annotation
          types <- mapM convertCheckedType inferredTypes
          evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
          desugarTermReference variable types evidence (seqTermArgumentTypes annotation)
  where
    constructorAnnotation =
      case maybeAnnotation of
        Just value -> pure value
        Nothing -> do
          constructorType <- lookupBindingType =<< requiredNameTermKey name
          pure (TcAnnotation constructorType [] [] [] [] [])

seqTermArgumentTypes :: TcAnnotation -> [TcType]
seqTermArgumentTypes annotation
  | null (tcAnnTermArgTypes annotation) =
      let (_, afterForAlls) = peelForAlls (tcAnnType annotation)
          (_, bodyType) = peelConstraints afterForAlls
       in fst (peelFunctions 2 bodyType)
  | otherwise = tcAnnTermArgTypes annotation

-- | An expression use of a bidirectional pattern synonym refers to its
-- builder.
patSynBuilderName :: Name -> ValueM Name
patSynBuilderName variable =
  case nameOrigin variable of
    OriginTop package moduleName' -> do
      patSyns <- gets vsPatSyns
      pure $
        case Map.lookup (TcTermGlobal package moduleName' (nameText variable)) patSyns of
          Just info -> variable {nameText = patSynHelperName "$b" info}
          Nothing -> variable
    _ -> pure variable

desugarTermReference :: Name -> [Type] -> [Expr] -> [TcType] -> ValueM Expr
desugarTermReference variable types evidence termArgumentTypes
  | nameText variable /= "seq" = do
      foreignImports <- gets vsForeignImports
      case nameOrigin variable of
        OriginTop package moduleName'
          | Just info <- Map.lookup (TcTermGlobal package moduleName' (nameText variable)) foreignImports ->
              desugarForeignReference variable (TcTermGlobal package moduleName' (nameText variable)) info types evidence
        _ -> pure ordinaryReference
  | OriginLocal {} <- nameOrigin variable = pure ordinaryReference
  | OriginTop package moduleName' <- nameOrigin variable = do
      moduleOrigin <- gets vsModuleOrigin
      primitivePackage <- gets (cePrimPackage . vsConvertEnv)
      if (package, moduleName') == (primitivePackage, "GHC.Prim")
        then do
          unless (null evidence) (failValue "GHC.Prim.seq has unexpected evidence arguments")
          desugarPrimitiveSeq termArgumentTypes
        else
          if (package, moduleName') == moduleOrigin
            then pure ordinaryReference
            else failValue "System FC accepts an imported seq only from the configured GHC.Prim module"
  where
    ordinaryReference = foldl ExApp (foldl ExTyApp (ExVar variable) types) evidence

desugarPrimitiveSeq :: [TcType] -> ValueM Expr
desugarPrimitiveSeq termArgumentTypes =
  case termArgumentTypes of
    [firstType, secondType] -> do
      first <- freshBinder "seq_first" firstType
      second <- freshBinder "seq_second" secondType
      evaluated <- freshBinder "seq_evaluated" firstType
      resultType <- convertCheckedType secondType
      pure
        ( ExLam
            first
            ( ExLam
                second
                ( ExCase
                    (ExVar (binderName first))
                    evaluated
                    resultType
                    [Alt AltDefault [] [] (ExVar (binderName second))]
                )
            )
        )
    argumentTypes -> failValue ("GHC.Prim.seq has " <> show (length argumentTypes) <> " checked term argument types")

-- | The strict field flags of a data constructor that has one strict field
-- or more.
strictConstructorData :: Syn.Name -> ValueM (Maybe [Bool])
strictConstructorData name = do
  strictConstructors <- gets vsStrictConstructors
  pure (nameTermKey name >>= (`Map.lookup` strictConstructors))

-- | A data constructor with strict fields evaluates each strict field before
-- it builds the value. Give the constructor a wrapper that does this. The
-- wrapper takes each field, forces the strict lifted fields in field order,
-- and then applies the constructor. An unlifted field is already evaluated,
-- so the wrapper does not force it.
desugarStrictConstructor :: Syn.Name -> TcAnnotation -> [Bool] -> ValueM Expr
desugarStrictConstructor name annotation strictFlags = do
  let (_, afterForAlls) = peelForAlls (tcAnnType annotation)
      (_, bodyType) = peelConstraints afterForAlls
      (fieldTypes, resultType) = splitFunctionType bodyType
  if length fieldTypes /= length strictFlags
    then failValue ("strict constructor " <> T.unpack (Syn.nameText name) <> " has an unexpected field count")
    else do
      constructor <- resolvedTermName name
      inferredTypes <- localOccurrenceTypeArguments name annotation
      types <- mapM convertCheckedType inferredTypes
      evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
      fields <- mapM (freshBinder "_strict_field") fieldTypes
      convertedResult <- convertCheckedType resultType
      kindEnv <- gets (ceKindEnv . vsConvertEnv)
      let applied =
            foldl
              ExApp
              (foldl ExApp (foldl ExTyApp (ExVar constructor) types) evidence)
              (map (ExVar . binderName) fields)
          forced (strict, binder, fieldType) inner
            | strict && not (isUnliftedTypeInEnv kindEnv fieldType) = do
                evaluated <- freshBinder "_strict_forced" fieldType
                pure
                  ( ExCase
                      (ExVar (binderName binder))
                      evaluated
                      convertedResult
                      [Alt AltDefault [] [] inner]
                  )
            | otherwise = pure inner
      body <- foldrM forced applied (zip3 strictFlags fields fieldTypes)
      pure (foldr ExLam body fields)

newtypeConstructorData :: Syn.Name -> ValueM (Maybe DataTypeInfo)
newtypeConstructorData name = do
  newtypes <- gets vsNewtypeConstructors
  pure (nameTermKey name >>= (`Map.lookup` newtypes))

familyConstructorData :: Syn.Name -> ValueM (Maybe DataFamilyInstanceInfo)
familyConstructorData name = do
  families <- gets vsFamilyConstructors
  pure (nameTermKey name >>= (`Map.lookup` families))

-- | A data-family constructor builds the representation type. The source
-- program sees the family type. Cast the built value with the family axiom.
-- A newtype instance has no constructor in System FC. Cast its field with
-- the representation axiom and then with the family axiom.
desugarFamilyConstructor :: Syn.Name -> TcAnnotation -> DataFamilyInstanceInfo -> ValueM Expr
desugarFamilyConstructor name annotation info = do
  let (_, afterForAlls) = peelForAlls (tcAnnType annotation)
      (_, bodyType) = peelConstraints afterForAlls
      (fieldTypes, resultType) = splitFunctionType bodyType
  instanceArguments <- familyInstanceArguments info resultType
  axiomArguments <- mapM convertCheckedType instanceArguments
  fields <- mapM (freshBinder "_field") fieldTypes
  let familyCoercion = CoSym (CoAxiom (familyAxiomName info) axiomArguments)
  body <-
    if dfiiIsNewtype info
      then case fields of
        [field] ->
          pure (ExCast (ExVar (binderName field)) (CoTrans (CoSym (CoAxiom (familyRepresentationAxiomName info) axiomArguments)) familyCoercion))
        _ -> failValue ("newtype family constructor does not have one field: " <> T.unpack (Syn.nameText name))
      else do
        constructor <- resolvedTermName name
        types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
        evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
        let applied = foldl ExApp (foldl ExApp (foldl ExTyApp (ExVar constructor) types) evidence) (map (ExVar . binderName) fields)
        pure (ExCast applied familyCoercion)
  pure (foldr ExLam body fields)

splitFunctionType :: TcType -> ([TcType], TcType)
splitFunctionType ty =
  case ty of
    TcFunTy argument result ->
      let (arguments, final) = splitFunctionType result
       in (argument : arguments, final)
    _ -> ([], ty)

-- | Match the instance head against a family type. The result gives the
-- instance type variables in declaration order.
familyInstanceArguments :: DataFamilyInstanceInfo -> TcType -> ValueM [TcType]
familyInstanceArguments info familyType =
  case matchTypes [dfiiFamilyType info] [familyType] of
    Nothing -> failValue ("data-family instance " <> T.unpack (dfiiFamilyName info) <> " does not match the type " <> show familyType)
    Just substitution ->
      mapM
        ( \tyVar ->
            maybe
              (failValue ("data-family instance " <> T.unpack (dfiiFamilyName info) <> " has an unbound type variable"))
              pure
              (Map.lookup (tvUnique tyVar) substitution)
        )
        (dfiiTyVars info)

familyAxiomName :: DataFamilyInstanceInfo -> Name
familyAxiomName info =
  let tyCon = dfiiRepresentationTyCon info
   in Name (dfiiAxiomName info) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

familyRepresentationAxiomName :: DataFamilyInstanceInfo -> Name
familyRepresentationAxiomName info =
  let tyCon = dfiiRepresentationTyCon info
   in Name ("$ax$" <> T.drop 1 (tyConName tyCon)) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

desugarNewtypeConstructor :: TcAnnotation -> DataTypeInfo -> ValueM Expr
desugarNewtypeConstructor annotation dataType = do
  let (_, afterForAlls) = peelForAlls (tcAnnType annotation)
      (_, bodyType) = peelConstraints afterForAlls
  (argumentType, resultType) <-
    case bodyType of
      TcFunTy argument result -> pure (argument, result)
      _ -> failValue ("newtype constructor does not have a function type: " <> T.unpack (dtiName dataType))
  argument <- freshBinder "_newtype" argumentType
  let resultArguments =
        case resultType of
          TcTyCon _ arguments -> arguments
          _ -> []
      typeArguments =
        case tcAnnTypeArgs annotation of
          [] -> resultArguments
          arguments -> arguments
      tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  pure (ExLam argument (ExCast (ExVar (binderName argument)) (CoSym (CoAxiom axiom convertedArguments))))

convertNewtypeAxiomArguments :: DataTypeInfo -> [TcType] -> ValueM [Type]
convertNewtypeAxiomArguments dataType arguments =
  if length arguments > length (dtiTyVars dataType)
    then mapM convertCheckedType arguments
    else do
      env <- gets vsConvertEnv
      invisibleArguments <- liftEither (invisibleKindArgs env (dtiTyCon dataType) arguments Nothing)
      visibleArguments <- mapM convertCheckedType arguments
      pure (invisibleArguments <> visibleArguments)

convertTyConApplicationArguments :: TyCon -> [TcType] -> ValueM [Type]
convertTyConApplicationArguments tyCon arguments = do
  env <- gets vsConvertEnv
  invisibleArguments <- liftEither (invisibleKindArgs env tyCon arguments Nothing)
  visibleArguments <- mapM convertCheckedType arguments
  pure (invisibleArguments <> visibleArguments)

-- | Desugar a lambda-case into ordinary function equations.
desugarLambdaCaseMatches :: [Syn.Match] -> ValueM Expr
desugarLambdaCaseMatches matches = do
  ty <- lambdaCaseType matches
  desugarMatches ty matches

-- | The type of a lambda-case: the pattern types of its first alternative
-- to the type of its right-hand side.
lambdaCaseType :: [Syn.Match] -> ValueM TcType
lambdaCaseType matches =
  case matches of
    [] -> failValue "lambda-case has no alternative"
    first : _ -> do
      types <- mapM requiredPatternType (Syn.matchPats first)
      resultType <- requiredRhsType (Syn.matchRhs first)
      pure (foldr TcFunTy resultType types)

requiredRhsType :: Syn.Rhs Syn.Expr -> ValueM TcType
requiredRhsType rhs =
  case rhs of
    Syn.UnguardedRhs _ expression _ -> requiredExprType expression
    Syn.GuardedRhss _ alternatives _ ->
      case alternatives of
        alternative : _ -> requiredExprType (Syn.guardedRhsBody alternative)
        [] -> failValue "lambda-case has no guarded right-hand side"

-- | Desugar a lambda. Variable, wildcard, and as-patterns bind their
-- argument directly. Any other pattern turns the lambda into a single
-- function equation so the match desugarer emits the constructor cases.
-- The checked lambda type, when the annotation supplies it, provides the
-- result type; otherwise the body's own annotation does.
desugarLambda :: Maybe TcType -> [Syn.Pattern] -> Syn.Expr -> ValueM Expr
desugarLambda lambdaType patterns body = do
  types <- mapM requiredPatternType patterns
  binders <- zipWithM freshPatternBinder patterns types
  direct <- sequence <$> mapM (\(pattern', binder, ty) -> directPatternBindings pattern' binder ty) (zip3 patterns binders types)
  case direct of
    Just locals -> do
      body' <- withLocals (concat locals) (desugarExpr body)
      pure (foldr ExLam body' binders)
    Nothing -> do
      resultType <-
        case lambdaType of
          Just ty -> pure (snd (peelFunctions (length patterns) ty))
          Nothing -> requiredExprType body
      desugarMatches (foldr TcFunTy resultType types) [lambdaMatch]
  where
    lambdaMatch =
      Syn.Match
        { Syn.matchAnns = [],
          Syn.matchHeadForm = Syn.MatchHeadPrefix,
          Syn.matchPats = patterns,
          Syn.matchRhs = Syn.UnguardedRhs [] body Nothing
        }

desugarList :: TcAnnotation -> [Syn.Expr] -> ValueM Expr
desugarList annotation elements = do
  elementType <-
    case tcAnnTypeArgs annotation of
      [ty] -> pure ty
      types -> failValue ("list annotation has " <> show (length types) <> " element types")
  convertedType <- convertCheckedType elementType
  elements' <- mapM desugarExpr elements
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) convertedType
      cons = ExTyApp (ExVar consName) convertedType
  pure (foldr (ExApp . ExApp cons) nil elements')

desugarListComp :: TcAnnotation -> Syn.Expr -> [Syn.CompStmt] -> ValueM Expr
desugarListComp annotation expression statements = do
  elementType <- listElementType "list comprehension result" (tcAnnType annotation)
  convertedElementType <- convertCheckedType elementType
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) convertedElementType
      cons = ExTyApp (ExVar consName) convertedElementType
  desugarListCompStatements elementType cons expression statements nil

desugarListCompStatements :: TcType -> Expr -> Syn.Expr -> [Syn.CompStmt] -> Expr -> ValueM Expr
desugarListCompStatements resultElementType cons expression statements rest =
  case statements of
    [] -> do
      expression' <- desugarExpr expression
      pure (ExApp (ExApp cons expression') rest)
    statement : remaining ->
      case statement of
        Syn.CompAnn _ inner -> desugarListCompStatements resultElementType cons expression (inner : remaining) rest
        Syn.CompGen pattern' source ->
          desugarListCompGenerator resultElementType cons expression pattern' source remaining rest
        Syn.CompGuard guard -> do
          success <- desugarListCompStatements resultElementType cons expression remaining rest
          desugarListCompGuard resultElementType guard success rest
        Syn.CompLetDecls declarations ->
          desugarLocalDecls declarations (listTypeFromElement resultElementType) (desugarListCompStatements resultElementType cons expression remaining rest)
        unsupported -> failValue ("unsupported list comprehension statement: " <> take 80 (show unsupported))

desugarListCompGenerator :: TcType -> Expr -> Syn.Expr -> Syn.Pattern -> Syn.Expr -> [Syn.CompStmt] -> Expr -> ValueM Expr
desugarListCompGenerator resultElementType cons expression pattern' source remaining rest = do
  sourceType <- requiredExprType source
  sourceElementType <- listElementType "list comprehension generator" sourceType
  resultListType <- listTypeFromElement resultElementType
  function <- freshBinder "_list_comp" (TcFunTy sourceType resultListType)
  argument <- freshBinder "_list_comp_list" sourceType
  item <- freshPatternBinder pattern' sourceElementType
  items <- freshBinder "_list_comp_tail" sourceType
  caseBinder <- freshBinder "_list_comp_scrut" sourceType
  resultType <- convertCheckedType resultListType
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let recursiveCall = ExApp (ExVar (binderName function)) (ExVar (binderName items))
  success <-
    desugarListCompPattern
      resultListType
      item
      sourceElementType
      pattern'
      (desugarListCompStatements resultElementType cons expression remaining recursiveCall)
      recursiveCall
  source' <- desugarExpr source
  let loop =
        ExCase
          (ExVar (binderName argument))
          caseBinder
          resultType
          [ Alt (AltData nilName) [] [] rest,
            Alt (AltData consName) [] [item, items] success
          ]
  pure (ExRec [Bind function (ExLam argument loop)] (ExApp (ExVar (binderName function)) source'))

desugarListCompGuard :: TcType -> Syn.Expr -> Expr -> Expr -> ValueM Expr
desugarListCompGuard resultElementType guard success failure = do
  guard' <- desugarExpr guard
  guardType <- requiredExprType guard
  binder <- freshBinder "_list_comp_guard" guardType
  resultType <- convertCheckedType =<< listTypeFromElement resultElementType
  trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
  falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
  pure
    ( ExCase
        guard'
        binder
        resultType
        [ Alt (AltData trueName) [] [] success,
          Alt (AltData falseName) [] [] failure
        ]
    )

desugarListCompPattern :: TcType -> Binder -> TcType -> Syn.Pattern -> ValueM Expr -> Expr -> ValueM Expr
desugarListCompPattern resultType binder ty pattern' success failure =
  case pattern' of
    Syn.PAnn _ inner -> desugarListCompPattern resultType binder ty inner success failure
    Syn.PParen inner -> desugarListCompPattern resultType binder ty inner success failure
    Syn.PStrict inner -> desugarListCompPattern resultType binder ty inner success failure
    Syn.PIrrefutable inner -> desugarListCompPattern resultType binder ty inner success failure
    Syn.PTypeSig inner _ -> desugarListCompPattern resultType binder ty inner success failure
    Syn.PVar name -> do
      locals <- binderEntry name binder ty
      withLocals locals success
    Syn.PWildcard -> success
    Syn.PAs name inner -> do
      locals <- binderEntry name binder ty
      withLocals locals (desugarListCompPattern resultType binder ty inner success failure)
    _ -> do
      -- The wrappers above are peeled off, so the pattern can have lost
      -- its checked type. A list pattern needs it for its synthesized
      -- tail.
      let typed =
            case patternType pattern' of
              Just _ -> pattern'
              Nothing -> Syn.PAnn (Syn.mkAnnotation (TcAnnotation ty [] [] [] [] [])) pattern'
      desugarListCompConstructorPattern resultType binder typed success failure

desugarListCompConstructorPattern :: TcType -> Binder -> Syn.Pattern -> ValueM Expr -> Expr -> ValueM Expr
desugarListCompConstructorPattern resultType binder pattern' success failure = do
  maybeNewtype <- doPatternNewtype pattern'
  case maybeNewtype of
    Just dataType -> desugarListCompNewtypePattern resultType binder pattern' dataType success failure
    Nothing -> do
      let children = patternChildren pattern'
          predicates = patternGivenPredicates pattern'
          typeVariables = patternTypeVariables pattern'
      typeBinders <- convertTypeBinders typeVariables
      fieldTypes <- patternFieldTypes pattern' children
      fields <- zipWithM freshPatternBinder children fieldTypes
      dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] predicates
      constructor <- patternConstructor pattern'
      resultType' <- convertCheckedType resultType
      caseBinder <- freshBinderFromType "_list_comp_pattern" (binderType binder)
      body <-
        withAlternativeScope
          (not (null typeBinders))
          (zipWith Dictionary predicates dictionaries)
          (desugarListCompChildPatterns resultType (zip3 fields fieldTypes children) success failure)
      pure
        ( ExCase
            (ExVar (binderName binder))
            caseBinder
            resultType'
            [ Alt constructor typeBinders (dictionaries <> fields) body,
              Alt AltDefault [] [] failure
            ]
        )

desugarListCompChildPatterns :: TcType -> [(Binder, TcType, Syn.Pattern)] -> ValueM Expr -> Expr -> ValueM Expr
desugarListCompChildPatterns resultType children success failure =
  case children of
    [] -> success
    (binder, ty, pattern') : remaining ->
      desugarListCompPattern
        resultType
        binder
        ty
        pattern'
        (desugarListCompChildPatterns resultType remaining success failure)
        failure

desugarListCompNewtypePattern :: TcType -> Binder -> Syn.Pattern -> DataTypeInfo -> ValueM Expr -> Expr -> ValueM Expr
desugarListCompNewtypePattern resultType binder pattern' dataType success failure = do
  child <-
    case patternChildren pattern' of
      [fieldPattern] -> pure fieldPattern
      _ -> failValue ("newtype list comprehension pattern does not have one field: " <> T.unpack (dtiName dataType))
  childType <- requiredPatternType child
  field <- freshPatternBinder child childType
  typeArguments <- newtypePatternArguments pattern'
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  let tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      unwrapped = ExCast (ExVar (binderName binder)) (CoAxiom axiom convertedArguments)
  body <- desugarListCompPattern resultType field childType child success failure
  pure (ExLet (Bind field unwrapped) body)

listElementType :: String -> TcType -> ValueM TcType
listElementType label ty =
  case ty of
    TcTyCon tyCon [elementType]
      | tyConName tyCon == "[]" -> pure elementType
    _ -> failValue (label <> " does not have a checked list type: " <> show ty)

listTypeFromElement :: TcType -> ValueM TcType
listTypeFromElement elementType = do
  package <- gets (cePrimPackage . vsConvertEnv)
  pure (TcTyCon (mkTyConWithOrigin package "GHC.Types" "[]" 1) [elementType])

desugarArithSeq :: Syn.ArithSeq -> ValueM Expr
desugarArithSeq arithSeq =
  case arithSeq of
    Syn.ArithSeqAnn annotation inner
      | Just tcAnnotation <- Syn.fromAnnotation annotation ->
          desugarCheckedArithSeq tcAnnotation inner
      | otherwise -> desugarArithSeq inner
    _ -> failValue "arithmetic sequence is missing its checked method"

desugarCheckedArithSeq :: TcAnnotation -> Syn.ArithSeq -> ValueM Expr
desugarCheckedArithSeq tcAnnotation arithSeq =
  case arithSeq of
    Syn.ArithSeqAnn annotation inner
      | Just resolution <- Syn.fromAnnotation annotation -> do
          method <- desugarResolvedOccurrence tcAnnotation resolution
          arguments <- mapM desugarExpr (arithSeqArguments inner)
          pure (foldl ExApp method arguments)
      | otherwise -> desugarCheckedArithSeq tcAnnotation inner
    _ -> failValue "arithmetic sequence is missing its resolved method"

arithSeqArguments :: Syn.ArithSeq -> [Syn.Expr]
arithSeqArguments arithSeq =
  case arithSeq of
    Syn.ArithSeqAnn _ inner -> arithSeqArguments inner
    Syn.ArithSeqFrom from -> [from]
    Syn.ArithSeqFromThen from thenExpression -> [from, thenExpression]
    Syn.ArithSeqFromTo from to -> [from, to]
    Syn.ArithSeqFromThenTo from thenExpression to -> [from, thenExpression, to]

-- | A Template Haskell quote such as @[| e |]@, @[t| ty |]@, or @'name@.
isTemplateHaskellQuote :: Syn.Expr -> Bool
isTemplateHaskellQuote expression =
  case expression of
    Syn.ETHExpQuote {} -> True
    Syn.ETHTypedQuote {} -> True
    Syn.ETHDeclQuote {} -> True
    Syn.ETHTypeQuote {} -> True
    Syn.ETHPatQuote {} -> True
    Syn.ETHNameQuote {} -> True
    Syn.ETHTypeNameQuote {} -> True
    _ -> False

-- | Template Haskell is not supported. A quote compiles to a call of
-- @raise#@ with a message, so code that only defines quotes still
-- compiles.
desugarTemplateHaskellQuote :: TcAnnotation -> ValueM Expr
desugarTemplateHaskellQuote annotation = do
  let resultType = tcAnnType annotation
  convertedResult <- convertCheckedType resultType
  representation <- checkedRuntimeRep resultType
  raiseName <- primitiveName "GHC.Prim" "raise#" SortValue
  listName <- primitiveName "GHC.Types" "[]" SortTypeConstructor
  charName <- primitiveName "GHC.Types" "Char" SortTypeConstructor
  message <- desugarStringValue "TH is unsupported"
  let stringType = TyApp (TyCon listName) (TyCon charName)
      raise = foldl ExTyApp (ExVar raiseName) [representation, stringType, convertedResult]
  pure (ExApp raise message)

desugarString :: TcAnnotation -> Text -> ValueM Expr
desugarString annotation value = do
  elementType <-
    case tcAnnType annotation of
      TcTyCon tyCon [ty]
        | tyConName tyCon == "[]" -> pure ty
      ty -> failValue ("string literal has non-list type " <> show ty)
  convertedType <- convertCheckedType elementType
  charConstructor <- boxedCharConstructor
  representation <- convertRuntimeRep WordRep
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) convertedType
      cons = ExTyApp (ExVar consName) convertedType
      boxedChar character = ExApp (ExVar charConstructor) (ExLit (LitChar representation character))
  pure (foldr (ExApp . ExApp cons . boxedChar) nil (T.unpack value))

desugarTuple :: TcAnnotation -> Syn.TupleFlavor -> [Maybe Syn.Expr] -> ValueM Expr
desugarTuple annotation flavor elements = do
  let elementTypes = tcAnnTypeArgs annotation
  unless (length elementTypes == length elements) $
    failValue ("tuple annotation has " <> show (length elementTypes) <> " element types for " <> show (length elements) <> " fields")
  convertedTypes <- mapM convertCheckedType elementTypes
  convertedElements <- zipWithM desugarTupleElement elementTypes elements
  representationTypes <-
    case flavor of
      Syn.Boxed -> pure []
      Syn.Unboxed -> mapM checkedRuntimeRep elementTypes
  let arity = length elements
  constructorName <- tupleConstructorName annotation flavor arity
  let constructor = ExVar constructorName
      applied = foldl ExApp (foldl ExTyApp constructor (representationTypes <> convertedTypes)) (map fst convertedElements)
  pure (foldr ExLam applied (concatMap snd convertedElements))

checkedRuntimeRep :: TcType -> ValueM Type
checkedRuntimeRep ty = do
  kindEnv <- gets (ceKindEnv . vsConvertEnv)
  liftEither (runtimeRepOfTypeInEnv kindEnv ty) >>= convertRuntimeRep

desugarTupleElement :: TcType -> Maybe Syn.Expr -> ValueM (Expr, [Binder])
desugarTupleElement _ (Just expression) = (,[]) <$> desugarExpr expression
desugarTupleElement ty Nothing = do
  binder <- freshBinder "_tuple_section" ty
  pure (ExVar (binderName binder), [binder])

tupleConstructorName :: TcAnnotation -> Syn.TupleFlavor -> Int -> ValueM Name
tupleConstructorName annotation flavor arity = do
  primPackage <- gets (cePrimPackage . vsConvertEnv)
  pure (Name constructorText SortDataConstructor (origin primPackage))
  where
    constructorText =
      case flavor of
        Syn.Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
        Syn.Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"
    origin primPackage =
      case sectionResultType (tcAnnType annotation) of
        TcTyCon tyCon _ -> OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)
        _ ->
          case flavor of
            Syn.Boxed -> OriginTop (PackageId "") "GHC.Tuple"
            Syn.Unboxed -> OriginTop primPackage "GHC.Types"
    -- A tuple section has a function type whose result is the tuple.
    sectionResultType ty =
      case ty of
        TcFunTy _ result -> sectionResultType result
        _ -> ty

desugarDo :: [Syn.DoStmt Syn.Expr] -> ValueM Expr
desugarDo statements =
  case statements of
    [] -> failValue "do block has no statements"
    [statement] ->
      case peelDoStatement statement of
        Syn.DoExpr body -> desugarExpr body
        other -> failValue ("invalid final do statement: " <> take 80 (show other))
    statement : rest ->
      case peelDoStatement statement of
        Syn.DoLetDecls declarations -> do
          desugarLocalDecls declarations (doResultType rest) (desugarDo rest)
        Syn.DoBind pattern' action -> do
          (annotation, resolution) <- requiredDoBindOccurrence statement
          bind <- desugarResolvedOccurrence annotation resolution
          action' <- desugarExpr action
          continuation <- desugarDoPatternContinuation annotation pattern' rest
          pure (ExApp (ExApp bind action') continuation)
        Syn.DoExpr action -> do
          (annotation, resolution) <- requiredDoBindOccurrence statement
          method <- desugarResolvedOccurrence annotation resolution
          action' <- desugarExpr action
          continuation <- desugarDo rest
          pure (ExApp (ExApp method action') continuation)
        other -> failValue ("unsupported do statement: " <> take 80 (show other))

doResultType :: [Syn.DoStmt Syn.Expr] -> ValueM TcType
doResultType statements =
  case reverse statements of
    statement : _ ->
      case peelDoStatement statement of
        Syn.DoExpr body -> requiredExprType body
        other -> failValue ("invalid final do statement: " <> take 80 (show other))
    [] -> failValue "do block has no statements"

desugarDoPatternContinuation :: TcAnnotation -> Syn.Pattern -> [Syn.DoStmt Syn.Expr] -> ValueM Expr
desugarDoPatternContinuation annotation pattern' rest = do
  ty <- requiredPatternType pattern'
  binder <- freshPatternBinder pattern' ty
  locals <- directPatternBindings pattern' binder ty
  case locals of
    Just bindings -> ExLam binder <$> withLocals bindings (desugarDo rest)
    Nothing -> do
      resultType <- doBindResultType annotation
      body <- desugarDoPattern resultType binder ty pattern' (desugarDo rest)
      pure (ExLam binder body)

desugarDoPattern :: TcType -> Binder -> TcType -> Syn.Pattern -> ValueM Expr -> ValueM Expr
desugarDoPattern resultType binder ty pattern' success =
  desugarPatternWithFailure resultType binder ty pattern' success Nothing

-- | Match one binder against a pattern. The failure expression, when given,
-- is the default alternative of each constructor case.
desugarPatternWithFailure :: TcType -> Binder -> TcType -> Syn.Pattern -> ValueM Expr -> Maybe Expr -> ValueM Expr
desugarPatternWithFailure resultType binder ty pattern' success failure =
  case pattern' of
    _
      | isOverloadedLiteralPattern pattern' -> do
          -- An overloaded literal compares with the equality
          -- method of its type, as in a function equation.
          test <- desugarOverloadedLiteralPatternTest (ExVar (binderName binder)) pattern'
          testType <- requiredPatternMethodResultType "==" pattern'
          testBinder <- freshBinder "_literal_guard" testType
          resultType' <- convertCheckedType resultType
          trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
          falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
          success' <- success
          failure' <-
            case failure of
              Just failureExpression -> pure failureExpression
              Nothing -> do
                failureBinder <- freshBinderFromType "_literal_nomatch" (binderType binder)
                pure (ExCase (ExVar (binderName binder)) failureBinder resultType' [])
          pure
            ( ExCase
                test
                testBinder
                resultType'
                [ Alt (AltData trueName) [] [] success',
                  Alt (AltData falseName) [] [] failure'
                ]
            )
    Syn.PAnn _ inner -> desugarPatternWithFailure resultType binder ty inner success failure
    Syn.PParen inner -> desugarPatternWithFailure resultType binder ty inner success failure
    Syn.PStrict inner -> desugarPatternWithFailure resultType binder ty inner success failure
    Syn.PIrrefutable inner -> desugarPatternWithFailure resultType binder ty inner success failure
    Syn.PTypeSig inner _ -> desugarPatternWithFailure resultType binder ty inner success failure
    Syn.PVar name -> do
      locals <- binderEntry name binder ty
      withLocals locals success
    Syn.PWildcard -> success
    Syn.PAs name inner -> do
      locals <- binderEntry name binder ty
      withLocals locals (desugarPatternWithFailure resultType binder ty inner success failure)
    Syn.PView viewFunction inner -> do
      function <- desugarExpr viewFunction
      innerType <- requiredPatternType inner
      viewBinder <- freshPatternBinder inner innerType
      body <- desugarPatternWithFailure resultType viewBinder innerType inner success failure
      pure (ExLet (Bind viewBinder (ExApp function (ExVar (binderName binder)))) body)
    _ -> do
      -- The wrappers above are peeled off, so the pattern can have lost
      -- its checked type. A list pattern needs it for its synthesized
      -- tail.
      let typed =
            case patternType pattern' of
              Just _ -> pattern'
              Nothing -> Syn.PAnn (Syn.mkAnnotation (TcAnnotation ty [] [] [] [] [])) pattern'
      maybePatSyn <- patternPatSyn typed
      case maybePatSyn of
        Just (info, annotation) -> desugarPatSynWithFailure resultType binder typed info annotation success failure
        Nothing -> desugarDoConstructorPattern resultType binder typed success failure

desugarDoConstructorPattern :: TcType -> Binder -> Syn.Pattern -> ValueM Expr -> Maybe Expr -> ValueM Expr
desugarDoConstructorPattern resultType binder pattern' success failure = do
  maybeNewtype <- doPatternNewtype pattern'
  case maybeNewtype of
    Just dataType -> desugarDoNewtypePattern resultType binder pattern' dataType success failure
    Nothing -> do
      let children = patternChildren pattern'
          predicates = patternGivenPredicates pattern'
      let typeVariables = patternTypeVariables pattern'
      typeBinders <- convertTypeBinders typeVariables
      fieldTypes <- patternFieldTypes pattern' children
      fields <- zipWithM freshPatternBinder children fieldTypes
      dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] predicates
      constructor <- patternConstructor pattern'
      resultType' <- convertCheckedType resultType
      caseBinder <- freshBinderFromType "_do_scrut" (binderType binder)
      body <-
        withAlternativeScope
          (not (null typeBinders))
          (zipWith Dictionary predicates dictionaries)
          (desugarDoChildPatterns resultType (zip3 fields fieldTypes children) success failure)
      let defaultAlternatives = [Alt AltDefault [] [] failureExpression | Just failureExpression <- [failure]]
      pure (ExCase (ExVar (binderName binder)) caseBinder resultType' (Alt constructor typeBinders (dictionaries <> fields) body : defaultAlternatives))

desugarDoChildPatterns :: TcType -> [(Binder, TcType, Syn.Pattern)] -> ValueM Expr -> Maybe Expr -> ValueM Expr
desugarDoChildPatterns resultType children success failure =
  case children of
    [] -> success
    (binder, ty, pattern') : rest ->
      desugarPatternWithFailure resultType binder ty pattern' (desugarDoChildPatterns resultType rest success failure) failure

doPatternNewtype :: Syn.Pattern -> ValueM (Maybe DataTypeInfo)
doPatternNewtype pattern' = do
  newtypes <- gets vsNewtypeConstructors
  pure $ do
    name <- patternConstructorSourceName pattern'
    key <- nameTermKey name
    Map.lookup key newtypes

desugarDoNewtypePattern :: TcType -> Binder -> Syn.Pattern -> DataTypeInfo -> ValueM Expr -> Maybe Expr -> ValueM Expr
desugarDoNewtypePattern resultType binder pattern' dataType success failure = do
  child <-
    case patternChildren pattern' of
      [fieldPattern] -> pure fieldPattern
      _ -> failValue ("newtype do pattern does not have one field: " <> T.unpack (dtiName dataType))
  childType <- requiredPatternType child
  field <- freshPatternBinder child childType
  typeArguments <- newtypePatternArguments pattern'
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  let tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      unwrapped = ExCast (ExVar (binderName binder)) (CoAxiom axiom convertedArguments)
  body <- desugarPatternWithFailure resultType field childType child success failure
  pure (ExLet (Bind field unwrapped) body)

directPatternBindings :: Syn.Pattern -> Binder -> TcType -> ValueM (Maybe [(TcTermKey, (Binder, TcType))])
directPatternBindings pattern' binder ty =
  case pattern' of
    Syn.PAnn _ inner -> directPatternBindings inner binder ty
    Syn.PParen inner -> directPatternBindings inner binder ty
    Syn.PStrict inner -> directPatternBindings inner binder ty
    Syn.PIrrefutable inner -> directPatternBindings inner binder ty
    Syn.PTypeSig inner _ -> directPatternBindings inner binder ty
    Syn.PVar name -> Just <$> binderEntry name binder ty
    Syn.PWildcard -> pure (Just [])
    Syn.PAs name inner -> do
      outer <- binderEntry name binder ty
      innerResult <- directPatternBindings inner binder ty
      pure ((outer <>) <$> innerResult)
    _ -> pure Nothing

peelDoStatement :: Syn.DoStmt body -> Syn.DoStmt body
peelDoStatement statement =
  case statement of
    Syn.DoAnn _ inner -> peelDoStatement inner
    _ -> statement

requiredDoBindOccurrence :: Syn.DoStmt Syn.Expr -> ValueM (TcAnnotation, ResolutionAnnotation)
requiredDoBindOccurrence statement =
  case doBindOccurrence statement of
    Just occurrence -> pure occurrence
    Nothing -> failValue ("missing checked do method occurrence: " <> take 80 (show statement))

doBindOccurrence :: Syn.DoStmt Syn.Expr -> Maybe (TcAnnotation, ResolutionAnnotation)
doBindOccurrence = go Nothing Nothing
  where
    go maybeAnnotation maybeResolution statement =
      case statement of
        Syn.DoAnn annotation inner ->
          go
            ((Syn.fromAnnotation annotation :: Maybe TcAnnotation) <|> maybeAnnotation)
            ((Syn.fromAnnotation annotation :: Maybe ResolutionAnnotation) <|> maybeResolution)
            inner
        _ -> (,) <$> maybeAnnotation <*> maybeResolution

-- | The type of the continuation body of a checked @>>=@ method.
doBindResultType :: TcAnnotation -> ValueM TcType
doBindResultType annotation =
  case tcAnnType annotation of
    TcFunTy _ (TcFunTy (TcFunTy _ resultType) _) -> pure resultType
    ty -> failValue ("invalid checked >>= result type: " <> show ty)

desugarCase :: TcType -> Syn.Expr -> [Syn.CaseAlt Syn.Expr] -> ValueM Expr
desugarCase resultType scrutinee alternatives = do
  scrutinee' <- desugarExpr scrutinee
  scrutineeType <- requiredExprType scrutinee
  convertedType <- convertCheckedType scrutineeType
  case alternatives of
    [] -> do
      binder <- freshBinder "_case" scrutineeType
      resultType' <- convertCheckedType resultType
      pure (ExCase scrutinee' binder resultType' [])
    _ -> do
      let matches = map caseAlternativeMatch alternatives
      case scrutinee' of
        ExVar name -> desugarMatchArguments resultType Nothing [Binder name convertedType] [scrutineeType] (map emptyMatchWork matches)
        _ -> do
          binder <- freshBinder "_case" scrutineeType
          body <- desugarMatchArguments resultType Nothing [binder] [scrutineeType] (map emptyMatchWork matches)
          pure (ExLet (Bind binder scrutinee') body)

caseAlternativeMatch :: Syn.CaseAlt Syn.Expr -> Syn.Match
caseAlternativeMatch alternative =
  case alternative of
    Syn.CaseAlt annotations pattern' rhs ->
      (emptyMatch rhs)
        { Syn.matchAnns = annotations,
          Syn.matchPats = [pattern']
        }

lambdaCaseAltMatch :: Syn.LambdaCaseAlt -> Syn.Match
lambdaCaseAltMatch alternative =
  (emptyMatch (Syn.lambdaCaseAltRhs alternative))
    { Syn.matchAnns = Syn.lambdaCaseAltAnns alternative,
      Syn.matchPats = Syn.lambdaCaseAltPats alternative
    }

desugarLocalDecls :: [Syn.Decl] -> ValueM TcType -> ValueM Expr -> ValueM Expr
desugarLocalDecls declarations bodyType body = do
  groups <- groupLocalValues declarations
  allocated <- mapM allocateLocal groups
  if all isImplicitParamAllocation allocated && not (null allocated)
    then desugarImplicitParamDecls allocated body
    else withLocals (concatMap allocationLocals allocated) $ do
      resultType <- bodyType
      bindGroups <- mapM desugarLocal allocated
      unlifted <- mapM allocationHasUnliftedBinder allocated
      forcedBody <- foldr (forceStrictPattern resultType) body allocated
      components <- liftEither (localBindingComponents (zip bindGroups unlifted))
      foldr wrapComponent (pure forcedBody) components
  where
    isImplicitParamAllocation allocation =
      case allocation of
        LocalImplicitParamAllocation {} -> True
        _ -> False
    wrapComponent component inner = do
      innerExpression <- inner
      case component of
        LocalRecursiveBinds binds -> pure (ExRec binds innerExpression)
        LocalStrictBinds binds -> pure (foldr ExLet innerExpression binds)
    allocationHasUnliftedBinder allocation = do
      kindEnv <- gets (ceKindEnv . vsConvertEnv)
      let types =
            case allocation of
              LocalNamedAllocation _ _ ty _ -> [ty]
              LocalPatternAllocation _ _ _ rhsType binders _ -> rhsType : [ty | (_, _, ty) <- binders]
              LocalImplicitParamAllocation _ _ _ ty -> [ty]
      pure (any (isUnliftedTypeInEnv kindEnv) types)
    allocateLocal (LocalNamedGroup group) = do
      let key = groupKey group
          name = groupName group
          ty = groupType group
      binder <- freshBinder name ty
      pure (LocalNamedAllocation key binder ty group)
    allocateLocal (LocalPatternGroup pattern' rhs rhsType strict) = do
      rhsBinder <- freshBinder "_pat_rhs" rhsType
      specs <- patternBinderSpecs pattern'
      binders <- mapM (\(key, name, ty) -> (key,,ty) <$> freshBinder name ty) specs
      pure (LocalPatternAllocation pattern' rhs rhsBinder rhsType binders strict)
    allocateLocal (LocalImplicitParamGroup name rhs rhsType) = do
      binder <- freshBinder ("$ip" <> T.drop 1 name) rhsType
      pure (LocalImplicitParamAllocation name rhs binder rhsType)
    desugarLocal (LocalNamedAllocation _ binder ty group) = do
      rhs <-
        case group of
          FunctionGroup _ _ matches _ -> desugarMatches ty matches
          PatternGroup _ _ sourceRhs _ -> desugarMatches ty [emptyMatch sourceRhs]
      pure [Bind binder rhs]
    desugarLocal (LocalPatternAllocation pattern' sourceRhs rhsBinder rhsType binders _) = do
      rhs <- desugarMatches rhsType [emptyMatch sourceRhs]
      selectors <- mapM (desugarPatternSelector pattern' rhsBinder rhsType) binders
      pure (Bind rhsBinder rhs : selectors)
    desugarLocal (LocalImplicitParamAllocation name _ _ _) =
      failValue ("implicit parameter binding " <> T.unpack name <> " in a mixed local group")
    desugarPatternSelector pattern' rhsBinder rhsType (key, binder, ty) = do
      selector <- desugarDoPattern ty rhsBinder rhsType pattern' $ do
        (field, _) <- lookupLocal key (nameText (binderName binder))
        pure (ExVar (binderName field))
      pure (Bind binder selector)
    forceStrictPattern resultType allocation success =
      case allocation of
        LocalPatternAllocation pattern' _ rhsBinder rhsType _ True ->
          desugarDoPattern resultType rhsBinder rhsType pattern' success
        _ -> success

-- | One dependency component of a local binding group.
data LocalBindingComponent
  = -- | Lifted bindings that can refer to each other.
    LocalRecursiveBinds [Bind]
  | -- | Bindings with an unlifted binder. They are strict and not recursive.
    LocalStrictBinds [Bind]

-- | Order local bindings by their dependencies. The result lists the
-- bindings that other bindings use first. A binding group with an unlifted
-- binder must not be recursive.
localBindingComponents :: [([Bind], Bool)] -> Either String [LocalBindingComponent]
localBindingComponents groups =
  mapM component (Graph.stronglyConnComp nodes)
  where
    indexed = zip [0 :: Int ..] groups
    definitions =
      Map.fromList
        [ (binderName (bindBinder bind), index)
        | (index, (binds, _)) <- indexed,
          bind <- binds
        ]
    -- A binding group lists its own binders in order, so a reference to an
    -- earlier binder of the same group is not a dependency cycle.
    nodes =
      [ (group, index, filter (/= index) (Set.toList (Set.fromList (concatMap dependencies binds))))
      | (index, group@(binds, _)) <- indexed
      ]
    dependencies bind = mapMaybe (`Map.lookup` definitions) (Set.toList (expressionFreeNames (bindRhs bind)))
    component scc =
      case scc of
        Graph.AcyclicSCC (binds, unlifted)
          | unlifted -> Right (LocalStrictBinds binds)
          | otherwise -> Right (LocalRecursiveBinds binds)
        Graph.CyclicSCC members
          | any snd members -> Left ("System FC does not accept a recursive local binding with an unlifted binder: " <> show [binderName (bindBinder bind) | (binds, _) <- members, bind <- binds])
          | otherwise -> Right (LocalRecursiveBinds (concatMap fst members))

-- | The free value names of a System FC expression.
expressionFreeNames :: Expr -> Set Name
expressionFreeNames expression =
  case expression of
    ExVar name -> Set.singleton name
    ExLit _ -> Set.empty
    ExApp function argument -> expressionFreeNames function <> expressionFreeNames argument
    ExTyApp function _ -> expressionFreeNames function
    ExLam binder inner -> Set.delete (binderName binder) (expressionFreeNames inner)
    ExTyLam _ inner -> expressionFreeNames inner
    ExLet binding inner -> expressionFreeNames (bindRhs binding) <> Set.delete (binderName (bindBinder binding)) (expressionFreeNames inner)
    ExRec bindings inner ->
      let names = Set.fromList (map (binderName . bindBinder) bindings)
       in (foldMap (expressionFreeNames . bindRhs) bindings <> expressionFreeNames inner) `Set.difference` names
    ExCase scrutinee binder _ alternatives ->
      expressionFreeNames scrutinee
        <> Set.delete (binderName binder) (foldMap alternativeFreeNames alternatives)
    ExCast inner _ -> expressionFreeNames inner
    ExForeignCall _ _ arguments -> foldMap expressionFreeNames arguments
  where
    alternativeFreeNames alternative =
      expressionFreeNames (altRhs alternative)
        `Set.difference` Set.fromList (map binderName (altBinders alternative))

allocationLocals :: LocalAllocation -> [(TcTermKey, (Binder, TcType))]
allocationLocals allocation =
  case allocation of
    LocalNamedAllocation key binder ty _ -> [(key, (binder, ty))]
    LocalPatternAllocation _ _ _ _ binders _ -> [(key, (binder, ty)) | (key, binder, ty) <- binders]
    LocalImplicitParamAllocation {} -> []

-- | Desugar a group of implicit-parameter bindings.
--
-- Each right-hand side sees only the enclosing bindings, so the group is a
-- chain of non-recursive lets. The body sees each new binding as the
-- evidence for its implicit parameter.
desugarImplicitParamDecls :: [LocalAllocation] -> ValueM Expr -> ValueM Expr
desugarImplicitParamDecls allocated body = do
  binds <- mapM desugarBinding allocated
  let dictionaries = [Dictionary (IParamPred name ty) binder | LocalImplicitParamAllocation name _ binder ty <- allocated]
  body' <- withDictionaryScope dictionaries body
  pure (foldr ExLet body' binds)
  where
    desugarBinding allocation =
      case allocation of
        LocalImplicitParamAllocation _ sourceRhs binder rhsType -> do
          rhs <- desugarMatches rhsType [emptyMatch sourceRhs]
          pure (Bind binder rhs)
        _ -> failValue "implicit-parameter group contains another binding"

desugarEvidence :: Ev.EvTerm -> ValueM Expr
desugarEvidence evidence =
  case evidence of
    Ev.EvVarTerm variable -> failValue ("unresolved evidence variable: " <> show variable)
    Ev.EvGiven predicate -> do
      dictionaries <- gets vsDictionaries
      case Map.lookup (predicateKey predicate) dictionaries of
        Just binder -> pure (ExVar (binderName binder))
        Nothing -> failValue ("missing given dictionary for " <> show predicate)
    Ev.EvDict origin dictionaryName types subEvidence -> do
      convertedTypes <- mapM convertCheckedType types
      evidenceArguments <- mapM desugarEvidence subEvidence
      let (packageName, moduleName') = origin
          package = PackageId packageName
          name = Name dictionaryName SortValue (OriginTop package moduleName')
      pure (foldl ExApp (foldl ExTyApp (ExVar name) convertedTypes) evidenceArguments)
    Ev.EvCoercion coercion -> ExCast (ExVar (Name "coercion" SortValue (OriginLocal (Unique 0)))) <$> convertCoercion coercion
    Ev.EvSuperClass _ _ _ fieldTypes fieldIndex -> do
      resultPredicateType <-
        case drop fieldIndex fieldTypes of
          fieldType : _ -> pure fieldType
          [] -> failValue "superclass field type index is outside the dictionary layout"
      shareSuperClass evidence resultPredicateType (desugarSuperClass evidence)
    Ev.EvCast inner coercion -> ExCast <$> desugarEvidence inner <*> convertCoercion coercion
    Ev.EvTypeable origin ty arguments -> desugarTypeableEvidence origin ty arguments
    Ev.EvTypeLam variable body ->
      withoutEvidenceScope (ExTyLam <$> convertTypeBinder variable <*> desugarEvidence body)
    Ev.EvDictLam predicate binderType body -> withoutEvidenceScope $ do
      binder <- freshBinder "$quantified_d" binderType
      body' <- withDictionaries [Dictionary predicate binder] (desugarEvidence body)
      pure (ExLam binder body')
    Ev.EvTypeApp function argument ->
      ExTyApp <$> desugarEvidence function <*> convertCheckedType argument
    Ev.EvDictApp function argument ->
      ExApp <$> desugarEvidence function <*> desugarEvidence argument
    Ev.EvCallStackPush (packageName, moduleName') function site parent -> desugarCallStackPush (packageName, moduleName') function site parent
    Ev.EvCallStackEmpty origin -> desugarCallStackEmpty origin

-- | Build the case chain that selects one superclass field.
desugarSuperClass :: Ev.EvTerm -> ValueM Expr
desugarSuperClass evidence =
  case evidence of
    Ev.EvSuperClass source _ sourcePredicate fieldTypes fieldIndex -> do
      sourceExpression <- desugarEvidence source
      (classTyCon, sourceType) <-
        case sourcePredicate of
          ClassPred classTyCon arguments -> pure (classTyCon, TcTyCon classTyCon arguments)
          EqPred {} -> failValue "cannot select a superclass from equality evidence"
          QuantifiedPred {} -> failValue "cannot select a superclass from quantified evidence before application"
          IParamPred {} -> failValue "cannot select a superclass from implicit-parameter evidence"
      sourceBinder <- freshBinder "$super_source" sourceType
      fieldBinders <- zipWithM (freshIndexedBinder "$super_field") [0 :: Int ..] fieldTypes
      selected <-
        case drop fieldIndex fieldBinders of
          field : _ -> pure field
          [] -> failValue "superclass field index is outside the dictionary layout"
      resultType <-
        case drop fieldIndex fieldTypes of
          fieldType : _ -> convertCheckedType fieldType
          [] -> failValue "superclass field type index is outside the dictionary layout"
      pure
        ( ExCase
            sourceExpression
            sourceBinder
            resultType
            [Alt (AltData (classDictConName classTyCon)) [] fieldBinders (ExVar (binderName selected))]
        )
    _ -> failValue "superclass projection expects superclass evidence"

-- | One entry of the call stack of an occurrence with @HasCallStack@.
desugarCallStackPush :: (Text, Text) -> Text -> Ev.CallSite -> Ev.EvTerm -> ValueM Expr
desugarCallStackPush (packageName, moduleName') function site parent = do
  parent' <- desugarEvidence parent
  (currentPackage, currentModule) <- gets vsModuleOrigin
  functionText <- desugarStringValue function
  packageText <- desugarStringValue (packageIdText currentPackage)
  moduleText <- desugarStringValue currentModule
  fileText <- desugarStringValue (Ev.callSiteFile site)
  intRepresentation <- convertRuntimeRep IntRep
  intConstructor <- primitiveName "GHC.Types" "I#" SortDataConstructor
  charName <- primitiveName "GHC.Types" "Char" SortTypeConstructor
  listName <- primitiveName "GHC.Types" "[]" SortTypeConstructor
  pairConstructor <- primitiveName "GHC.Tuple" "(,)" SortDataConstructor
  let libraryName name sort = Name name sort (OriginTop (PackageId packageName) moduleName')
      boxedInt value = ExApp (ExVar intConstructor) (ExLit (LitInt intRepresentation (toInteger value)))
      stringType = TyApp (TyCon listName) (TyCon charName)
      locationType = TyCon (libraryName "SrcLoc" SortTypeConstructor)
      -- GHC's pushCallStack takes the call site as a pair.
      callSite srcLoc =
        foldl ExApp (ExTyApp (ExTyApp (ExVar pairConstructor) stringType) locationType) [functionText, srcLoc]
      location =
        foldl
          ExApp
          (ExVar (libraryName "SrcLoc" SortDataConstructor))
          [ packageText,
            moduleText,
            fileText,
            boxedInt (Ev.callSiteStartLine site),
            boxedInt (Ev.callSiteStartColumn site),
            boxedInt (Ev.callSiteEndLine site),
            boxedInt (Ev.callSiteEndColumn site)
          ]
  pure (foldl ExApp (ExVar (libraryName "pushCallStack" SortValue)) [callSite location, parent'])

-- | The call stack of an occurrence that starts a new stack.
desugarCallStackEmpty :: (Text, Text) -> ValueM Expr
desugarCallStackEmpty (packageName, moduleName') =
  pure (ExVar (Name "emptyCallStack" SortValue (OriginTop (PackageId packageName) moduleName')))

-- | A boxed string literal for compiler-generated code.
desugarStringValue :: Text -> ValueM Expr
desugarStringValue value = do
  charName <- primitiveName "GHC.Types" "Char" SortTypeConstructor
  charConstructor <- boxedCharConstructor
  representation <- convertRuntimeRep WordRep
  desugarFcList
    (TyCon charName)
    [ExApp (ExVar charConstructor) (ExLit (LitChar representation character)) | character <- T.unpack value]

desugarTypeableEvidence :: Maybe (Text, Text) -> TcType -> [Ev.EvTerm] -> ValueM Expr
desugarTypeableEvidence origin ty argumentEvidence = do
  (_, argumentTypes) <- typeableTypeView ty
  unless (length argumentTypes == length argumentEvidence) (failValue "Typeable evidence argument count does not match its type")
  argumentRepresentations <- zipWithM (desugarTypeableArgument origin) argumentTypes argumentEvidence
  representation <- desugarTypeRepresentation origin ty argumentRepresentations
  convertedType <- convertCheckedType ty
  proxyName <- typeableName origin "Data.Proxy" "Proxy" SortTypeConstructor
  let proxyType = TyApp (TyCon proxyName) convertedType
  proxyBinder <- freshBinderFromType "$typeable_proxy" proxyType
  valueBinder <- freshBinderFromType "$typeable_value" convertedType
  dictionaryConstructor <- typeableName origin "Type.Reflection" "$Dict$Typeable" SortDataConstructor
  pure
    ( ExApp
        (ExApp (ExTyApp (ExVar dictionaryConstructor) convertedType) (ExLam proxyBinder representation))
        (ExLam valueBinder representation)
    )

desugarTypeableArgument :: Maybe (Text, Text) -> TcType -> Ev.EvTerm -> ValueM Expr
desugarTypeableArgument origin ty evidence = do
  dictionary <- desugarEvidence evidence
  convertedType <- convertCheckedType ty
  selector <- typeableName origin "Type.Reflection" "typeRep" SortValue
  someTypeRepConstructor <- typeableName origin "Type.Reflection" "SomeTypeRep" SortDataConstructor
  proxyConstructor <- typeableName origin "Data.Proxy" "Proxy" SortDataConstructor
  let proxy = ExTyApp (ExVar proxyConstructor) convertedType
      typeRepValue = ExApp (ExApp (ExTyApp (ExVar selector) convertedType) dictionary) proxy
  pure (ExApp (ExTyApp (ExVar someTypeRepConstructor) convertedType) typeRepValue)

desugarTypeRepresentation :: Maybe (Text, Text) -> TcType -> [Expr] -> ValueM Expr
desugarTypeRepresentation origin ty arguments = do
  (typeName, _) <- typeableTypeView ty
  convertedType <- convertCheckedType ty
  someTypeRepName <- typeableName origin "Type.Reflection" "SomeTypeRep" SortTypeConstructor
  typeRepConstructor <- typeableName origin "Type.Reflection" "TypeRep" SortDataConstructor
  tyConAxiom <- typeableName origin "Type.Reflection" "$ax$TyCon" SortAxiom
  charName <- primitiveName "GHC.Types" "Char" SortTypeConstructor
  charConstructor <- boxedCharConstructor
  wordRep <- convertRuntimeRep WordRep
  let someTypeRepType = TyCon someTypeRepName
      charType = TyCon charName
      typeNameChars =
        [ ExApp (ExVar charConstructor) (ExLit (LitChar wordRep character))
        | character <- T.unpack typeName
        ]
  nameList <- desugarFcList charType typeNameChars
  argumentList <- desugarFcList someTypeRepType arguments
  let tyCon = ExCast nameList (CoSym (CoAxiom tyConAxiom []))
  pure (ExApp (ExApp (ExTyApp (ExVar typeRepConstructor) convertedType) tyCon) argumentList)

desugarFcList :: Type -> [Expr] -> ValueM Expr
desugarFcList elementType elements = do
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) elementType
      cons item = ExApp (ExApp (ExTyApp (ExVar consName) elementType) item)
  pure (foldr cons nil elements)

typeableName :: Maybe (Text, Text) -> Text -> Text -> Sort -> ValueM Name
typeableName origin fallbackModule name sort =
  case origin of
    Just (packageName, moduleName') ->
      let selectedModule = if fallbackModule == "Type.Reflection" then moduleName' else fallbackModule
       in pure (Name name sort (OriginTop (PackageId packageName) selectedModule))
    Nothing -> failValue ("Typeable evidence has no origin for " <> T.unpack name)

typeableTypeView :: TcType -> ValueM (Text, [TcType])
typeableTypeView ty =
  case ty of
    TcTyCon tyCon arguments -> pure (tyConName tyCon, arguments)
    TcFunTy argument result -> pure ("(->)", [argument, result])
    _ -> failValue ("cannot construct Typeable evidence for " <> show ty)

desugarResolvedOccurrence :: TcAnnotation -> ResolutionAnnotation -> ValueM Expr
desugarResolvedOccurrence annotation resolution = do
  name <- resolvedAnnotationName resolution
  types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
  evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
  desugarTermReference name types evidence (seqTermArgumentTypes annotation)

resolvedAnnotationName :: ResolutionAnnotation -> ValueM Name
resolvedAnnotationName resolution =
  case resolutionTarget resolution of
    ResolvedTopLevel package target ->
      pure
        ( Name
            (Syn.nameText target)
            (sourceNameSort target)
            (OriginTop package (fromMaybe "" (Syn.nameQualifier target)))
        )
    ResolvedLocal unique localName ->
      binderName . fst <$> lookupLocal (TcTermLocal unique) (Syn.unqualifiedNameText localName)
    ResolvedSyntax -> failValue ("syntax identifier reached ordinary occurrence " <> T.unpack (displayIdentifier (resolutionIdentifier resolution)))
    ResolvedError message -> failValue message

desugarOverloadedInteger :: TcAnnotation -> ResolutionAnnotation -> Integer -> ValueM Expr
desugarOverloadedInteger annotation resolution value = do
  fromIntegerExpression <- desugarResolvedOccurrence annotation resolution
  integer <- desugarIntegerLiteral value
  pure (ExApp fromIntegerExpression integer)

-- | An overloaded fractional literal applies fromRational to a Rational.
--
-- The Rational is the ratio of the numerator and the denominator of the
-- literal. Both are Integer literals.
desugarOverloadedRational :: TcAnnotation -> ResolutionAnnotation -> Rational -> ValueM Expr
desugarOverloadedRational annotation resolution value = do
  fromRationalExpression <- desugarResolvedOccurrence annotation resolution
  rational <- desugarRationalLiteral value
  pure (ExApp fromRationalExpression rational)

desugarRationalLiteral :: Rational -> ValueM Expr
desugarRationalLiteral value = do
  constructor <- primitiveName "GHC.Prim.Real" "Ratio" SortDataConstructor
  package <- gets (cePrimPackage . vsConvertEnv)
  integerType <- convertCheckedType (TcTyCon (mkTyConWithOrigin package "GHC.Prim.Integer" "Integer" 0) [])
  numeratorExpression <- desugarIntegerLiteral (numerator value)
  denominatorExpression <- desugarIntegerLiteral (denominator value)
  pure (ExApp (ExApp (ExTyApp (ExVar constructor) integerType) numeratorExpression) denominatorExpression)

desugarIntegerLiteral :: Integer -> ValueM Expr
desugarIntegerLiteral value = do
  constructor <- primitiveName "GHC.Prim.Integer" "IS" SortDataConstructor
  intRepresentation <- convertRuntimeRep IntRep
  wordRepresentation <- convertRuntimeRep WordRep
  let small integer = ExApp (ExVar constructor) (ExLit (LitInt intRepresentation integer))
      coreName text = Name text SortValue (nameOrigin constructor)
      apply name = foldl ExApp (ExVar (coreName name))
      word integer = ExLit (LitInt wordRepresentation integer)
      positive integer
        | integer <= maxInt = small integer
        | integer <= maxWord =
            apply "integerFromTwoWords#" [ExLit (LitInt intRepresentation 1), word 0, word integer]
        | otherwise =
            let (high, low) = integer `quotRem` wordBase
                shifted = apply "integerShiftL#" [positive high, ExLit (LitInt intRepresentation 64)]
             in apply "integerAdd" [shifted, positive low]
      magnitude = positive (abs value)
  pure
    ( if value >= minInt && value <= maxInt
        then small value
        else if value < 0 then apply "integerNegate" [magnitude] else magnitude
    )
  where
    wordBase = 18446744073709551616
    maxWord = wordBase - 1
    maxInt = 9223372036854775807
    minInt = -9223372036854775808

convertCoercion :: Ev.Coercion -> ValueM Coercion
convertCoercion coercion =
  case coercion of
    Ev.CoVar (Ev.EvVar unique) -> pure (CoVar (Name "c" SortValue (OriginLocal unique)))
    Ev.Refl ty -> CoRefl <$> convertCheckedType ty
    Ev.Sym inner -> CoSym <$> convertCoercion inner
    Ev.Trans left right -> CoTrans <$> convertCoercion left <*> convertCoercion right
    Ev.TyConAppCo tyCon arguments -> do
      env <- gets vsConvertEnv
      CoTyConApp (tyConNameFc env tyCon) <$> mapM convertCoercion arguments
    Ev.AxiomInstCo key arguments ->
      CoAxiom (lookupAxiomName key) <$> mapM convertCheckedType arguments

resolvedTermName :: Syn.Name -> ValueM Name
resolvedTermName sourceName =
  case termResolution sourceName of
    Just resolution ->
      case resolutionTarget resolution of
        ResolvedTopLevel package target ->
          pure
            ( Name
                (Syn.nameText target)
                (sourceNameSort target)
                (OriginTop package (fromMaybe "" (Syn.nameQualifier target)))
            )
        ResolvedSyntax -> failValue ("syntax identifier reached ordinary term " <> T.unpack (Syn.nameText sourceName))
        ResolvedLocal unique localName ->
          binderName . fst <$> lookupLocal (TcTermLocal unique) (Syn.unqualifiedNameText localName)
        ResolvedError message -> failValue message
    Nothing -> failValue ("missing resolved value " <> T.unpack (Syn.nameText sourceName))

termResolution :: Syn.Name -> Maybe ResolutionAnnotation
termResolution sourceName =
  listToMaybe
    [ resolution
    | resolution <- mapMaybe Syn.fromAnnotation (Syn.nameAnns sourceName),
      resolutionNamespace resolution == ResolutionNamespaceTerm
    ]

sourceNameSort :: Syn.Name -> Sort
sourceNameSort sourceName =
  case T.uncons (Syn.nameText sourceName) of
    Just (first, _)
      | first == ':' || first == '[' || first == '(' || isAsciiUpper first -> SortDataConstructor
    _ -> SortValue

topName :: (PackageId, Text) -> Text -> Name
topName (package, moduleName') name = Name name SortValue (OriginTop package moduleName')

primitiveName :: Text -> Text -> Sort -> ValueM Name
primitiveName moduleName' name sort = do
  package <- gets (cePrimPackage . vsConvertEnv)
  pure (Name name sort (OriginTop package moduleName'))

boxedCharConstructor :: ValueM Name
boxedCharConstructor = primitiveName "GHC.Types" "C#" SortDataConstructor

boxedCharFieldType :: ValueM TcType
boxedCharFieldType = do
  package <- gets (cePrimPackage . vsConvertEnv)
  pure (TcTyCon (mkTyConWithOrigin package "GHC.Prim" "Char#" 0) [])

freshArgument :: Int -> TcType -> ValueM Binder
freshArgument index = freshBinder (argumentName index)

argumentName :: Int -> Text
argumentName index
  | index < 3 = T.singleton (['x', 'y', 'z'] !! index)
  | otherwise = "x" <> T.pack (show (index - 2))

freshIndexedBinder :: Text -> Int -> TcType -> ValueM Binder
freshIndexedBinder prefix index = freshBinder (prefix <> T.pack (show index))

freshDictionaryBinder :: Text -> Int -> Pred -> ValueM Binder
freshDictionaryBinder prefix index predicate = do
  unique <- freshUnique
  env <- gets vsConvertEnv
  ty <- liftEither (convertPred env predicate)
  pure (Binder (Name (prefix <> T.pack (show index)) SortValue (OriginLocal unique)) ty)

freshBinder :: Text -> TcType -> ValueM Binder
freshBinder name ty = do
  unique <- freshUnique
  converted <- convertCheckedType ty
  pure (Binder (Name name SortValue (OriginLocal unique)) converted)

freshBinderFromType :: Text -> Type -> ValueM Binder
freshBinderFromType name ty = do
  unique <- freshUnique
  pure (Binder (Name name SortValue (OriginLocal unique)) ty)

freshUnique :: ValueM Unique
freshUnique = do
  next <- gets vsNextUnique
  modify' (\state -> state {vsNextUnique = next + 1})
  pure (Unique next)

requiredBinderKey :: Syn.UnqualifiedName -> ValueM TcTermKey
requiredBinderKey name =
  maybe (failValue ("missing resolved binder " <> T.unpack (Syn.unqualifiedNameText name))) pure (binderTermKey name)

-- | Whether an expression is an application, whose type the type checker
-- can instantiate at an enclosing application.
isApplicationExpression :: Syn.Expr -> Bool
isApplicationExpression expression =
  case expression of
    Syn.EAnn _ inner -> isApplicationExpression inner
    Syn.EParen inner -> isApplicationExpression inner
    Syn.EApp {} -> True
    Syn.EInfix {} -> True
    _ -> False

-- | Whether an operator is the application operator of GHC.Base, which the
-- type checker checks like an application.
isApplicationOperator :: Syn.Name -> Bool
isApplicationOperator operator =
  Syn.nameText operator == "$"
    && case nameTermKey operator of
      Just (TcTermGlobal _ moduleName' "$") -> moduleName' == "GHC.Base"
      _ -> False

requiredNameTermKey :: Syn.Name -> ValueM TcTermKey
requiredNameTermKey sourceName =
  maybe (failValue ("missing resolved term " <> T.unpack (Syn.nameText sourceName))) pure (nameTermKey sourceName)

binderTermKey :: Syn.UnqualifiedName -> Maybe TcTermKey
binderTermKey name = do
  resolution <- unqualifiedTermResolution name
  resolutionTermKey (Syn.unqualifiedNameText name) resolution

nameTermKey :: Syn.Name -> Maybe TcTermKey
nameTermKey sourceName = do
  resolution <- termResolution sourceName
  resolutionTermKey (Syn.nameText sourceName) resolution

unqualifiedTermResolution :: Syn.UnqualifiedName -> Maybe ResolutionAnnotation
unqualifiedTermResolution name =
  listToMaybe
    [ resolution
    | resolution <- mapMaybe Syn.fromAnnotation (Syn.unqualifiedNameAnns name),
      resolutionNamespace resolution == ResolutionNamespaceTerm
    ]

resolutionTermKey :: Text -> ResolutionAnnotation -> Maybe TcTermKey
resolutionTermKey displayName resolution =
  case resolutionTarget resolution of
    ResolvedLocal unique _ -> Just (TcTermLocal unique)
    ResolvedTopLevel package target ->
      Just (TcTermGlobal package (fromMaybe "" (Syn.nameQualifier target)) (Syn.nameText target))
    ResolvedSyntax -> Just (TcTermGlobal (PackageId "") "" displayName)
    ResolvedError _ -> Nothing

lookupLocal :: TcTermKey -> Text -> ValueM (Binder, TcType)
lookupLocal key displayName = do
  local <- Map.lookup key <$> gets vsLocals
  case local of
    Just entry -> pure entry
    Nothing -> failValue ("missing local value " <> T.unpack displayName)

isPrimitiveLiteral :: Syn.Expr -> Bool
isPrimitiveLiteral expression =
  case expression of
    Syn.EInt _ numericType _ -> numericType /= Syn.TInteger
    Syn.ECharHash {} -> True
    Syn.EStringHash {} -> True
    _ -> False

lookupBindingType :: TcTermKey -> ValueM TcType
lookupBindingType key = do
  local <- Map.lookup key <$> gets vsLocals
  case local of
    Just (_, ty) -> pure ty
    Nothing -> do
      types <- gets vsBindingTypes
      case Map.lookup key types of
        Just ty -> pure ty
        Nothing -> failValue ("missing checked type for " <> show key)

requiredExprType :: Syn.Expr -> ValueM TcType
requiredExprType expression =
  case exprType expression of
    Just ty -> pure ty
    Nothing -> inferExprType expression

inferExprType :: Syn.Expr -> ValueM TcType
inferExprType expression =
  case expression of
    Syn.EAnn _ inner -> inferExprType inner
    Syn.EVar name -> lookupBindingType =<< requiredNameTermKey name
    Syn.EApp function _ -> do
      functionType <- inferExprType function
      case applicationResultType functionType of
        Just result -> pure result
        Nothing -> failValue ("application head is not a checked function: " <> show functionType)
    Syn.EInfix left operator _
      | isApplicationOperator operator -> do
          functionType <- inferExprType left
          case applicationResultType functionType of
            Just result -> pure result
            Nothing -> failValue ("application operator head is not a checked function: " <> show functionType)
    Syn.EInfix _ operator _ -> do
      operatorType <-
        maybe
          (lookupBindingType =<< requiredNameTermKey operator)
          (pure . tcAnnType)
          (listToMaybe (mapMaybe Syn.fromAnnotation (Syn.nameAnns operator)))
      case applicationResultType operatorType >>= applicationResultType of
        Just result -> pure result
        Nothing -> failValue ("infix operator is not a checked binary function: " <> show operatorType)
    Syn.EParen inner -> inferExprType inner
    Syn.ETypeSig inner _ -> inferExprType inner
    Syn.ETypeApp inner _ -> inferExprType inner
    -- A let expression and an if expression have the type of their body.
    Syn.ELetDecls _ body -> inferExprType body
    Syn.EIf _ thenExpression _ -> inferExprType thenExpression
    Syn.ELambdaCase alternatives -> lambdaCaseType (map caseAlternativeMatch alternatives)
    Syn.ELambdaCases alternatives -> lambdaCaseType (map lambdaCaseAltMatch alternatives)
    unsupported -> failValue ("missing checked expression type: " <> take 80 (show unsupported))

exprType :: Syn.Expr -> Maybe TcType
exprType expression =
  case expression of
    Syn.EAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> exprType inner
    Syn.EApp function _ -> exprType function >>= applicationResultType
    Syn.EInfix function operator _
      | isApplicationOperator operator -> exprType function >>= applicationResultType
    Syn.EParen inner -> exprType inner
    Syn.ELetDecls _ body -> exprType body
    Syn.EIf _ thenExpression _ -> exprType thenExpression
    Syn.ETypeSig inner _ -> exprType inner
    Syn.ETypeApp inner _ -> exprType inner
    _ -> Nothing

applicationResultType :: TcType -> Maybe TcType
applicationResultType ty =
  case ty of
    TcForAllTy _ body -> applicationResultType body
    TcQualTy _ body -> applicationResultType body
    TcFunTy _ result -> Just result
    _ -> Nothing

convertCheckedType :: TcType -> ValueM Type
convertCheckedType ty = do
  env <- gets vsConvertEnv
  case convertType env ty of
    Left message -> failValue (message <> " while converting " <> show ty)
    Right converted -> pure converted

convertTypeBinder :: TyVarId -> ValueM Binder
convertTypeBinder tyVar = do
  env <- gets vsConvertEnv
  liftEither (tyVarBinder env tyVar)

convertTypeBinders :: [TyVarId] -> ValueM [Binder]
convertTypeBinders variables =
  withTypeVariables variables (mapM convertTypeBinder variables)

convertRuntimeRep :: TcType -> ValueM Type
convertRuntimeRep runtimeRep = do
  env <- gets vsConvertEnv
  liftEither (convertRep env runtimeRep)

numericRepresentation :: Syn.NumericType -> TcType
numericRepresentation numericType =
  case numericType of
    Syn.TInteger -> IntRep
    Syn.TIntHash -> IntRep
    Syn.TWordHash -> WordRep
    Syn.TInt8Hash -> Int8Rep
    Syn.TInt16Hash -> Int16Rep
    Syn.TInt32Hash -> Int32Rep
    Syn.TInt64Hash -> Int64Rep
    Syn.TWord8Hash -> Word8Rep
    Syn.TWord16Hash -> Word16Rep
    Syn.TWord32Hash -> Word32Rep
    Syn.TWord64Hash -> Word64Rep

withLocals :: [(TcTermKey, (Binder, TcType))] -> ValueM a -> ValueM a
withLocals additions action = do
  previous <- gets vsLocals
  modify' (\state -> state {vsLocals = foldr (uncurry Map.insert) previous additions})
  result <- action
  modify' (\state -> state {vsLocals = previous})
  pure result

-- | Run an action with more dictionaries in scope and bind the superclass
-- projections that it shares inside its result.
--
-- The new dictionaries are in scope only inside the result, so the action
-- gets a scope of its own. A projection that names one of them then stays
-- inside the result.
withDictionaryScope :: [Dictionary] -> ValueM Expr -> ValueM Expr
withDictionaryScope additions action = withDictionaries additions (evidenceScope action)

-- | Run the body of a case alternative, or of another construct that can
-- bind evidence.
--
-- The body gets a scope of its own only when the construct binds something
-- that an evidence term can name: a dictionary, or a type variable. Most
-- alternatives bind neither, and then the body keeps the scope of the
-- enclosing binding. A projection in an alternative can therefore share the
-- binding that the body around the case already has.
withAlternativeScope :: Bool -> [Dictionary] -> ValueM Expr -> ValueM Expr
withAlternativeScope bindsTypeVariables additions action
  | not bindsTypeVariables && null additions = action
  | otherwise = withDictionaryScope additions action

-- | Give an expression its own evidence scope. A superclass projection that
-- the expression selects more than once becomes one @let@ around it.
evidenceScope :: ValueM Expr -> ValueM Expr
evidenceScope action = do
  previous <- gets vsEvidenceScope
  modify' (\state -> state {vsEvidenceScope = Just (EvidenceScope Map.empty [])})
  result <- action
  binds <- gets (maybe [] (reverse . evidenceBindsRev) . vsEvidenceScope)
  modify' (\state -> state {vsEvidenceScope = previous})
  pure (bindSharedEvidence binds result)

-- | Bind the shared projections around a body.
--
-- A projection is a short case chain, and a binding for it costs a closure.
-- A name therefore pays only from the second use, so put a projection back
-- where it stands when the body names it once, and drop it when the body
-- does not name it at all.
--
-- A later binding can name an earlier one. 'foldr' decides the innermost
-- binding first, so each decision counts the uses in every binding that
-- stays inside it.
bindSharedEvidence :: [Bind] -> Expr -> Expr
bindSharedEvidence binds result = foldr step result binds
  where
    step binding body =
      let name = binderName (bindBinder binding)
       in case countUses name body of
            0 -> body
            1 -> substituteVar name (bindRhs binding) body
            _ -> ExLet binding body

-- | Run an action with evidence sharing off. A binder inside an evidence
-- term has no place outside that term, so a projection under it stays where
-- the type checker put it.
withoutEvidenceScope :: ValueM a -> ValueM a
withoutEvidenceScope action = do
  previous <- gets vsEvidenceScope
  modify' (\state -> state {vsEvidenceScope = Nothing})
  result <- action
  modify' (\state -> state {vsEvidenceScope = previous})
  pure result

-- | Give a superclass projection a name, and reuse the name when the same
-- projection appears again in the same scope.
shareSuperClass :: Ev.EvTerm -> TcType -> ValueM Expr -> ValueM Expr
shareSuperClass evidence resultType build = do
  scope <- gets vsEvidenceScope
  case scope of
    Nothing -> build
    Just current ->
      case Map.lookup evidence (evidenceCache current) of
        Just binder -> pure (ExVar (binderName binder))
        Nothing -> do
          expression <- build
          binder <- freshBinder "$super" resultType
          -- The build step can open and close scopes of its own, so read the
          -- scope again rather than extending the one from before.
          modify' $ \state ->
            state
              { vsEvidenceScope =
                  fmap
                    ( \latest ->
                        latest
                          { evidenceCache = Map.insert evidence binder (evidenceCache latest),
                            evidenceBindsRev = Bind binder expression : evidenceBindsRev latest
                          }
                    )
                    (vsEvidenceScope state)
              }
          pure (ExVar (binderName binder))

withDictionaries :: [Dictionary] -> ValueM a -> ValueM a
withDictionaries additions action = do
  previous <- gets vsDictionaries
  let updated = foldr insertDictionary previous additions
  modify' (\state -> state {vsDictionaries = updated})
  result <- action
  modify' (\state -> state {vsDictionaries = previous})
  pure result
  where
    insertDictionary dictionary =
      Map.insert (predicateKey (dictionaryPredicate dictionary)) (dictionaryBinder dictionary)

predicateKey :: Pred -> Text
predicateKey predicate =
  case predicate of
    ClassPred classTyCon arguments -> dictionaryKey classTyCon arguments
    EqPred left right -> typeKey left <> "~" <> typeKey right
    QuantifiedPred {} -> "quantified:" <> T.pack (show predicate)
    -- The name alone identifies an implicit parameter.
    IParamPred name _ -> "implicit:" <> name

dictionaryKey :: TyCon -> [TcType] -> Text
dictionaryKey classTyCon arguments =
  packageIdText (tyConPackageId classTyCon)
    <> ":"
    <> tyConModuleName classTyCon
    <> ":"
    <> tyConName classTyCon
    <> T.concat (map ((":" <>) . typeKey) arguments)

typeKey :: TcType -> Text
typeKey ty = T.pack (show ty)

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls ty =
  case ty of
    TcForAllTy tyVar body ->
      let (tyVars, result) = peelForAlls body
       in (tyVar : tyVars, result)
    _ -> ([], ty)

peelConstraints :: TcType -> ([Pred], TcType)
peelConstraints ty =
  case ty of
    TcQualTy predicates body -> (predicates, body)
    _ -> ([], ty)

peelFunctions :: Int -> TcType -> ([TcType], TcType)
peelFunctions count ty
  | count <= 0 = ([], ty)
peelFunctions count (TcFunTy argument result) =
  let (arguments, finalResult) = peelFunctions (count - 1) result
   in (argument : arguments, finalResult)
peelFunctions _ ty = ([], ty)

liftEither :: Either String a -> ValueM a
liftEither = either failValue pure

failValue :: String -> ValueM a
failValue = lift . Left
