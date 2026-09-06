{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Write derived instances as surface instance declarations.
--
-- A deriving plan with a resolved context becomes an ordinary
-- @instance ctx => C (T a) where ...@ whose method equations are built here
-- from the checked constructor layout of the datatype. The declaration then
-- goes through the same instance checker and System FC lowering as source,
-- so no later phase needs to know that it was derived.
--
-- Generated names carry resolver annotations, the way the resolver would
-- have left them: constructors and methods point at their defining module,
-- pattern variables are locals with uniques that no resolver local uses, and
-- library values come from the 'DerivingReferences' of the configuration.
module Aihc.Tc.Deriving.Generate
  ( generateDerivedInstances,
  )
where

import Aihc.Parser.Syntax
  ( ArrowKind (..),
    CaseAlt (..),
    Decl (..),
    Expr (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    Name (..),
    NameType (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan,
    StandaloneDerivingDecl (..),
    Type (..),
    TypePromotion (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    mkUnqualifiedName,
    peelDeclAnn,
    qualifyName,
  )
import Aihc.Resolve (Identifier (..), PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Annotations
  ( TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
  )
import Aihc.Tc.Deriving.Context (isSupportedStockClass, newtypeRepresentation, stockFieldTypes, typeTyVars)
import Aihc.Tc.Deriving.References
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataConSourceForm (..), DataTypeInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | The instance declarations for every deriving plan of a module. A plan
-- whose context is unresolved has already reported its error; a plan for a
-- strategy or class the generator does not support reports a warning and
-- produces no instance.
generateDerivedInstances :: (Text, Text) -> Module -> TcM [Decl]
generateDerivedInstances origin modu = do
  references <- getDerivingReferences
  concat <$> mapM (declDerivedInstances references origin) (moduleDecls modu)

declDerivedInstances :: DerivingReferences -> (Text, Text) -> Decl -> TcM [Decl]
declDerivedInstances references origin decl =
  case decl of
    DeclAnn annotation inner -> do
      own <-
        case fromAnnotation @TcDerivingAnnotation annotation of
          Just derivingAnnotation ->
            catMaybes <$> mapM (generatePlan references origin (peelDeclAnn inner)) (tcDerivingPlans derivingAnnotation)
          Nothing -> pure []
      rest <- declDerivedInstances references origin inner
      pure (own <> rest)
    _ -> pure []

-- | The generation context of one plan.
data Gen = Gen
  { genSpan :: !SourceSpan,
    genReferences :: !DerivingReferences,
    genPlan :: !TcDerivingPlan,
    -- | Package and module of the class, where its methods live.
    genClassOrigin :: !(Text, Text)
  }

generatePlan :: DerivingReferences -> (Text, Text) -> Decl -> TcDerivingPlan -> TcM (Maybe Decl)
generatePlan references origin sourceDecl plan =
  case supportedStrategy of
    Left message -> do
      emitWarning (tcDerivingSourceSpan plan) (OtherError message)
      pure Nothing
    Right () ->
      case tcDerivingContext plan of
        -- Context inference already reported why the plan has no context.
        TcDerivingInferContext -> pure Nothing
        TcDerivingExplicitContext context -> do
          available <- referencesAvailable gen
          case (available, instanceHeader context) of
            (Just missing, _) -> do
              emitError (genSpan gen) (OtherError (mechanism <> " needs " <> missing <> ", which is not available in this compilation"))
              pure Nothing
            (Nothing, Nothing) -> do
              emitError (genSpan gen) (OtherError (mechanism <> " cannot express the instance head or context as source syntax"))
              pure Nothing
            (Nothing, Just (forallBinders, surfaceContext, surfaceHead)) -> do
              items <- generateItems gen
              pure $
                items <&> \generated ->
                  DeclAnn (mkAnnotation (genSpan gen)) $
                    DeclInstance
                      InstanceDecl
                        { instanceDeclPragmas = [],
                          instanceDeclWarning = Nothing,
                          instanceDeclForall = forallBinders,
                          instanceDeclContext = surfaceContext,
                          instanceDeclHead = surfaceHead,
                          instanceDeclItems = generated
                        }
  where
    gen =
      Gen
        { genSpan = tcDerivingSourceSpan plan,
          genReferences = references,
          genPlan = plan,
          genClassOrigin = fromMaybe origin (tcDerivingClassOrigin plan)
        }
    className = T.unpack (tcDerivingClassName plan)
    mechanism =
      case tcDerivingStrategy plan of
        TcDerivingStock -> "stock " <> className <> " deriving"
        TcDerivingNewtype -> "newtype " <> className <> " deriving"
        TcDerivingAnyclass -> "anyclass " <> className <> " deriving"
        TcDerivingVia {} -> "via " <> className <> " deriving"
    supportedStrategy =
      case tcDerivingStrategy plan of
        TcDerivingAnyclass -> Right ()
        TcDerivingNewtype -> Right ()
        TcDerivingStock
          | isSupportedStockClass (tcDerivingClassName plan) -> Right ()
          | otherwise -> Left ("stock deriving of " <> className <> " is not supported yet; no instance is generated")
        TcDerivingVia {} -> Left ("deriving via is not supported yet; no instance is generated for " <> className)
    -- A standalone declaration keeps the syntax the user wrote. An attached
    -- clause renders its checked head and inferred context.
    instanceHeader context =
      case sourceDecl of
        DeclStandaloneDeriving derivingDecl ->
          Just (standaloneDerivingForall derivingDecl, standaloneDerivingContext derivingDecl, standaloneDerivingHead derivingDecl)
        _ -> do
          surfaceContext <- mapM (surfacePred (genSpan gen)) context
          headArguments <- mapM (surfaceType (genSpan gen)) (tcDerivingHeadTypes plan)
          pure ([], surfaceContext, foldl TApp (TCon (tyConNameSyntax (genSpan gen) (tcDerivingClassTyCon plan)) Unpromoted) headArguments)

-- | The method equations of a plan, or 'Nothing' after reporting why the
-- datatype cannot be derived.
generateItems :: Gen -> TcM (Maybe [InstanceDeclItem])
generateItems gen =
  case tcDerivingStrategy plan of
    TcDerivingAnyclass -> pure (Just [])
    TcDerivingNewtype ->
      case newtypeRepresentation plan of
        Left message -> failWith message
        Right _ -> Just <$> newtypeItems gen
    TcDerivingStock ->
      case (stockFieldTypes plan, tcDerivingDataType plan) of
        (Left message, _) -> failWith message
        (Right _, Just dataType) ->
          let constructors = dtiConstructors dataType
           in case tcDerivingClassName plan of
                "Eq" -> Just <$> eqItems gen constructors
                "Ord" -> Just <$> ordItems gen constructors
                "Show" -> Just <$> showItems gen constructors
                "Bounded" -> boundedItems gen constructors
                other -> failWith ("stock deriving of " <> T.unpack other <> " is not supported yet")
        (Right _, Nothing) -> failWith "stock deriving requires checked datatype metadata"
    TcDerivingVia {} -> pure Nothing
  where
    plan = genPlan gen
    failWith message = do
      emitError (genSpan gen) (OtherError message)
      pure Nothing

-- | The first library reference the plan needs that the type environment
-- does not know, as a description for the error message.
referencesAvailable :: Gen -> TcM (Maybe String)
referencesAvailable gen = do
  present <- forM needed $ \reference -> do
    binder <- lookupTermKey (TcTermGlobal (referencePackage reference) (referenceModule reference) (referenceName reference))
    pure (reference, binder)
  pure (listToMaybe [describe reference | (reference, Nothing) <- present])
  where
    references = genReferences gen
    needed =
      case (tcDerivingStrategy (genPlan gen), tcDerivingClassName (genPlan gen)) of
        (TcDerivingStock, "Eq") -> [derivingTrue references, derivingFalse references]
        (TcDerivingStock, "Ord") -> [derivingLT references, derivingEQ references, derivingGT references]
        (TcDerivingStock, "Show") ->
          [derivingIntCon references, derivingGreaterOrEqual references, derivingCons references]
        _ -> []
    describe reference = T.unpack (referenceModule reference <> "." <> referenceName reference)
    listToMaybe candidates =
      case candidates of
        [] -> Nothing
        candidate : _ -> Just candidate

-- * Eq

eqItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
eqItems gen constructors = do
  matches <- mapM constructorMatch constructors
  let fallback = [simpleMatch gen [atPattern gen PWildcard, atPattern gen PWildcard] (referenceExpr gen derivingFalse) | length constructors > 1]
  pure [methodBind gen "==" (matches <> fallback)]
  where
    constructorMatch constructor = do
      lefts <- fieldLocals gen "a" constructor
      rights <- fieldLocals gen "b" constructor
      pure
        ( simpleMatch
            gen
            [constructorPattern gen constructor (map Just lefts), constructorPattern gen constructor (map Just rights)]
            (conjunction [methodApp gen "==" [localExpr gen left, localExpr gen right] | (left, right) <- zip lefts rights])
        )
    conjunction tests =
      case tests of
        [] -> referenceExpr gen derivingTrue
        [test] -> test
        test : rest ->
          caseOf
            gen
            test
            [ (referencePattern gen derivingTrue, conjunction rest),
              (referencePattern gen derivingFalse, referenceExpr gen derivingFalse)
            ]

-- * Ord

ordItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
ordItems gen constructors =
  case constructors of
    [constructor] -> do
      lefts <- fieldLocals gen "a" constructor
      rights <- fieldLocals gen "b" constructor
      pure
        [ methodBind
            gen
            "compare"
            [simpleMatch gen [constructorPattern gen constructor (map Just lefts), constructorPattern gen constructor (map Just rights)] (compareFields (zip lefts rights))]
        ]
    _ -> do
      left <- freshLocal gen "x"
      right <- freshLocal gen "y"
      alternatives <- mapM (outerAlternative right) (zip [0 :: Int ..] constructors)
      pure [methodBind gen "compare" [simpleMatch gen [atPattern gen (PVar left), atPattern gen (PVar right)] (caseOf gen (localExpr gen left) alternatives)]]
  where
    lastIndex = length constructors - 1
    -- The constructors before this one compare greater, the same
    -- constructor compares its fields, and every later one compares less.
    outerAlternative right (index, constructor) = do
      lefts <- fieldLocals gen "a" constructor
      rights <- fieldLocals gen "b" constructor
      let earlier =
            [ (constructorPattern gen other (map (const Nothing) (dciFields other)), referenceExpr gen derivingGT)
            | other <- take index constructors
            ]
          same = (constructorPattern gen constructor (map Just rights), compareFields (zip lefts rights))
          later = [(atPattern gen PWildcard, referenceExpr gen derivingLT) | index < lastIndex]
      pure (constructorPattern gen constructor (map Just lefts), caseOf gen (localExpr gen right) (earlier <> [same] <> later))
    compareFields pairs =
      case pairs of
        [] -> referenceExpr gen derivingEQ
        [(left, right)] -> methodApp gen "compare" [localExpr gen left, localExpr gen right]
        (left, right) : rest ->
          caseOf
            gen
            (methodApp gen "compare" [localExpr gen left, localExpr gen right])
            [ (referencePattern gen derivingLT, referenceExpr gen derivingLT),
              (referencePattern gen derivingGT, referenceExpr gen derivingGT),
              (referencePattern gen derivingEQ, compareFields rest)
            ]

-- * Show

showItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
showItems gen constructors = do
  matches <- mapM constructorMatch constructors
  pure [methodBind gen "showsPrec" matches]
  where
    constructorMatch constructor
      | null (dciFields constructor) = do
          body <- showStringExpr (prefixConstructorText constructor)
          pure (simpleMatch gen [atPattern gen PWildcard, constructorPattern gen constructor []] body)
      | otherwise = do
          precedence <- freshLocal gen "d"
          fields <- fieldLocals gen "a" constructor
          suffix <- freshLocal gen "s"
          rendering <- freshLocal gen "k"
          tail' <- freshLocal gen "t"
          let (parenPrecedence, pieces) = constructorPieces constructor fields
              -- @showParen (d >= p) k s@, with the rendering @k@ bound once
              -- so the pieces are not written out for both branches.
              body =
                lambda gen suffix $
                  applyN
                    gen
                    ( lambda gen rendering $
                        caseOf
                          gen
                          (applyN gen (referenceExpr gen derivingGreaterOrEqual) [localExpr gen precedence, intLiteral gen parenPrecedence])
                          [ (referencePattern gen derivingTrue, cons '(' (applyN gen (localExpr gen rendering) [cons ')' (localExpr gen suffix)])),
                            (referencePattern gen derivingFalse, applyN gen (localExpr gen rendering) [localExpr gen suffix])
                          ]
                    )
                    [lambda gen tail' (foldr ($) (localExpr gen tail') pieces)]
          pure (simpleMatch gen [atPattern gen (PVar precedence), constructorPattern gen constructor (map Just fields)] body)
    -- The pieces of the rendering, each applied to the rest of the output,
    -- with the precedence above which the whole needs parentheses.
    constructorPieces constructor fields =
      case dciSourceForm constructor of
        RecordDataCon ->
          ( 11,
            showStringPiece (prefixConstructorText constructor <> " {")
              : concat
                [ [showStringPiece (separator <> fieldLabelText label <> " = "), showsPrecPiece 0 field]
                | (index, (label, field)) <- zip [0 :: Int ..] (zip (map dcfiLabel (dciFields constructor)) fields),
                  let separator = if index == 0 then "" else ", "
                ]
                <> [showStringPiece "}"]
          )
        InfixDataCon
          | [left, right] <- fields ->
              -- Without fixity information every infix constructor takes
              -- the default fixity 9.
              ( 10,
                [showsPrecPiece 10 left, showStringPiece (" " <> infixConstructorText constructor <> " "), showsPrecPiece 10 right]
              )
        _ ->
          ( 11,
            showStringPiece (prefixConstructorText constructor <> " ")
              : intersperseWith (showStringPiece " ") (map (showsPrecPiece 11) fields)
          )
    -- @showString text@ as a chain of list constructors, so the rendering
    -- needs nothing beyond the primitive package.
    showStringPiece text rest = foldr cons rest (T.unpack text)
    showsPrecPiece precedence field rest = methodApp gen "showsPrec" [intLiteral gen precedence, localExpr gen field, rest]
    showStringExpr text = do
      suffix <- freshLocal gen "s"
      pure (lambda gen suffix (showStringPiece text (localExpr gen suffix)))
    cons character rest = applyN gen (referenceExpr gen derivingCons) [at gen (EChar character (T.pack (show character))), rest]
    intersperseWith separator pieces =
      case pieces of
        [] -> []
        [piece] -> [piece]
        piece : rest -> piece : separator : intersperseWith separator rest

prefixConstructorText :: DataConInfo -> Text
prefixConstructorText constructor
  | isSymbolic (dciName constructor) = "(" <> dciName constructor <> ")"
  | otherwise = dciName constructor

infixConstructorText :: DataConInfo -> Text
infixConstructorText constructor
  | isSymbolic (dciName constructor) = dciName constructor
  | otherwise = "`" <> dciName constructor <> "`"

fieldLabelText :: Maybe Text -> Text
fieldLabelText label =
  case label of
    Just text
      | isSymbolic text -> "(" <> text <> ")"
      | otherwise -> text
    Nothing -> ""

-- * Bounded

boundedItems :: Gen -> [DataConInfo] -> TcM (Maybe [InstanceDeclItem])
boundedItems gen constructors
  | all (null . dciFields) constructors,
    first : _ <- constructors =
      pure (Just [bound "minBound" (constructorExpr gen first), bound "maxBound" (constructorExpr gen (last constructors))])
  | [constructor] <- constructors =
      pure
        ( Just
            [ bound "minBound" (applyN gen (constructorExpr gen constructor) (map (const (methodExpr gen "minBound")) (dciFields constructor))),
              bound "maxBound" (applyN gen (constructorExpr gen constructor) (map (const (methodExpr gen "maxBound")) (dciFields constructor)))
            ]
        )
  | otherwise = do
      emitError (genSpan gen) (OtherError "stock Bounded deriving requires an enumeration or a single-constructor type")
      pure Nothing
  where
    bound name body = methodBind gen name [simpleMatch gen [] body]

-- * Newtype

-- | Each class method at the newtype, wrapped and unwrapped around the
-- method at the representation type. A method whose type mentions the
-- class parameter somewhere the wrapper cannot reach keeps its default,
-- or is reported when it has none.
newtypeItems :: Gen -> TcM [InstanceDeclItem]
newtypeItems gen =
  catMaybes <$> mapM methodItem (tcDerivingClassMethods (genPlan gen))
  where
    plan = genPlan gen
    classVar =
      case reverse (tcDerivingClassTyVars plan) of
        variable : _ -> Just variable
        [] -> Nothing
    methodItem method = do
      let name = tcClassMethodName method
          body = stripQualifiers (snd (peelForAlls (tcClassMethodType method)))
      coerced <-
        case classVar of
          Just variable
            | variable `elem` typeTyVars body -> coerceExpr gen variable ToNewtype body (methodExpr gen name)
          _ -> pure Nothing
      case coerced of
        Just expr -> pure (Just (methodBind gen name [simpleMatch gen [] expr]))
        Nothing
          | name `elem` tcDerivingDefaultMethods plan -> pure Nothing
          | otherwise -> do
              emitError (genSpan gen) (OtherError ("newtype deriving cannot coerce the method " <> T.unpack name <> " of " <> T.unpack (tcDerivingClassName plan)))
              pure Nothing
    stripQualifiers ty =
      case ty of
        TcQualTy _ inner -> stripQualifiers inner
        _ -> ty

data CoerceDirection = ToNewtype | FromNewtype

flipDirection :: CoerceDirection -> CoerceDirection
flipDirection direction =
  case direction of
    ToNewtype -> FromNewtype
    FromNewtype -> ToNewtype

-- | Convert a value between the representation and the newtype along the
-- structure of a method type: occurrences of the class parameter wrap or
-- unwrap, function types convert argument and result, and types without
-- the parameter pass through.
coerceExpr :: Gen -> TyVarId -> CoerceDirection -> TcType -> Expr -> TcM (Maybe Expr)
coerceExpr gen classVar direction ty expr
  | classVar `notElem` typeTyVars ty = pure (Just expr)
  | headIsClassVar ty =
      case (direction, newtypeConstructor) of
        (ToNewtype, Just constructor) -> pure (Just (at gen (EApp (constructorExpr gen constructor) expr)))
        (FromNewtype, Just constructor) -> do
          field <- freshLocal gen "r"
          pure (Just (caseOf gen expr [(constructorPattern gen constructor [Just field], localExpr gen field)]))
        _ -> pure Nothing
  | TcFunTy argument result <- ty = do
      parameter <- freshLocal gen "x"
      convertedArgument <- coerceExpr gen classVar (flipDirection direction) argument (localExpr gen parameter)
      case convertedArgument of
        Nothing -> pure Nothing
        Just argumentExpr -> do
          convertedResult <- coerceExpr gen classVar direction result (at gen (EApp expr argumentExpr))
          pure (lambda gen parameter <$> convertedResult)
  | otherwise = pure Nothing
  where
    newtypeConstructor =
      case dtiConstructors <$> tcDerivingDataType (genPlan gen) of
        Just [constructor] -> Just constructor
        _ -> Nothing
    headIsClassVar candidate =
      case candidate of
        TcTyVar variable -> variable == classVar
        TcAppTy function _ -> headIsClassVar function
        _ -> False

-- * Syntax builders

-- | A method equation group, placed at the deriving clause so diagnostics
-- and annotations of the generated code point at the clause.
methodBind :: Gen -> Text -> [Match] -> InstanceDeclItem
methodBind gen name matches =
  InstanceItemAnn
    (mkAnnotation (genSpan gen))
    (InstanceItemBind (FunctionBind (UnqualifiedName (variableNameType name) name [mkAnnotation (genSpan gen)]) matches))

simpleMatch :: Gen -> [Pattern] -> Expr -> Match
simpleMatch gen patterns body =
  Match
    { matchAnns = [mkAnnotation (genSpan gen)],
      matchHeadForm = MatchHeadPrefix,
      matchPats = patterns,
      matchRhs = UnguardedRhs [] body Nothing
    }

-- | Place a generated expression at the deriving clause, so every type
-- annotation the checker attaches to it has a source position.
at :: Gen -> Expr -> Expr
at gen = EAnn (mkAnnotation (genSpan gen))

atPattern :: Gen -> Pattern -> Pattern
atPattern gen = PAnn (mkAnnotation (genSpan gen))

caseOf :: Gen -> Expr -> [(Pattern, Expr)] -> Expr
caseOf gen scrutinee alternatives =
  at gen $
    ECase
      scrutinee
      [ CaseAlt {caseAltAnns = [mkAnnotation (genSpan gen)], caseAltPattern = atPattern gen pat, caseAltRhs = UnguardedRhs [] body Nothing}
      | (pat, body) <- alternatives
      ]

lambda :: Gen -> UnqualifiedName -> Expr -> Expr
lambda gen parameter body = at gen (ELambdaPats [atPattern gen (PVar parameter)] body)

applyN :: Gen -> Expr -> [Expr] -> Expr
applyN gen = foldl (\function argument -> at gen (EApp function argument))

-- | A pattern variable for each field of a constructor.
fieldLocals :: Gen -> Text -> DataConInfo -> TcM [UnqualifiedName]
fieldLocals gen prefix constructor =
  mapM (\index -> freshLocal gen (prefix <> T.pack (show index))) [1 .. length (dciFields constructor)]

-- | A local binder that the type checker makes. The negative unique does
-- not collide with a resolver local or with other synthesized binders.
freshLocal :: Gen -> Text -> TcM UnqualifiedName
freshLocal gen text = do
  Unique key <- freshUnique
  let unique = negate (1000 + key)
  pure
    ( UnqualifiedName
        NameVarId
        text
        [ mkAnnotation (genSpan gen),
          mkAnnotation (ResolutionAnnotation (genSpan gen) (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))
        ]
    )

localExpr :: Gen -> UnqualifiedName -> Expr
localExpr gen = at gen . EVar . qualifyName Nothing

-- | A constructor pattern whose fields are variables or wildcards.
constructorPattern :: Gen -> DataConInfo -> [Maybe UnqualifiedName] -> Pattern
constructorPattern gen constructor fields =
  atPattern gen (PCon (constructorName gen constructor) [] (map (atPattern gen . maybe PWildcard PVar) fields))

constructorExpr :: Gen -> DataConInfo -> Expr
constructorExpr gen constructor = at gen (EVar (constructorName gen constructor))

-- | A resolved occurrence of a constructor.
constructorName :: Gen -> DataConInfo -> Name
constructorName gen constructor =
  resolvedName (genSpan gen) packageId moduleName' (constructorNameType text) ResolutionNamespaceTerm text
  where
    (packageId, moduleName') = dciOrigin constructor
    text = dciName constructor

-- | A resolved occurrence of a method of the class being derived.
methodExpr :: Gen -> Text -> Expr
methodExpr gen name =
  at gen $ EVar (resolvedName (genSpan gen) (PackageId packageId) moduleName' (variableNameType name) ResolutionNamespaceTerm name)
  where
    (packageId, moduleName') = genClassOrigin gen

methodApp :: Gen -> Text -> [Expr] -> Expr
methodApp gen name = applyN gen (methodExpr gen name)

referenceSyntax :: Gen -> (DerivingReferences -> DerivingReference) -> Name
referenceSyntax gen select =
  resolvedName (genSpan gen) (referencePackage reference) (referenceModule reference) (referenceNameType reference) (referenceNamespace reference) (referenceName reference)
  where
    reference = select (genReferences gen)

referenceExpr :: Gen -> (DerivingReferences -> DerivingReference) -> Expr
referenceExpr gen select = at gen (EVar (referenceSyntax gen select))

referencePattern :: Gen -> (DerivingReferences -> DerivingReference) -> Pattern
referencePattern gen select = atPattern gen (PCon (referenceSyntax gen select) [] [])

-- | A boxed @Int@ literal built from a primitive literal, so the value
-- needs no numeric class.
intLiteral :: Gen -> Integer -> Expr
intLiteral gen value =
  at gen $
    EApp
      (referenceExpr gen derivingIntCon)
      ( at gen $
          EAnn
            (mkAnnotation (ResolutionAnnotation (genSpan gen) (IdentifierNamed (referenceName primType)) ResolutionNamespaceType (ResolvedTopLevel (referencePackage primType) (Name (Just (referenceModule primType)) NameConId (referenceName primType) []))))
            (EInt value TIntHash (T.pack (show value) <> "#"))
      )
  where
    primType = derivingIntPrimType (genReferences gen)

resolvedName :: SourceSpan -> PackageId -> Text -> NameType -> ResolutionNamespace -> Text -> Name
resolvedName sp packageId moduleName' nameType namespace text =
  Name
    (Just moduleName')
    nameType
    text
    [ mkAnnotation sp,
      mkAnnotation (ResolutionAnnotation sp (IdentifierNamed text) namespace (ResolvedTopLevel packageId (Name (Just moduleName') nameType text [])))
    ]

tyConNameSyntax :: SourceSpan -> TyCon -> Name
tyConNameSyntax sp tyCon =
  resolvedName sp (tyConPackageId tyCon) (tyConModuleName tyCon) (constructorNameType (tyConName tyCon)) (tyConNamespace tyCon) (tyConName tyCon)

constructorNameType :: Text -> NameType
constructorNameType text
  | isSymbolic text = NameConSym
  | otherwise = NameConId

variableNameType :: Text -> NameType
variableNameType text
  | isSymbolic text = NameVarSym
  | otherwise = NameVarId

isSymbolic :: Text -> Bool
isSymbolic text =
  case T.uncons text of
    Just (first, _) -> not (isIdentifierStart first)
    Nothing -> False
  where
    isIdentifierStart character =
      character == '_' || character `elem` ['a' .. 'z'] || character `elem` ['A' .. 'Z'] || character > '\x7f'

-- * Surface types

-- | The checked type as the source syntax that the instance checker reads
-- back, or 'Nothing' for a type without a source form.
surfaceType :: SourceSpan -> TcType -> Maybe Type
surfaceType sp ty =
  case ty of
    TcTyVar tyVar -> Just (TVar (mkUnqualifiedName NameVarId (tvName tyVar)))
    TcFunTy argument result -> TFun ArrowUnrestricted <$> surfaceType sp argument <*> surfaceType sp result
    TcAppTy function argument -> TApp <$> surfaceType sp function <*> surfaceType sp argument
    TcTyCon tyCon [argument, result]
      | isArrowTyCon tyCon -> TFun ArrowUnrestricted <$> surfaceType sp argument <*> surfaceType sp result
    TcTyCon tyCon arguments
      | tyConNamespace tyCon == ResolutionNamespaceType ->
          foldl TApp (TCon (tyConNameSyntax sp tyCon) Unpromoted) <$> mapM (surfaceType sp) arguments
    _ -> Nothing

surfacePred :: SourceSpan -> Pred -> Maybe Type
surfacePred sp predicate =
  case predicate of
    ClassPred classTyCon arguments ->
      foldl TApp (TCon (tyConNameSyntax sp classTyCon) Unpromoted) <$> mapM (surfaceType sp) arguments
    _ -> Nothing

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tyVar body) =
  let (tyVars, inner) = peelForAlls body
   in (tyVar : tyVars, inner)
peelForAlls ty = ([], ty)
