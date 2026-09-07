{-# LANGUAGE OverloadedStrings #-}

-- | Type-check System FC terms and types. Kinds are types.
module Aihc.Fc.Lint
  ( lintProgram,
    loadScopeClosure,
    ModuleLoader,
    storeModuleLoader,
    LintError (..),
  )
where

import Aihc.Fc.Imports (unusedImports)
import Aihc.Fc.Name
import Aihc.Fc.Parser (parseProgram, renderParseError)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf hiding (coercionEndpoints)
import Aihc.Fc.Wired
import Aihc.Resolve (PackageId (..), packageIdText)
import Control.Monad (foldM, unless, when)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data LintError
  = UnboundName !Name
  | TypeMismatch !String !Type !Type
  | KindMismatch !String !Type !Type
  | ShadowedBinder !Name
  | UnusedImport !Name
  | LintFailure !String
  deriving (Eq, Show)

type ModuleLoader = PackageId -> Text -> IO (Maybe Program)

lintProgram :: Program -> [LintError]
lintProgram program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> [LintFailure "program needs a GHC.Types scope"]
    Just primPackage ->
      let env = typeEnvFromProgram primPackage program
       in map UnusedImport (unusedImports program)
            <> lintImportDeclarations env (programImports program)
            <> concatMap (lintDeclHeaders env) (programDecls program)
            <> concatMap (lintDeclBodies env) (programDecls program)

loadScopeClosure :: ModuleLoader -> [Program] -> IO [Program]
loadScopeClosure loader seeds = do
  resolved <- go Map.empty (concatMap scopeKeys seeds)
  pure (List.nub (seeds <> catMaybes (Map.elems resolved)))
  where
    go resolved [] = pure resolved
    go resolved (key : rest)
      | Map.member key resolved = go resolved rest
      | otherwise = do
          loaded <- uncurry loader key
          case loaded of
            Nothing -> go (Map.insert key Nothing resolved) rest
            Just program ->
              go (Map.insert key (Just program) resolved) (rest <> scopeKeys program)

storeModuleLoader :: FilePath -> ModuleLoader
storeModuleLoader storeRoot package moduleName = do
  let path = storeRoot </> T.unpack (packageIdText package) </> moduleDirectoryText moduleName </> "core"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      source <- TIO.readFile path
      case parseProgram source of
        Left parseError -> fail ("Invalid core file " <> path <> ": " <> renderParseError parseError)
        Right program -> pure (Just program)

moduleDirectoryText :: Text -> FilePath
moduleDirectoryText name =
  List.foldl' (</>) "" (map T.unpack (T.splitOn "." name))

scopeKeys :: Program -> [(PackageId, Text)]
scopeKeys program =
  [(package, name) | (_, package, name) <- scopeEntries (programScopes program)]

lintImportDeclarations :: TypeEnv -> Imports -> [LintError]
lintImportDeclarations env imports =
  concatMap (eitherToList . lintType env) (Map.elems (importHeaders imports))
    <> concatMap (eitherToList . lintType env) (Map.elems (importSynonyms imports))
    <> concatMap (lintAxiomDecl env) (Map.elems (importAxioms imports))
    <> concatMap (eitherToList . lintType env) (Map.elems (importBinders imports))

lintDeclHeaders :: TypeEnv -> Decl -> [LintError]
lintDeclHeaders env decl =
  case decl of
    DeclType declaration -> lintTypeDecl env declaration
    DeclSynonym declaration -> lintSynonymDecl env declaration
    DeclAxiom declaration -> lintAxiomDecl env declaration
    DeclVal declaration -> eitherToList (lintType env (valType declaration))

lintForeignImportDependency :: TypeEnv -> ForeignImportDependency -> [LintError]
lintForeignImportDependency env dependency =
  case dependency of
    ForeignAxiom name -> [UnboundName name | Map.notMember name (teAxioms env)]
    ForeignConstructor name -> [UnboundName name | Map.notMember name (teHeaders env)]

lintDeclBodies :: TypeEnv -> Decl -> [LintError]
lintDeclBodies env decl =
  case decl of
    DeclVal declaration ->
      map
        (addDeclarationContext (valName declaration))
        (eitherToList (checkExpr env "val body" (valType declaration) (valBody declaration)))
    _ -> []

addDeclarationContext :: Name -> LintError -> LintError
addDeclarationContext name lintError =
  let contextPrefix context = T.unpack (nameText name) <> ": " <> context
   in case lintError of
        TypeMismatch context expected actual -> TypeMismatch (contextPrefix context) expected actual
        KindMismatch context expected actual -> KindMismatch (contextPrefix context) expected actual
        other -> other

lintTypeDecl :: TypeEnv -> TypeDecl -> [LintError]
lintTypeDecl env declaration =
  case foldM bindLocal env (typeBinders declaration) of
    Left err -> [err]
    Right binderEnv ->
      eitherToList (lintType binderEnv (typeResult declaration))
        <> concatMap (eitherToList . lintType env . conType) (typeCons declaration)

lintSynonymDecl :: TypeEnv -> SynonymDecl -> [LintError]
lintSynonymDecl env declaration =
  case foldM bindLocal env (synBinders declaration) of
    Left err -> [err]
    Right binderEnv ->
      case (lintType binderEnv (synResult declaration), lintType binderEnv (synBody declaration)) of
        (Left err, _) -> [err]
        (_, Left err) -> [err]
        (Right {}, Right bodyKind) ->
          [KindMismatch "synonym body" (synResult declaration) bodyKind | not (typesEqual binderEnv (synResult declaration) bodyKind)]

lintAxiomDecl :: TypeEnv -> AxiomDecl -> [LintError]
lintAxiomDecl env declaration =
  case foldM bindLocal env (axiomBinders declaration) of
    Left err -> [err]
    Right binderEnv ->
      case (lintType binderEnv (axiomLeft declaration), lintType binderEnv (axiomRight declaration)) of
        (Left err, _) -> [err]
        (_, Left err) -> [err]
        (Right leftKind, Right rightKind) ->
          [KindMismatch "axiom sides" leftKind rightKind | not (typesEqual binderEnv leftKind rightKind)]

bindLocal :: TypeEnv -> Binder -> Either LintError TypeEnv
bindLocal env binder = do
  _ <- lintType env (binderType binder)
  let name = binderName binder
  when (Map.member name (teBinders env) || Map.member name (teHeaders env)) (Left (ShadowedBinder name))
  pure (extendBinder env binder)

lintType :: TypeEnv -> Type -> Either LintError Type
lintType env ty =
  case ty of
    TyVar name ->
      case lookupBinderType env name of
        Nothing -> Left (UnboundName name)
        Just kind -> Right kind
    TyCon name ->
      case lookupHeaderType env name of
        Nothing -> Left (UnboundName name)
        Just kind -> Right kind
    TyApp function argument -> do
      functionKind <- lintType env function
      argumentKind <- lintType env argument
      applyKind env function functionKind argument argumentKind
    TyFun r1 r2 argument result -> lintFun env r1 r2 argument result
    TyForAll binder body -> do
      binderEnv <- bindLocal env binder
      lintType binderEnv body
    TyEq left right -> do
      leftKind <- lintType env left
      rightKind <- lintType env right
      unless (typesEqual env leftKind rightKind) (Left (KindMismatch "equality" leftKind rightKind))
      Right (typeAppRep env (equalityRep (tePrimPackage env)))

lintFun :: TypeEnv -> Type -> Type -> Type -> Type -> Either LintError Type
lintFun env r1 r2 argument result = do
  r1Kind <- lintType env r1
  r2Kind <- lintType env r2
  argumentKind <- lintType env argument
  resultKind <- lintType env result
  let runtimeRep = runtimeRepKind env
      typeKind = typeKindType env
  unless (typesEqual env r1Kind runtimeRep) (Left (KindMismatch "FUN r1" runtimeRep r1Kind))
  unless (typesEqual env r2Kind runtimeRep) (Left (KindMismatch "FUN r2" runtimeRep r2Kind))
  unless (typesEqual env argumentKind (typeAppRep env r1)) (Left (KindMismatch "FUN argument" (typeAppRep env r1) argumentKind))
  unless (typesEqual env resultKind (typeAppRep env r2)) (Left (KindMismatch "FUN result" (typeAppRep env r2) resultKind))
  Right typeKind

applyKind :: TypeEnv -> Type -> Type -> Type -> Type -> Either LintError Type
applyKind env function functionKind argument argumentKind =
  case viewForAll env functionKind of
    Just (binder, body) -> do
      unless (kindsCompatible env function (binderType binder) argumentKind) (Left (KindMismatch "type application argument" (binderType binder) argumentKind))
      Right (substType (binderName binder) argument body)
    Nothing ->
      case viewFun env functionKind of
        Just (_, _, expected, result) -> do
          unless (kindsCompatible env function expected argumentKind) (Left (KindMismatch "type application argument" expected argumentKind))
          Right result
        Nothing -> Left (LintFailure ("type application to a type that is not a pi-type or FUN: " <> show functionKind))

kindsCompatible :: TypeEnv -> Type -> Type -> Type -> Bool
kindsCompatible env function expected actual =
  typesEqual env expected actual
    || kindFunctionsEqual env expected actual
    || (isTYPEName env function && isTypeKind env expected && isRuntimeRepKind env actual)

kindFunctionsEqual :: TypeEnv -> Type -> Type -> Bool
kindFunctionsEqual env left right =
  compareKinds (reduceType env left) (reduceType env right)
  where
    compareKinds first second
      | typesEqual env first second = True
    compareKinds (TyFun _ _ argument result) (TyForAll binder body) =
      not (typeUsesName (binderName binder) body)
        && typesEqual env argument (binderType binder)
        && compareKinds result body
    compareKinds (TyForAll binder body) (TyFun _ _ argument result) =
      not (typeUsesName (binderName binder) body)
        && typesEqual env (binderType binder) argument
        && compareKinds body result
    compareKinds _ _ = False

typeUsesName :: Name -> Type -> Bool
typeUsesName target ty =
  case ty of
    TyVar name -> name == target
    TyCon {} -> False
    TyApp function argument -> typeUsesName target function || typeUsesName target argument
    TyFun r1 r2 argument result -> any (typeUsesName target) [r1, r2, argument, result]
    TyForAll binder body
      | binderName binder == target -> typeUsesName target (binderType binder)
      | otherwise -> typeUsesName target (binderType binder) || typeUsesName target body
    TyEq left right -> typeUsesName target left || typeUsesName target right

isTYPEName :: TypeEnv -> Type -> Bool
isTYPEName env ty =
  case ty of
    TyCon name -> name == typeConstructor (tePrimPackage env)
    _ -> False

isTypeKind :: TypeEnv -> Type -> Bool
isTypeKind env = typesEqual env (typeKindType env)

isRuntimeRepKind :: TypeEnv -> Type -> Bool
isRuntimeRepKind env = typesEqual env (runtimeRepKind env)

viewForAll :: TypeEnv -> Type -> Maybe (Binder, Type)
viewForAll env ty =
  case reduceType env ty of
    TyForAll binder body -> Just (binder, body)
    _ -> Nothing

viewFun :: TypeEnv -> Type -> Maybe (Type, Type, Type, Type)
viewFun env ty =
  case reduceType env ty of
    TyFun r1 r2 argument result -> Just (r1, r2, argument, result)
    _ -> Nothing

typeKindType :: TypeEnv -> Type
typeKindType env = typeSynonym (tePrimPackage env)

runtimeRepKind :: TypeEnv -> Type
runtimeRepKind env = TyCon (runtimeRepConstructor (tePrimPackage env))

typeAppRep :: TypeEnv -> Type -> Type
typeAppRep env =
  TyApp (TyCon (typeConstructor (tePrimPackage env)))

checkExpr :: TypeEnv -> String -> Type -> Expr -> Either LintError ()
checkExpr env context expected expr =
  case expr of
    ExLit literal -> checkLiteral env expected literal
    ExLam binder body ->
      case viewFun env expected of
        Just (_, _, argument, result) -> do
          unless (typesEqual env argument (binderType binder)) (Left (TypeMismatch "lambda binder" argument (binderType binder)))
          binderEnv <- bindLocal env binder
          checkExpr binderEnv "lambda body" result body
        Nothing -> inferAndCompare
    ExTyLam binder body ->
      case viewForAll env expected of
        Just (expectedBinder, expectedBody) -> do
          unless (typesEqual env (binderType expectedBinder) (binderType binder)) (Left (KindMismatch "type lambda binder" (binderType expectedBinder) (binderType binder)))
          binderEnv <- bindLocal env binder
          let bodyType = substType (binderName expectedBinder) (TyVar (binderName binder)) expectedBody
          checkExpr binderEnv "type lambda body" bodyType body
        Nothing -> inferAndCompare
    ExLet binding body -> do
      bindEnv <- lintNonRecBind env binding
      checkExpr bindEnv context expected body
    ExRec bindings body -> do
      recEnv <- foldM bindLocal env (map bindBinder bindings)
      mapM_ (lintRecRhs recEnv) bindings
      checkExpr recEnv context expected body
    ExCase scrutinee binder resultType alts -> do
      unless (typesEqual env expected resultType) (Left (TypeMismatch "case result" expected resultType))
      _ <- lintCase env scrutinee binder resultType alts
      Right ()
    ExCast body coercion -> do
      (source, target) <- coercionEndpoints env coercion
      unless (typesEqual env expected target) (Left (TypeMismatch "cast target" expected target))
      checkExpr env "cast source" source body
    ExApp function argument
      | ExLam binder _ <- function -> do
          r1 <- representationOf env (binderType binder)
          r2 <- representationOf env expected
          checkExpr env "application function" (TyFun r1 r2 (binderType binder) expected) function
          checkExpr env "application argument" (binderType binder) argument
    _ -> inferAndCompare
  where
    inferAndCompare = do
      actual <- lintExpr env expr
      unless (typesEqual env expected actual) (Left (TypeMismatch context expected actual))

checkLiteral :: TypeEnv -> Type -> Literal -> Either LintError ()
checkLiteral env expected literal = do
  let representation = literalRepresentation literal
  representationKind <- lintType env representation
  let expectedRepresentationKind = runtimeRepKind env
  unless (typesEqual env expectedRepresentationKind representationKind) (Left (KindMismatch "literal representation" expectedRepresentationKind representationKind))
  checkLiteralRepresentation literal
  expectedRepresentation <- representationOf env expected
  unless (typesEqual env expectedRepresentation representation) (Left (TypeMismatch "literal representation" expectedRepresentation representation))

literalRepresentation :: Literal -> Type
literalRepresentation literal =
  case literal of
    LitInt representation _ -> representation
    LitChar representation _ -> representation
    LitAddr representation _ -> representation

checkLiteralRepresentation :: Literal -> Either LintError ()
checkLiteralRepresentation literal =
  case literal of
    LitInt (TyCon name) _
      | nameText name `elem` integerRepresentations -> Right ()
    LitInt {} -> Left (LintFailure "integer literal has an invalid representation")
    LitChar (TyCon name) _
      | nameText name == "WordRep" -> Right ()
    LitChar {} -> Left (LintFailure "character literal has an invalid representation")
    LitAddr (TyCon name) _
      | nameText name == "AddrRep" -> Right ()
    LitAddr {} -> Left (LintFailure "address literal has an invalid representation")
  where
    integerRepresentations =
      [ "IntRep",
        "WordRep",
        "Int8Rep",
        "Int16Rep",
        "Int32Rep",
        "Int64Rep",
        "Word8Rep",
        "Word16Rep",
        "Word32Rep",
        "Word64Rep",
        "FloatRep",
        "DoubleRep"
      ]

lintExpr :: TypeEnv -> Expr -> Either LintError Type
lintExpr env expr =
  case expr of
    ExCoercion proof -> do
      lintNominalCoercion env proof
      (left, right) <- coercionEndpoints env proof
      let ty = TyEq left right
      _ <- lintType env ty
      Right ty
    ExVar name -> lookupTerm env name
    ExLit {} -> Left (LintFailure "literal expression needs an expected type")
    ExApp function argument -> do
      functionType <- lintExpr env function
      case viewFun env functionType of
        Just (_, _, expected, result) -> do
          checkExpr env "application argument" expected argument
          Right result
        Nothing -> Left (LintFailure ("application to a non-FUN type: " <> show functionType))
    ExTyApp function argument -> do
      functionType <- lintExpr env function
      argumentKind <- lintType env argument
      case viewForAll env functionType of
        Just (binder, body) -> do
          unless (kindsCompatible env argument (binderType binder) argumentKind) (Left (KindMismatch "type application argument" (binderType binder) argumentKind))
          Right (substType (binderName binder) argument body)
        Nothing -> Left (LintFailure ("type application to a non-pi type: " <> show functionType))
    ExLam binder body -> do
      binderEnv <- bindLocal env binder
      bodyType <- lintExpr binderEnv body
      r1 <- representationOf binderEnv (binderType binder)
      r2 <- representationOf binderEnv bodyType
      Right (TyFun r1 r2 (binderType binder) bodyType)
    ExTyLam binder body -> do
      binderEnv <- bindLocal env binder
      bodyType <- lintExpr binderEnv body
      Right (TyForAll binder bodyType)
    ExLet bind body -> do
      bindEnv <- lintNonRecBind env bind
      lintExpr bindEnv body
    ExRec binds body -> do
      recEnv <- foldM bindLocal env (map bindBinder binds)
      mapM_ (lintRecRhs recEnv) binds
      lintExpr recEnv body
    ExCase scrutinee binder resultType alts -> lintCase env scrutinee binder resultType alts
    ExCast body coercion -> do
      (source, target) <- coercionEndpoints env coercion
      checkExpr env "cast source" source body
      Right target
    ExForeignCall call types arguments -> lintForeignCall env call types arguments

-- | A foreign call instantiates every leading binder of the foreign type and
-- fills every arrow of the foreign type. Its result is the type after the
-- arrows.
lintForeignCall :: TypeEnv -> ForeignCall -> [Type] -> [Expr] -> Either LintError Type
lintForeignCall env call types arguments = do
  -- The foreign type is closed, so its binders cannot shadow a local binder.
  _ <- lintType env {teBinders = Map.empty} (foreignCallType call)
  case concatMap (lintForeignImportDependency env) (foreignCallDependencies call) of
    [] -> Right ()
    problem : _ -> Left problem
  instantiated <- foldM applyTypeArgument (foreignCallType call) types
  when (isJust (viewForAll env instantiated)) (Left (LintFailure ("foreign call has too few type arguments: " <> show (foreignCallName call))))
  -- The declared type gives the arity. A type argument can be a function
  -- type, and the call does not take the arguments of that function.
  let arity = foreignCallArity env (foreignTypeBody env (foreignCallType call))
  unless (length arguments == arity) (Left (LintFailure ("foreign call has " <> show (length arguments) <> " arguments for an arity of " <> show arity <> ": " <> show (foreignCallName call))))
  foldM applyValueArgument instantiated arguments
  where
    applyTypeArgument functionType argument = do
      argumentKind <- lintType env argument
      case viewForAll env functionType of
        Just (binder, body) -> do
          unless (kindsCompatible env argument (binderType binder) argumentKind) (Left (KindMismatch "foreign call type argument" (binderType binder) argumentKind))
          Right (substType (binderName binder) argument body)
        Nothing -> Left (LintFailure ("foreign call has too many type arguments: " <> show (foreignCallName call)))
    applyValueArgument functionType argument =
      case viewFun env functionType of
        Just (_, _, expected, result) -> do
          checkExpr env "foreign call argument" expected argument
          Right result
        Nothing -> Left (LintFailure ("foreign call has too many arguments: " <> show (foreignCallName call)))

-- | The type after the leading binders of a foreign type.
foreignTypeBody :: TypeEnv -> Type -> Type
foreignTypeBody env ty =
  case viewForAll env ty of
    Just (_, body) -> foreignTypeBody env body
    Nothing -> ty

-- | The number of arrows of a foreign type after its binders.
foreignCallArity :: TypeEnv -> Type -> Int
foreignCallArity env ty =
  case viewFun env ty of
    Just (_, _, _, result) -> 1 + foreignCallArity env result
    Nothing -> 0

lookupTerm :: TypeEnv -> Name -> Either LintError Type
lookupTerm env name =
  case lookupBinderType env name of
    Just ty -> Right ty
    Nothing ->
      case lookupHeaderType env name of
        Just ty -> Right ty
        Nothing -> Left (UnboundName name)

lintNonRecBind :: TypeEnv -> Bind -> Either LintError TypeEnv
lintNonRecBind env bind = do
  _ <- lintType env (binderType (bindBinder bind))
  checkExpr env "let binding" (binderType (bindBinder bind)) (bindRhs bind)
  bindLocal env (bindBinder bind)

lintRecRhs :: TypeEnv -> Bind -> Either LintError ()
lintRecRhs env bind = checkExpr env "rec binding" (binderType (bindBinder bind)) (bindRhs bind)

lintCase :: TypeEnv -> Expr -> Binder -> Type -> [Alt] -> Either LintError Type
lintCase env scrutinee binder resultType alts = do
  checkExpr env "case binder" (binderType binder) scrutinee
  caseEnv <- bindLocal env binder
  _ <- representationOf env resultType
  mapM_ (lintAlt caseEnv (binderType binder) resultType) alts
  Right resultType

lintAlt :: TypeEnv -> Type -> Type -> Alt -> Either LintError ()
lintAlt env scrutType expected alt =
  case altCon alt of
    AltDefault -> do
      unless (null (altTypeBinders alt)) (Left (LintFailure "default alternative has type binders"))
      unless (null (altBinders alt)) (Left (LintFailure "default alternative has field binders"))
      checkExpr env "case alternative" expected (altRhs alt)
    AltLit literal -> do
      unless (null (altTypeBinders alt)) (Left (LintFailure "literal alternative has type binders"))
      unless (null (altBinders alt)) (Left (LintFailure "literal alternative has field binders"))
      matchLiteralAlternative env scrutType literal
      checkExpr env "case alternative" expected (altRhs alt)
    AltData name ->
      case lookupHeaderType env name of
        Nothing -> Left (UnboundName name)
        Just constructorType -> do
          (existentials, fields) <- matchConstructor env constructorType scrutType
          scopeEnv <- foldM bindLocal env (altTypeBinders alt)
          mapM_ (lintType scopeEnv . binderType) (altBinders alt)
          unless (length existentials == length (altTypeBinders alt)) (Left (LintFailure ("case alternative type binder count does not match constructor: " <> show name)))
          unless (length fields == length (altBinders alt)) (Left (LintFailure ("case alternative binder count does not match constructor: " <> show name)))
          (envEx, substitution) <- foldM (bindExistential name) (env, Map.empty) (zip existentials (altTypeBinders alt))
          envFields <- foldM (bindField name) envEx (zip (map (substTypes substitution) fields) (altBinders alt))
          checkExpr envFields "case alternative" expected (altRhs alt)

matchLiteralAlternative :: TypeEnv -> Type -> Literal -> Either LintError ()
matchLiteralAlternative = checkLiteral

bindField :: Name -> TypeEnv -> (Type, Binder) -> Either LintError TypeEnv
bindField constructorName env (expected, binder) = do
  env' <- bindLocal env binder
  unless (typesEqual env expected (binderType binder)) (Left (TypeMismatch ("case alternative binder for " <> show constructorName) expected (binderType binder)))
  Right env'

bindExistential :: Name -> (TypeEnv, Map Name Type) -> (Binder, Binder) -> Either LintError (TypeEnv, Map Name Type)
bindExistential constructorName (env, substitution) (expected, actual) = do
  unless (nameSort (binderName actual) == SortTypeVariable) (Left (LintFailure ("case alternative type binder has an invalid name sort: " <> show constructorName)))
  env' <- bindLocal env actual
  let expectedKind = substTypes substitution (binderType expected)
  unless (typesEqual env' expectedKind (binderType actual)) (Left (KindMismatch ("case alternative type binder for " <> show constructorName) expectedKind (binderType actual)))
  Right (env', Map.insert (binderName expected) (TyVar (binderName actual)) substitution)

matchConstructor :: TypeEnv -> Type -> Type -> Either LintError ([Binder], [Type])
matchConstructor env constructorType scrutType = do
  let (foralls, fields, result) = splitConType env constructorType
  subst <- matchExpected env (map binderName foralls) Map.empty result scrutType
  let existentials = [binder {binderType = substTypes subst (binderType binder)} | binder <- foralls, binderName binder `Map.notMember` subst]
      substituted = map (substTypes subst) fields
  Right (existentials, substituted)

splitConType :: TypeEnv -> Type -> ([Binder], [Type], Type)
splitConType env ty =
  case ty of
    TyForAll binder body ->
      let (binders, fields, result) = splitConType env body
       in (binder : binders, fields, result)
    TyFun _ _ argument body ->
      let (binders, fields, result) = splitConType env body
       in (binders, argument : fields, result)
    other ->
      let reduced = reduceType env other
       in if reduced == other
            then ([], [], other)
            else splitConType env reduced

matchExpected :: TypeEnv -> [Name] -> Map Name Type -> Type -> Type -> Either LintError (Map Name Type)
matchExpected env foralls subst expected actual =
  case (expected, actual) of
    (TyVar name, _)
      | name `elem` foralls ->
          case Map.lookup name subst of
            Nothing -> Right (Map.insert name actual subst)
            Just previous -> do
              unless (typesEqual env previous actual) (Left (TypeMismatch "constructor result" previous actual))
              Right subst
    _ -> matchReduced env foralls subst (reduceType env expected) (reduceType env actual)

matchReduced :: TypeEnv -> [Name] -> Map Name Type -> Type -> Type -> Either LintError (Map Name Type)
matchReduced env foralls subst expected actual =
  case (expected, actual) of
    (TyVar name, _)
      | name `elem` foralls -> matchExpected env foralls subst expected actual
      | TyVar other <- actual,
        name == other ->
          Right subst
      | otherwise -> Left (TypeMismatch "constructor result" expected actual)
    (TyCon left, TyCon right)
      | left == right -> Right subst
    (TyApp function1 argument1, TyApp function2 argument2) -> do
      subst' <- matchExpected env foralls subst function1 function2
      matchExpected env foralls subst' argument1 argument2
    (TyFun r1a r2a a1 b1, TyFun r1b r2b a2 b2) -> do
      subst1 <- matchExpected env foralls subst r1a r1b
      subst2 <- matchExpected env foralls subst1 r2a r2b
      subst3 <- matchExpected env foralls subst2 a1 a2
      matchExpected env foralls subst3 b1 b2
    (TyEq a1 b1, TyEq a2 b2) -> do
      subst' <- matchExpected env foralls subst a1 a2
      matchExpected env foralls subst' b1 b2
    _
      | typesEqual env expected actual -> Right subst
      | otherwise -> Left (TypeMismatch "constructor result" expected actual)

-- | Equality evidence cannot use representation equality axioms.
lintNominalCoercion :: TypeEnv -> Coercion -> Either LintError ()
lintNominalCoercion env proof =
  case proof of
    CoVar _ -> Right ()
    CoRefl _ -> Right ()
    CoSym inner -> lintNominalCoercion env inner
    CoTrans left right -> lintNominalCoercion env left >> lintNominalCoercion env right
    CoApp function argument -> lintNominalCoercion env function >> lintNominalCoercion env argument
    CoFun domain range -> lintNominalCoercion env domain >> lintNominalCoercion env range
    CoTyConApp _ arguments -> mapM_ (lintNominalCoercion env) arguments
    CoAxiom name _ ->
      case Map.lookup name (teAxioms env) of
        Just declaration | axiomRole declaration == Nominal -> Right ()
        _ -> Left (LintFailure "equality evidence requires a nominal axiom")

coercionEndpoints :: TypeEnv -> Coercion -> Either LintError (Type, Type)
coercionEndpoints env coercion =
  case coercion of
    CoVar name -> do
      ty <- lookupTerm env name
      case reduceType env ty of
        TyEq left right -> Right (left, right)
        _ -> Left (LintFailure ("coercion variable does not have an equality type: " <> show name))
    CoRefl ty -> do
      _ <- lintType env ty
      Right (ty, ty)
    CoSym inner -> do
      (left, right) <- coercionEndpoints env inner
      Right (right, left)
    CoTrans left right -> do
      (from, middleLeft) <- coercionEndpoints env left
      (middleRight, to) <- coercionEndpoints env right
      unless (typesEqual env middleLeft middleRight) (Left (TypeMismatch "coercion transitivity" middleLeft middleRight))
      Right (from, to)
    CoApp function argument -> do
      lintNominalCoercion env function
      lintNominalCoercion env argument
      (leftFunction, rightFunction) <- coercionEndpoints env function
      (leftArgument, rightArgument) <- coercionEndpoints env argument
      let left = TyApp leftFunction leftArgument
          right = TyApp rightFunction rightArgument
      leftKind <- lintType env left
      rightKind <- lintType env right
      unless (typesEqual env leftKind rightKind) (Left (KindMismatch "application coercion" leftKind rightKind))
      Right (left, right)
    CoFun domain range -> do
      (leftDomain, rightDomain) <- coercionEndpoints env domain
      (leftRange, rightRange) <- coercionEndpoints env range
      left <- functionEndpoint leftDomain leftRange
      right <- functionEndpoint rightDomain rightRange
      Right (left, right)
      where
        functionEndpoint argument result = do
          _ <- lintType env argument
          _ <- lintType env result
          argumentRep <- maybe (Left (LintFailure "function coercion domain has no runtime representation")) Right (repOf env argument)
          resultRep <- maybe (Left (LintFailure "function coercion range has no runtime representation")) Right (repOf env result)
          let ty = TyFun argumentRep resultRep argument result
          _ <- lintType env ty
          Right ty
    CoTyConApp name arguments -> do
      header <- case lookupHeaderType env name of
        Nothing -> Left (UnboundName name)
        Just ty -> Right ty
      pairs <- mapM (coercionEndpoints env) arguments
      checkTyConCoercion env header pairs
      Right (List.foldl' TyApp (TyCon name) (map fst pairs), List.foldl' TyApp (TyCon name) (map snd pairs))
    CoAxiom name arguments ->
      case Map.lookup name (teAxioms env) of
        Nothing -> Left (UnboundName name)
        Just declaration -> do
          unless (length arguments == length (axiomBinders declaration)) (Left (LintFailure ("coercion axiom arity mismatch: " <> show name)))
          mapM_ (lintType env) arguments
          let subst = Map.fromList (zip (map binderName (axiomBinders declaration)) arguments)
          mapM_
            ( \(binder, argument) -> do
                argumentKind <- lintType env argument
                unless (typesEqual env (substTypes subst (binderType binder)) argumentKind) (Left (KindMismatch "coercion axiom argument" (binderType binder) argumentKind))
            )
            (zip (axiomBinders declaration) arguments)
          Right (substTypes subst (axiomLeft declaration), substTypes subst (axiomRight declaration))

checkTyConCoercion :: TypeEnv -> Type -> [(Type, Type)] -> Either LintError ()
checkTyConCoercion env = go
  where
    go ty [] =
      case viewForAll env ty of
        Just {} -> Left (LintFailure "type constructor coercion arity mismatch")
        Nothing ->
          case viewFun env ty of
            Just {} -> Left (LintFailure "type constructor coercion arity mismatch")
            Nothing -> Right ()
    go ty ((left, right) : rest) =
      case viewForAll env ty of
        Just (binder, body) -> do
          checkCoercionArgumentKind env (binderType binder) left right
          go (substType (binderName binder) left body) rest
        Nothing ->
          case viewFun env ty of
            Just (_, _, expected, result) -> do
              checkCoercionArgumentKind env expected left right
              go result rest
            Nothing -> Left (LintFailure "type constructor coercion arity mismatch")

checkCoercionArgumentKind :: TypeEnv -> Type -> Type -> Type -> Either LintError ()
checkCoercionArgumentKind env expected left right = do
  leftKind <- lintType env left
  rightKind <- lintType env right
  unless (typesEqual env expected leftKind) (Left (KindMismatch "type constructor coercion argument" expected leftKind))
  unless (typesEqual env expected rightKind) (Left (KindMismatch "type constructor coercion argument" expected rightKind))

representationOf :: TypeEnv -> Type -> Either LintError Type
representationOf env ty = do
  kind <- lintType env ty
  case reduceType env kind of
    TyApp (TyCon name) representation
      | name == typeConstructor (tePrimPackage env) -> Right representation
    other -> Left (LintFailure ("term type does not have a TYPE representation: " <> show other))

eitherToList :: Either LintError a -> [LintError]
eitherToList = either pure (const [])
