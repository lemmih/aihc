{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Conservative lowering from System FC to GRIN.
module Aihc.Grin.Lower
  ( lowerProgram,
  )
where

import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as TypeOf
import Aihc.Fc.Wired qualified as Wired
import Aihc.Grin.Anf (normalizeGrinProgram)
import Aihc.Grin.Syntax
import Aihc.Grin.Tidy (tidyGrinProgram)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, gets, mapStateT, modify', runStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data LowerEnv = LowerEnv
  { lowerTypes :: !TypeOf.TypeEnv,
    lowerLocals :: !(Map Fc.Name [GrinVar]),
    lowerTypeSubstitution :: !(Map Fc.Name Fc.Type),
    lowerGlobalNames :: !(Map Fc.Name Text),
    lowerConstructorArities :: !(Map Fc.Name Int),
    lowerLocalFunctions :: !(Map Fc.Name LocalFunction)
  }

-- | A top-level function of this module. Its entry function is named before
-- any code is lowered, so a call of it compiles to a direct call and a
-- suspension of it to a plain thunk node.
data LocalFunction = LocalFunction
  { localFunctionEntry :: !FunctionName,
    -- | The runtime layout of every logical parameter.
    localFunctionLayouts :: ![[GrinRep]],
    localFunctionResultRep :: !GrinRep
  }

localFunctionArity :: LocalFunction -> Int
localFunctionArity = length . localFunctionLayouts

data LowerState = LowerState
  { lowerNextUnique :: !Int,
    -- | The top-level value that lowering works on. Each function that this
    -- value needs takes its name from this name.
    lowerCurrentValue :: !Text,
    lowerUsedFunctions :: !(Set FunctionName),
    lowerFunctionsRev :: ![GrinFunction],
    -- | The primitives that the module calls, by name.
    lowerPrimitives :: !(Map Text (GrinVar, Int)),
    -- | The C functions that the module calls, by the name of their import.
    lowerForeignCalls :: !(Map Text GrinForeignCall),
    -- | The function of each foreign import that the module applies to too
    -- few arguments. The function takes every argument of the import.
    lowerForeignFunctions :: !(Map Text FunctionName)
  }

type LowerM = StateT LowerState (Either String)

data TopParts = TopParts
  { topConstructors :: ![(Text, [[GrinRep]])],
    topGlobals :: ![(Text, GrinNode)]
  }

instance Semigroup TopParts where
  left <> right =
    TopParts
      { topConstructors = topConstructors left <> topConstructors right,
        topGlobals = topGlobals left <> topGlobals right
      }

instance Monoid TopParts where
  mempty = TopParts [] []

lowerProgram :: Fc.Program -> Either String GrinProgram
lowerProgram program = do
  primPackage <- maybe (Left "System FC program needs a GHC.Types scope") Right (Wired.primPackageFromScopes (Fc.programScopes program))
  let types = TypeOf.typeEnvFromProgram primPackage program
      globals = globalNameTable types
      constructorArities = constructorArityTable types
      baseEnv = LowerEnv types Map.empty Map.empty globals constructorArities Map.empty
      initialState = LowerState (-1000000000) "" Set.empty [] Map.empty Map.empty Map.empty
  (parts, finalState) <- flip runStateT initialState $ do
    localFunctions <- localFunctionTable baseEnv program
    let env = baseEnv {lowerLocalFunctions = localFunctions}
    mconcat <$> mapM (lowerDecl env) (Fc.programDecls program)
  pure
    ( tidyGrinProgram
        ( normalizeGrinProgram
            GrinProgram
              { grinConstructors = topConstructors parts,
                grinPrimitives = Map.elems (lowerPrimitives finalState),
                grinForeignCalls = Map.elems (lowerForeignCalls finalState),
                grinGlobals = topGlobals parts,
                grinFunctions = reverse (lowerFunctionsRev finalState)
              }
        )
    )

lowerDecl :: LowerEnv -> Fc.Decl -> LowerM TopParts
lowerDecl env declaration =
  case declaration of
    Fc.DeclType value -> lowerTypeDecl env value
    Fc.DeclVal value ->
      withLowerContext ("value " <> show (Fc.valName value)) $
        withCurrentValue (Fc.valName value) (lowerValueDecl env value)
    Fc.DeclSynonym {} -> pure mempty
    Fc.DeclAxiom {} -> pure mempty

withLowerContext :: String -> LowerM a -> LowerM a
withLowerContext context =
  mapStateT (either (Left . ((context <> ": ") <>)) Right)

-- | Lower one top-level value. Every function that this value needs takes its
-- name from the value, so that a reader can find the source of the code.
withCurrentValue :: Fc.Name -> LowerM a -> LowerM a
withCurrentValue name action = do
  modify' (\state -> state {lowerCurrentValue = Fc.nameText name})
  result <- action
  modify' (\state -> state {lowerCurrentValue = ""})
  pure result

lowerTypeDecl :: LowerEnv -> Fc.TypeDecl -> LowerM TopParts
lowerTypeDecl env declaration = do
  converted <- mapM lowerConstructor (Fc.typeCons declaration)
  pure
    mempty
      { topConstructors = concatMap first converted,
        topGlobals = concatMap second converted
      }
  where
    first (constructors, _) = constructors
    second (_, globals) = globals
    lowerConstructor constructor = do
      let name = Fc.conName constructor
          (typeBinders, monotype) = splitForAlls (applySubstitution env (Fc.conType constructor))
          constructorEnv = foldl extendTypeBinder env typeBinders
      if "(#" `T.isPrefixOf` Fc.nameText name
        then pure ([], [])
        else do
          fieldTypes <- liftEither (constructorArgumentTypes monotype)
          fieldLayouts <- mapM (liftEither . runtimeComponents constructorEnv) fieldTypes
          resultType <- liftEither (constructorResultType monotype)
          resultRep <- liftEither (runtimeRep constructorEnv resultType)
          case resultRep of
            TupleRep {} -> pure ([], [])
            _ -> do
              globalName <- lookupGlobalName env name
              let tag = constructorTag name
              pure ([(tag, fieldLayouts)], [(globalName, GrinNode (GrinConstructor tag (length fieldTypes)) [])])

lowerValueDecl :: LowerEnv -> Fc.ValDecl -> LowerM TopParts
lowerValueDecl env declaration = do
  representation <- liftEither (runtimeRep env (Fc.valType declaration))
  if representation /= liftedGrinRep
    then throwLower ("GRIN does not support an unlifted top-level value: " <> show (Fc.valName declaration))
    else do
      globalName <- lookupGlobalName env (Fc.valName declaration)
      node <-
        case Map.lookup (Fc.valName declaration) (lowerLocalFunctions env) of
          Just function -> makeClosure env (Just (localFunctionEntry function)) (Fc.valBody declaration)
          Nothing -> lazyNode env (Fc.nameText (Fc.valName declaration)) (Fc.valBody declaration)
      pure mempty {topGlobals = [(globalName, node)]}

isFunctionExpression :: Fc.Expr -> Bool
isFunctionExpression = (> 0) . functionArity

functionArity :: Fc.Expr -> Int
functionArity expression =
  case expression of
    Fc.ExLam _ body -> 1 + functionArity body
    Fc.ExTyLam _ body -> functionArity body
    _ -> 0

-- | Lower a foreign call. A call that gives every argument of the import
-- lowers to the primitive or C call in place. A call that gives fewer
-- arguments stores a closure of a function that takes every argument. This
-- happens when the source type hides an argument, for example the state
-- token of an @IO@ result.
lowerForeignCallExpr :: LowerEnv -> Fc.ForeignCall -> [Fc.Type] -> [Fc.Expr] -> LowerM GrinExpr
lowerForeignCallExpr env call types arguments = do
  -- The foreign type is closed. The type arguments go into it directly,
  -- not into the environment: a binder of the foreign type can have the
  -- name of a binder in a constructor header, and a substitution in the
  -- environment would rewrite that header too.
  let name = Fc.foreignCallName call
      (typeBinders, monotype) = splitForAlls (Fc.foreignCallType call)
  when (length types > length typeBinders) $
    throwLower ("GRIN foreign call has too many type arguments: " <> T.unpack (Fc.nameText name))
  let (instantiated, remaining) = splitAt (length types) typeBinders
      substitution = Map.fromList [(Fc.binderName binder, applySubstitution env argument) | (binder, argument) <- zip instantiated types]
      instantiatedType = TypeOf.substTypes substitution monotype
      foreignEnv = defaultRuntimeReps (foldl extendTypeBinder env remaining) remaining
      declaredEnv = defaultRuntimeReps (foldl extendTypeBinder env typeBinders) typeBinders
  axioms <- foreignAxiomDeclarations foreignEnv (Fc.foreignCallDependencies call)
  let constructors = foreignConstructorNames (Fc.foreignCallDependencies call)
  -- The declared type gives the arity. A type argument can be a function
  -- type, and the call does not take the arguments of that function.
  (declaredArguments, _) <- splitOperationalFunctionType declaredEnv axioms monotype
  (argumentTypes, resultType) <- splitOperationalArrows foreignEnv axioms (length declaredArguments) instantiatedType
  case compare (length arguments) (length argumentTypes) of
    -- The result of the call is a function of the remaining arguments, for
    -- example when @unsafeCoerce#@ gives a state transformer.
    GT -> do
      let (callArguments, extraArguments) = splitAt (length argumentTypes) arguments
      resultRep <- expressionRuntimeRep env (Fc.ExForeignCall call types arguments)
      evaluated <- freshVar "function_whnf" liftedGrinRep
      functionExpression <- lowerForeignCallExpr env call types callArguments
      rest <- lowerDynamicApplication env resultRep (GrinVarValue evaluated) extraArguments
      pure (GrinBind [evaluated] functionExpression rest)
    EQ
      | Fc.Prim <- Fc.foreignCallConvention call,
        Map.member (Fc.nameText name) specialPrimitiveArities -> do
          -- A call that never returns keeps a polymorphic representation.
          resultRep <- expressionRuntimeRep env (Fc.ExForeignCall call types arguments)
          lowerSpecialApplication env resultRep (Fc.nameText name) arguments
      | otherwise -> do
          resultRep <- liftEither (runtimeRep foreignEnv resultType)
          lowerArgumentGroups env arguments $ \valueGroups ->
            lowerForeignCallBody foreignEnv call axioms constructors argumentTypes valueGroups resultType resultRep
    LT -> do
      resultRep <- liftEither (runtimeRep foreignEnv resultType)
      functionName <- foreignFunction foreignEnv call axioms constructors argumentTypes resultType resultRep
      layouts <- mapM (liftEither . runtimeComponents foreignEnv) argumentTypes
      lowerArguments env arguments $ \values ->
        pure (GrinStore (GrinNode (GrinClosure functionName (drop (length arguments) layouts)) values))

-- | Lower the arguments of a foreign call, one group of values for each
-- argument.
lowerArgumentGroups :: LowerEnv -> [Fc.Expr] -> ([[GrinValue]] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgumentGroups env = go []
  where
    go groups [] continuation = continuation (reverse groups)
    go groups (argument : arguments) continuation =
      lowerArgument env argument (\values -> go (values : groups) arguments continuation)

-- | The body of a foreign call with the values of every argument.
lowerForeignCallBody :: LowerEnv -> Fc.ForeignCall -> [Fc.AxiomDecl] -> [Fc.Name] -> [Fc.Type] -> [[GrinValue]] -> Fc.Type -> GrinRep -> LowerM GrinExpr
lowerForeignCallBody env call axioms constructors argumentTypes valueGroups resultType resultRep = do
  let name = Fc.foreignCallName call
      arity = length argumentTypes
  case Fc.foreignCallConvention call of
    Fc.Prim -> do
      unless (Fc.nameText name `elem` compilerPrimitives) $
        declarePrimitive (GrinVar (Fc.nameText name) (-2000000000 + arity) resultRep, arity)
      lowerPrimitiveBody resultRep (Fc.nameText name) valueGroups
    Fc.CCall specification -> do
      let foreignCall = lowerForeignCall name specification
      declareForeignCall foreignCall
      (expression, adapterPrimitives) <- lowerForeignBody env axioms constructors foreignCall argumentTypes valueGroups resultType
      mapM_ declarePrimitive adapterPrimitives
      pure expression

-- | The function of a foreign import that takes every argument of the
-- import. The module has one such function for each import that it applies
-- to too few arguments.
foreignFunction :: LowerEnv -> Fc.ForeignCall -> [Fc.AxiomDecl] -> [Fc.Name] -> [Fc.Type] -> Fc.Type -> GrinRep -> LowerM FunctionName
foreignFunction env call axioms constructors argumentTypes resultType resultRep = do
  let name = Fc.foreignCallName call
      key = stableGlobalName name
  known <- gets (Map.lookup key . lowerForeignFunctions)
  case known of
    Just functionName -> pure functionName
    Nothing -> do
      functionName <- freshFunction (Fc.nameText name <> "_foreign")
      modify' (\state -> state {lowerForeignFunctions = Map.insert key functionName (lowerForeignFunctions state)})
      argumentGroups <-
        mapM
          (\(index, argumentType) -> freshVarsForType env ("foreign_argument_" <> T.pack (show index), argumentType))
          (zip [0 :: Int ..] argumentTypes)
      body <- lowerForeignCallBody env call axioms constructors argumentTypes (map (map GrinVarValue) argumentGroups) resultType resultRep
      emitFunction
        GrinFunction
          { grinFunctionName = functionName,
            grinFunctionParameters = concat argumentGroups,
            grinFunctionResultRep = resultRep,
            grinFunctionBody = body
          }
      pure functionName

declarePrimitive :: (GrinVar, Int) -> LowerM ()
declarePrimitive primitive@(var, _) =
  modify' (\state -> state {lowerPrimitives = Map.insert (grinVarName var) primitive (lowerPrimitives state)})

declareForeignCall :: GrinForeignCall -> LowerM ()
declareForeignCall foreignCall = do
  known <- gets (Map.lookup (grinForeignCallName foreignCall) . lowerForeignCalls)
  case known of
    Just existing
      | existing /= foreignCall ->
          throwLower ("GRIN module calls two different C functions under one name: " <> T.unpack (grinForeignCallName foreignCall))
    _ -> modify' (\state -> state {lowerForeignCalls = Map.insert (grinForeignCallName foreignCall) foreignCall (lowerForeignCalls state)})

foreignAxiomDeclarations :: LowerEnv -> [Fc.ForeignImportDependency] -> LowerM [Fc.AxiomDecl]
foreignAxiomDeclarations env dependencies =
  mapM lookupAxiom [name | Fc.ForeignAxiom name <- dependencies]
  where
    lookupAxiom name =
      case Map.lookup name (TypeOf.teAxioms (lowerTypes env)) of
        Just axiom -> pure axiom
        Nothing -> throwLower ("GRIN cannot find an explicit foreign axiom: " <> show name)

foreignConstructorNames :: [Fc.ForeignImportDependency] -> [Fc.Name]
foreignConstructorNames dependencies =
  [name | Fc.ForeignConstructor name <- dependencies]

compilerPrimitives :: [Text]
compilerPrimitives = ["aihcExit#", "unsafeCoerce#", "raise#", "catch#", "runRW#"]

-- | A primitive call with the values of every argument. A compiler primitive
-- never comes here: its call is always saturated, so 'lowerSpecialApplication'
-- lowers it from its argument expressions.
lowerPrimitiveBody :: GrinRep -> Text -> [[GrinValue]] -> LowerM GrinExpr
lowerPrimitiveBody resultRep name valueGroups
  | name `elem` compilerPrimitives = throwLower ("GRIN cannot lower a compiler primitive from values: " <> T.unpack name)
  | otherwise = pure (GrinPrimitiveCall resultRep name (concat valueGroups))

-- | Apply a state transformer to the real world token. The token has no
-- runtime value, so the transformer gets no argument.
lowerRunRW :: GrinRep -> GrinValue -> LowerM GrinExpr
lowerRunRW resultRep action = do
  evaluatedAction <- freshVar "run_rw_action" liftedGrinRep
  pure
    ( GrinBind
        [evaluatedAction]
        (GrinEval liftedGrinRep action)
        (GrinApply resultRep (GrinVarValue evaluatedAction) [])
    )

-- | Lower a foreign call body. The result also lists the primitives that
-- the argument adapters use, so the module declares them.
lowerForeignBody :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> GrinForeignCall -> [Fc.Type] -> [[GrinValue]] -> Fc.Type -> LowerM (GrinExpr, [(GrinVar, Int)])
lowerForeignBody env axioms constructors foreignCall argumentTypes valueGroups resultType = do
  operands <- concat <$> zipWithM (sourceValues env) argumentTypes valueGroups
  resultValues <- sourceValueTypes env resultType
  let signature = grinForeignCallSignature foreignCall
      expectedOperands = grinForeignOperandReps signature
      resultReps = grinForeignCallResultReps signature
      adapterPrimitives =
        [ (GrinVar byteArrayContentsPrimitive (-2000000000 + 1) AddrRep, 1)
        | any (\((_, value), expectedRep) -> isByteArrayOperand value expectedRep) (zip operands expectedOperands)
        ]
  if length operands /= length expectedOperands
    then throwLower ("GRIN foreign source arguments do not match the C ABI: " <> T.unpack (grinForeignCallName foreignCall))
    else case (resultValues, resultReps) of
      ([(resultValueType, resultValueRep)], [foreignResultRep]) -> do
        expression <-
          adaptForeignOperands env axioms constructors (zip operands expectedOperands) $ \values ->
            adaptForeignResult env axioms constructors resultValueType resultValueRep foreignResultRep (GrinForeignCallExpr foreignCall values)
        pure (expression, adapterPrimitives)
      -- A C procedure gives no value; the Haskell result is the nullary
      -- constructor of its type, which is the unit type in practice.
      ([(resultValueType, resultValueRep)], [])
        | isLiftedRuntimeRep resultValueRep -> do
            tag <- findNullaryConstructor env axioms constructors resultValueType
            expression <-
              adaptForeignOperands env axioms constructors (zip operands expectedOperands) $ \values ->
                pure (GrinBind [] (GrinForeignCallExpr foreignCall values) (GrinStore (GrinNode (GrinConstructor tag 0) [])))
            pure (expression, adapterPrimitives)
      _ -> throwLower ("GRIN foreign result does not match the C ABI: " <> T.unpack (grinForeignCallName foreignCall))

-- | The primitive that gives the payload address of a byte array.
byteArrayContentsPrimitive :: Text
byteArrayContentsPrimitive = "byteArrayContents#"

-- | A byte array value that a foreign call receives as an address.
isByteArrayOperand :: GrinValue -> GrinRep -> Bool
isByteArrayOperand value expectedRep =
  grinValueRuntimeRep value == BoxedRep Unlifted && expectedRep == AddrRep

sourceValues :: LowerEnv -> Fc.Type -> [GrinValue] -> LowerM [(Fc.Type, GrinValue)]
sourceValues env sourceType values = do
  types <- sourceValueTypes env sourceType
  if length types == length values
    then pure (zip (map fst types) values)
    else throwLower ("GRIN cannot match source values to type: " <> show sourceType)

sourceValueTypes :: LowerEnv -> Fc.Type -> LowerM [(Fc.Type, GrinRep)]
sourceValueTypes env sourceType = do
  representation <- liftEither (runtimeRep env sourceType)
  case representation of
    TupleRep fields -> do
      let (_, arguments) = collectTypeApplications (reduce env sourceType)
          fieldTypes = drop (length arguments - length fields) arguments
      if length fieldTypes /= length fields
        then throwLower ("GRIN cannot find unboxed tuple fields for type: " <> show sourceType)
        else fmap concat (zipWithM sourceFieldTypes fieldTypes fields)
    _ -> pure [(sourceType, component) | component <- runtimeRepComponents representation]
  where
    sourceFieldTypes fieldType fieldRep =
      case runtimeRepComponents fieldRep of
        [] -> pure []
        [component] -> pure [(fieldType, component)]
        _ -> throwLower ("GRIN does not support a nested tuple foreign value: " <> show fieldType)

adaptForeignOperands :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> [((Fc.Type, GrinValue), GrinRep)] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
adaptForeignOperands env axioms constructors operands continuation = go [] operands
  where
    go values [] = continuation (reverse values)
    go values (((sourceType, value), expectedRep) : rest)
      | grinValueRuntimeRep value == expectedRep = go (value : values) rest
      -- A byte array argument passes the address of its payload.
      | isByteArrayOperand value expectedRep = do
          contents <- freshVar "foreign_contents" AddrRep
          body <- go (GrinVarValue contents : values) rest
          pure (GrinBind [contents] (GrinPrimitiveCall AddrRep byteArrayContentsPrimitive [value]) body)
      | isLiftedRuntimeRep (grinValueRuntimeRep value) = do
          (tag, fieldRep) <- findUnaryConstructor env axioms constructors sourceType expectedRep
          evaluated <- freshVar "foreign_box" liftedGrinRep
          caseBinder <- freshVar "foreign_box_case" liftedGrinRep
          field <- freshVar "foreign_field" fieldRep
          body <- go (GrinVarValue field : values) rest
          pure
            ( GrinBind
                [evaluated]
                (GrinEval liftedGrinRep value)
                ( GrinCase
                    (GrinVarValue evaluated)
                    caseBinder
                    [GrinAlt (GrinDataAlt tag) [field] body]
                )
            )
      | otherwise = throwLower ("GRIN cannot adapt a foreign argument representation: " <> show sourceType)

adaptForeignResult :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> Fc.Type -> GrinRep -> GrinRep -> GrinExpr -> LowerM GrinExpr
adaptForeignResult env axioms constructors sourceType sourceRep foreignRep foreignExpression
  | sourceRep == foreignRep = pure foreignExpression
  | isLiftedRuntimeRep sourceRep = do
      (tag, fieldRep) <- findUnaryConstructor env axioms constructors sourceType foreignRep
      result <- freshVar "foreign_result" fieldRep
      pure
        ( GrinBind
            [result]
            foreignExpression
            (GrinStore (GrinNode (GrinConstructor tag 0) [GrinVarValue result]))
        )
  | otherwise = throwLower ("GRIN cannot adapt a foreign result representation: " <> show sourceType)

findUnaryConstructor :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> Fc.Type -> GrinRep -> LowerM (Text, GrinRep)
findUnaryConstructor env axioms constructors resultType expectedRep =
  case listToMaybe (mapMaybe matchConstructor (foreignConstructorEntries env constructors)) of
    Just result -> pure result
    Nothing -> throwLower ("GRIN cannot find a unary constructor adapter for type: " <> show resultType <> " among the constructors " <> show constructors)
  where
    matchConstructor (name, constructorType)
      | Fc.nameSort name /= Fc.SortDataConstructor = Nothing
      | otherwise = do
          fieldTypes <- instantiateConstructorFields env axioms constructorType resultType
          case fieldTypes of
            [fieldType] ->
              case runtimeRep env fieldType of
                Right fieldRep
                  | fieldRep == expectedRep -> Just (constructorTag name, fieldRep)
                _ -> Nothing
            _ -> Nothing

-- | The constructor of a type with no fields, such as the unit constructor.
findNullaryConstructor :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> Fc.Type -> LowerM Text
findNullaryConstructor env axioms constructors resultType =
  case listToMaybe (mapMaybe matchConstructor (foreignConstructorEntries env constructors)) of
    Just tag -> pure tag
    Nothing -> throwLower ("GRIN cannot find a nullary constructor adapter for type: " <> show resultType)
  where
    matchConstructor (name, constructorType)
      | Fc.nameSort name /= Fc.SortDataConstructor = Nothing
      | otherwise = do
          fieldTypes <- instantiateConstructorFields env axioms constructorType resultType
          case fieldTypes of
            [] -> Just (constructorTag name)
            _ -> Nothing

foreignConstructorEntries :: LowerEnv -> [Fc.Name] -> [(Fc.Name, Fc.Type)]
foreignConstructorEntries env constructors =
  [ (name, constructorType)
  | name <- constructors,
    Just constructorType <- [Map.lookup name (TypeOf.teHeaders (lowerTypes env))]
  ]

instantiateConstructorFields :: LowerEnv -> [Fc.AxiomDecl] -> Fc.Type -> Fc.Type -> Maybe [Fc.Type]
instantiateConstructorFields env axioms constructorType targetType = do
  let (binders, monotype) = splitForAlls constructorType
  (fieldTypes, constructorResult) <- either (const Nothing) Just (splitFunctionType monotype)
  substitution <- matchTypeBinders env (Map.fromList [(Fc.binderName binder, Nothing) | binder <- binders]) constructorResult (applyForeignAxioms env axioms targetType)
  resolved <- sequenceA substitution
  pure (map (TypeOf.substTypes resolved) fieldTypes)

matchTypeBinders :: LowerEnv -> Map Fc.Name (Maybe Fc.Type) -> Fc.Type -> Fc.Type -> Maybe (Map Fc.Name (Maybe Fc.Type))
matchTypeBinders env substitution patternType actualType =
  case (reduce env patternType, reduce env actualType) of
    (Fc.TyVar name, actual)
      | Just current <- Map.lookup name substitution ->
          case current of
            Nothing -> Just (Map.insert name (Just actual) substitution)
            Just previous
              | TypeOf.typesEqual (lowerTypes env) previous actual -> Just substitution
              | otherwise -> Nothing
    (Fc.TyVar name, Fc.TyVar actualName)
      | name == actualName -> Just substitution
    (Fc.TyCon name, Fc.TyCon actualName)
      | name == actualName -> Just substitution
    (Fc.TyApp function argument, Fc.TyApp actualFunction actualArgument) ->
      matchTypeBinders env substitution function actualFunction
        >>= \next -> matchTypeBinders env next argument actualArgument
    (Fc.TyFun r1 r2 argument result, Fc.TyFun actualR1 actualR2 actualArgument actualResult) ->
      matchTypeBinders env substitution r1 actualR1
        >>= \s1 ->
          matchTypeBinders env s1 r2 actualR2
            >>= \s2 ->
              matchTypeBinders env s2 argument actualArgument
                >>= \s3 -> matchTypeBinders env s3 result actualResult
    (Fc.TyEq left right, Fc.TyEq actualLeft actualRight) ->
      matchTypeBinders env substitution left actualLeft
        >>= \next -> matchTypeBinders env next right actualRight
    _ -> Nothing

collectTypeApplications :: Fc.Type -> (Fc.Type, [Fc.Type])
collectTypeApplications = go []
  where
    go arguments (Fc.TyApp function argument) = go (argument : arguments) function
    go arguments function = (function, arguments)

lowerExpr :: LowerEnv -> Fc.Expr -> LowerM GrinExpr
lowerExpr env expression =
  case expression of
    Fc.ExVar name -> lowerVariable env name
    Fc.ExLit literal -> GrinConstant . pure . GrinLitValue <$> lowerLiteral env literal
    Fc.ExApp function argument -> lowerApplication env function argument
    Fc.ExTyApp (Fc.ExTyLam binder body) argument -> lowerExpr (substituteTypeBinder env binder argument) body
    Fc.ExTyApp function _ -> lowerExpr env function
    Fc.ExLam {} -> GrinStore <$> makeClosure env Nothing expression
    Fc.ExTyLam binder body -> lowerExpr (extendTypeBinder env binder) body
    Fc.ExLet binding body -> lowerLet env binding body
    Fc.ExRec bindings body -> lowerRec env bindings body
    Fc.ExCase scrutinee binder _ alternatives -> lowerCase env scrutinee binder alternatives
    Fc.ExCast inner _ -> lowerExpr env inner
    Fc.ExForeignCall call types arguments -> lowerForeignCallExpr env call types arguments

lowerVariable :: LowerEnv -> Fc.Name -> LowerM GrinExpr
lowerVariable env name = do
  ty <- lookupNameType env name
  representation <- liftEither (runtimeRep env ty)
  let components = runtimeRepComponents representation
  case Map.lookup name (lowerLocals env) of
    Just variables ->
      if isLiftedRuntimeRep representation
        then case variables of
          [variable] -> pure (GrinEval representation (GrinVarValue variable))
          _ -> throwLower ("GRIN expected one lifted local value: " <> show name)
        else pure (GrinConstant (map GrinVarValue variables))
    Nothing
      | null components -> pure (GrinConstant [])
      | otherwise -> do
          globalName <- lookupGlobalName env name
          pure (GrinEval representation (GrinGlobalValue globalName))

lowerApplication :: LowerEnv -> Fc.Expr -> Fc.Expr -> LowerM GrinExpr
lowerApplication env function argument = do
  let application = Fc.ExApp function argument
  resultRep <- expressionRuntimeRep env application
  case (resultRep, collectApplications application) of
    (_, (Fc.ExVar name, arguments))
      | Just arity <- Map.lookup (Fc.nameText name) specialPrimitiveArities,
        length arguments == arity ->
          lowerSpecialApplication env resultRep (Fc.nameText name) arguments
    (TupleRep {}, (Fc.ExVar name, arguments))
      | "(#" `T.isPrefixOf` Fc.nameText name -> lowerTupleArguments env arguments
    (_, (Fc.ExVar name, arguments))
      | resultRep == liftedGrinRep,
        not ("(#" `T.isPrefixOf` Fc.nameText name),
        Just arity <- Map.lookup name (lowerConstructorArities env),
        length arguments <= arity ->
          lowerConstructorApplication env name (arity - length arguments) arguments
    (_, (Fc.ExVar name, arguments))
      | Just localFunction <- Map.lookup name (lowerLocalFunctions env) ->
          lowerLocalFunctionApplication env resultRep name localFunction arguments
    -- A foreign call whose result is a function of more arguments, such as
    -- an @IO@ action applied to the state token, takes them in one call.
    (_, (Fc.ExForeignCall call types callArguments, arguments)) ->
      lowerForeignCallExpr env call types (callArguments <> arguments)
    _ -> do
      -- The function is needed in weak head normal form right away, so it is
      -- computed directly rather than suspended and then evaluated.
      evaluated <- freshVar "function_whnf" liftedGrinRep
      functionExpression <- lowerExpr env function
      lowerArgument env argument $ \argumentValues ->
        pure
          ( GrinBind
              [evaluated]
              functionExpression
              (GrinApply resultRep (GrinVarValue evaluated) argumentValues)
          )

collectApplications :: Fc.Expr -> (Fc.Expr, [Fc.Expr])
collectApplications expression = go expression []
  where
    go (Fc.ExApp function argument) arguments = go function (argument : arguments)
    go (Fc.ExTyApp function _) arguments = go function arguments
    go (Fc.ExCast function _) arguments = go function arguments
    go function arguments = (function, arguments)

lowerTupleArguments :: LowerEnv -> [Fc.Expr] -> LowerM GrinExpr
lowerTupleArguments env = go []
  where
    go values [] = pure (GrinConstant values)
    go values (argument : arguments) =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments)

lowerConstructorApplication :: LowerEnv -> Fc.Name -> Int -> [Fc.Expr] -> LowerM GrinExpr
lowerConstructorApplication env name remaining = go []
  where
    go values [] = pure (GrinStore (GrinNode (GrinConstructor (constructorTag name) remaining) values))
    go values (argument : arguments) =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments)

lowerLocalFunctionApplication :: LowerEnv -> GrinRep -> Fc.Name -> LocalFunction -> [Fc.Expr] -> LowerM GrinExpr
lowerLocalFunctionApplication env resultRep name function arguments
  | length arguments < arity =
      lowerArguments env arguments $ \argumentValues ->
        pure (GrinStore (GrinNode (GrinClosure entry (drop (length arguments) (localFunctionLayouts function))) argumentValues))
  | localFunctionResultRep function == directResultRep =
      lowerArguments env saturatedArguments $ \argumentValues ->
        case remainingArguments of
          [] -> pure (GrinCall resultRep entry argumentValues)
          _ -> do
            applied <- freshVar "function_application" liftedGrinRep
            rest <- lowerDynamicApplication env resultRep (GrinVarValue applied) remainingArguments
            pure (GrinBind [applied] (GrinCall liftedGrinRep entry argumentValues) rest)
  | otherwise = do
      globalName <- lookupGlobalName env name
      lowerDynamicApplication env resultRep (GrinGlobalValue globalName) arguments
  where
    entry = localFunctionEntry function
    arity = localFunctionArity function
    (saturatedArguments, remainingArguments) = splitAt arity arguments
    directResultRep
      | null remainingArguments = resultRep
      | otherwise = liftedGrinRep

lowerDynamicApplication :: LowerEnv -> GrinRep -> GrinValue -> [Fc.Expr] -> LowerM GrinExpr
lowerDynamicApplication env resultRep = go
  where
    go functionValue [argument] = lowerArgument env argument (pure . GrinApply resultRep functionValue)
    go functionValue (argument : remaining) =
      lowerArgument env argument $ \argumentValues -> do
        applied <- freshVar "function_application" liftedGrinRep
        rest <- go (GrinVarValue applied) remaining
        pure (GrinBind [applied] (GrinApply liftedGrinRep functionValue argumentValues) rest)
    go _ [] = throwLower "GRIN local function application needs an argument"

lowerArguments :: LowerEnv -> [Fc.Expr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArguments env = go []
  where
    go values [] continuation = continuation values
    go values (argument : arguments) continuation =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments continuation)

specialPrimitiveArities :: Map Text Int
specialPrimitiveArities = Map.fromList [("aihcExit#", 2), ("unsafeCoerce#", 1), ("raise#", 1), ("catch#", 3), ("runRW#", 1)]

lowerSpecialApplication :: LowerEnv -> GrinRep -> Text -> [Fc.Expr] -> LowerM GrinExpr
lowerSpecialApplication env resultRep name arguments =
  case (name, arguments) of
    ("aihcExit#", status : state : _) ->
      lowerArgument env status $ \case
        value : _ -> lowerArgument env state (const (pure (GrinExit value)))
        [] -> throwLower "GRIN process exit requires a status value"
    ("unsafeCoerce#", value : _) -> lowerArgument env value (pure . GrinConstant)
    ("raise#", exception : _) ->
      lowerLazy env "exception" exception (pure . GrinThrow)
    ("catch#", action : handler : state : _) ->
      lowerLazy env "action" action $ \actionValue ->
        lowerLazy env "handler" handler $ \handlerValue ->
          lowerArgument env state (lowerCatch resultRep actionValue handlerValue)
    ("runRW#", action : _) ->
      lowerLazy env "action" action (lowerRunRW resultRep)
    _ -> throwLower ("GRIN cannot lower compiler primitive application: " <> T.unpack name)

lowerCatch :: GrinRep -> GrinValue -> GrinValue -> [GrinValue] -> LowerM GrinExpr
lowerCatch resultRep action handler stateValues = do
  evaluatedHandler <- freshVar "catch_handler" liftedGrinRep
  handlerCapture <- freshVar "catch_handler_capture" liftedGrinRep
  stateCaptures <- mapM (freshVar "catch_state_capture" . grinValueRuntimeRep) stateValues
  exception <- freshVar "catch_exception" liftedGrinRep
  handlerAction <- freshVar "catch_handler_action" liftedGrinRep
  evaluatedAction <- freshVar "catch_evaluated_action" liftedGrinRep
  wrapper <- freshVar "catch_handler_wrapper" liftedGrinRep
  functionName <- freshFunction "catch_handler"
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = handlerCapture : stateCaptures <> [exception],
        grinFunctionResultRep = resultRep,
        grinFunctionBody =
          GrinBind
            [handlerAction]
            (GrinApply liftedGrinRep (GrinVarValue handlerCapture) [GrinVarValue exception])
            ( GrinBind
                [evaluatedAction]
                (GrinEval liftedGrinRep (GrinVarValue handlerAction))
                (GrinApply resultRep (GrinVarValue evaluatedAction) (map GrinVarValue stateCaptures))
            )
      }
  pure
    ( GrinBind
        [evaluatedHandler]
        (GrinEval liftedGrinRep handler)
        ( GrinBind
            [wrapper]
            ( GrinStore
                ( GrinNode
                    (GrinClosure functionName [[liftedGrinRep]])
                    (GrinVarValue evaluatedHandler : stateValues)
                )
            )
            (GrinCatch resultRep action (GrinVarValue wrapper) stateValues)
        )
    )

lowerArgument :: LowerEnv -> Fc.Expr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgument env expression continuation = do
  representation <- expressionRuntimeRep env expression
  if null (runtimeRepComponents representation)
    then continuation []
    else
      if isLiftedRuntimeRep representation
        then lowerLazy env "argument" expression (continuation . (: []))
        else bindExpression env "argument" expression continuation

-- | Name the value of a lifted expression without evaluating it.
--
-- The lazy form of an expression is the cheapest thing that stands for it
-- without running it: a variable is itself, a lambda is a closure, a
-- constructor application or a call of a known function is a node whose
-- operands are named the same way, and a let floats its bindings out. Only an
-- expression whose lazy form needs code of its own, such as a case or a call
-- of an unknown function, is suspended in a function by 'makeThunk'.
lowerLazy :: LowerEnv -> Text -> Fc.Expr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerLazy env0 hint expression0 continuation =
  case expression of
    Fc.ExVar name ->
      case Map.lookup name (lowerLocals env) of
        Just [variable] -> continuation (GrinVarValue variable)
        Just _ -> throwLower ("GRIN expected one lazy local value: " <> show name)
        Nothing -> lookupGlobalName env name >>= continuation . GrinGlobalValue
    Fc.ExLam {} -> makeClosure env Nothing expression >>= storeNode
    Fc.ExLet binding body -> do
      representation <- binderRep env (Fc.bindBinder binding)
      if isLiftedRuntimeRep representation
        then lowerLetBinding env binding (\bodyEnv -> lowerLazy bodyEnv hint body continuation)
        else suspend
    Fc.ExRec bindings body -> lowerRecBindings env bindings (\bodyEnv -> lowerLazy bodyEnv hint body continuation)
    _ -> do
      shape <- lazyNodeShape env expression
      case shape of
        Just (tag, operands) -> do
          classified <- mapM (classifyOperand env) operands
          case sequence classified of
            Just lazyOperands -> lowerLazyOperands env lazyOperands (storeNode . GrinNode tag)
            Nothing -> suspend
        Nothing -> suspend
  where
    (env, expression) = stripLazyWrappers env0 expression0
    suspend = makeThunk env hint expression >>= storeNode
    storeNode node = do
      pointer <- freshVar hint liftedGrinRep
      rest <- continuation (GrinVarValue pointer)
      pure (GrinBind [pointer] (GrinStore node) rest)

-- | The node that stands for a lifted expression where no pointer can be
-- bound before it: a recursive binding or a global. The node is direct only
-- when every field is already a value; anything else is suspended.
lazyNode :: LowerEnv -> Text -> Fc.Expr -> LowerM GrinNode
lazyNode env0 hint expression0 =
  case expression of
    Fc.ExLam {} -> makeClosure env Nothing expression
    _ -> do
      shape <- lazyNodeShape env expression
      case shape of
        Just (tag, operands) -> do
          classified <- mapM (classifyOperand env) operands
          case traverse (settledOperand =<<) classified of
            Just values -> pure (GrinNode tag (concat values))
            Nothing -> makeThunk env hint expression
        Nothing -> makeThunk env hint expression
  where
    (env, expression) = stripLazyWrappers env0 expression0
    settledOperand operand =
      case operand of
        SettledOperand values -> Just values
        LazyOperand {} -> Nothing

-- | Drop the type applications, type lambdas and casts that carry no runtime
-- value, keeping the type environment they establish.
stripLazyWrappers :: LowerEnv -> Fc.Expr -> (LowerEnv, Fc.Expr)
stripLazyWrappers env expression =
  case expression of
    Fc.ExTyApp (Fc.ExTyLam binder body) argument -> stripLazyWrappers (substituteTypeBinder env binder argument) body
    Fc.ExTyApp inner _ -> stripLazyWrappers env inner
    Fc.ExTyLam binder body -> stripLazyWrappers (extendTypeBinder env binder) body
    Fc.ExCast inner _ -> stripLazyWrappers env inner
    _ -> (env, expression)

-- | The node an application allocates to when it needs no code of its own,
-- with the operands that fill its fields: a constructor application, a
-- saturated call of a known function with a lifted result, or a partial
-- application of a known function.
lazyNodeShape :: LowerEnv -> Fc.Expr -> LowerM (Maybe (GrinNodeTag, [Fc.Expr]))
lazyNodeShape env expression =
  case collectApplications expression of
    (Fc.ExVar name, arguments)
      | Map.member (Fc.nameText name) specialPrimitiveArities -> pure Nothing
      | "(#" `T.isPrefixOf` Fc.nameText name -> pure Nothing
      | Just arity <- Map.lookup name (lowerConstructorArities env),
        length arguments <= arity -> do
          representation <- expressionRuntimeRep env expression
          pure
            ( if isLiftedRuntimeRep representation
                then Just (GrinConstructor (constructorTag name) (arity - length arguments), arguments)
                else Nothing
            )
      | Just function <- Map.lookup name (lowerLocalFunctions env) ->
          pure
            ( case compare (length arguments) (localFunctionArity function) of
                LT -> Just (GrinClosure (localFunctionEntry function) (drop (length arguments) (localFunctionLayouts function)), arguments)
                EQ
                  | isLiftedRuntimeRep (localFunctionResultRep function) ->
                      Just (GrinThunk (localFunctionEntry function), arguments)
                _ -> Nothing
            )
    _ -> pure Nothing

-- | An operand of a lazily allocated node.
data LazyOperand
  = -- | Values that already exist, so naming them costs nothing.
    SettledOperand [GrinValue]
  | -- | A lifted expression that 'lowerLazy' names in its own lazy form.
    LazyOperand Fc.Expr

-- | Classify an operand, or fail when it is unlifted and not yet a value:
-- computing it would run code the surrounding node must not run.
classifyOperand :: LowerEnv -> Fc.Expr -> LowerM (Maybe LazyOperand)
classifyOperand env expression = do
  representation <- expressionRuntimeRep env expression
  case stripValueWrappers expression of
    _ | null (runtimeRepComponents representation) -> pure (Just (SettledOperand []))
    Fc.ExVar name ->
      case Map.lookup name (lowerLocals env) of
        Just variables -> pure (Just (SettledOperand (map GrinVarValue variables)))
        Nothing
          | isLiftedRuntimeRep representation -> Just . SettledOperand . pure . GrinGlobalValue <$> lookupGlobalName env name
          | otherwise -> pure Nothing
    Fc.ExLit literal
      | not (isLiftedRuntimeRep representation) -> Just . SettledOperand . pure . GrinLitValue <$> lowerLiteral env literal
    _
      | isLiftedRuntimeRep representation -> pure (Just (LazyOperand expression))
      | otherwise -> pure Nothing

lowerLazyOperands :: LowerEnv -> [LazyOperand] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerLazyOperands env = go []
  where
    go values [] continuation = continuation values
    go values (SettledOperand newValues : operands) continuation = go (values <> newValues) operands continuation
    go values (LazyOperand expression : operands) continuation =
      lowerLazy env "argument" expression (\value -> go (values <> [value]) operands continuation)

bindExpression :: LowerEnv -> Text -> Fc.Expr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
bindExpression env hint expression continuation = do
  representation <- expressionRuntimeRep env expression
  variables <- freshVars hint representation
  valueExpression <- lowerExpr env expression
  rest <- continuation (map GrinVarValue variables)
  pure (GrinBind variables valueExpression rest)

lowerLet :: LowerEnv -> Fc.Bind -> Fc.Expr -> LowerM GrinExpr
lowerLet env binding body = lowerLetBinding env binding (`lowerExpr` body)

-- | Bind one let binding and continue with the environment that sees it.
lowerLetBinding :: LowerEnv -> Fc.Bind -> (LowerEnv -> LowerM GrinExpr) -> LowerM GrinExpr
lowerLetBinding env binding continuation = do
  let binder = Fc.bindBinder binding
      hint = Fc.nameText (Fc.binderName binder)
  representation <- binderRep env binder
  if isLiftedRuntimeRep representation
    then lowerLazy env hint (Fc.bindRhs binding) $ \case
      GrinVarValue variable -> continuation (bindLocal env binder [variable])
      value -> do
        variable <- freshVar hint representation
        rest <- continuation (bindLocal env binder [variable])
        pure (GrinBind [variable] (GrinConstant [value]) rest)
    else do
      variables <- freshVars hint representation
      loweredRhs <- lowerExpr env (Fc.bindRhs binding)
      rest <- continuation (bindLocal env binder variables)
      pure (GrinBind variables loweredRhs rest)

binderRep :: LowerEnv -> Fc.Binder -> LowerM GrinRep
binderRep env binder = liftEither (runtimeRep env (applySubstitution env (Fc.binderType binder)))

lowerRec :: LowerEnv -> [Fc.Bind] -> Fc.Expr -> LowerM GrinExpr
lowerRec env bindings body = lowerRecBindings env bindings (`lowerExpr` body)

-- | Allocate a recursive binding group and continue with the environment
-- that sees it.
lowerRecBindings :: LowerEnv -> [Fc.Bind] -> (LowerEnv -> LowerM GrinExpr) -> LowerM GrinExpr
lowerRecBindings env bindings continuation = do
  variables <- mapM makeVariables bindings
  let recursiveEnv = foldl bindOne env (zip bindings variables)
  nodes <- mapM (makeBindingNode recursiveEnv) bindings
  loweredBody <- continuation recursiveEnv
  pure (GrinStoreRec (zip (concat variables) nodes) loweredBody)
  where
    makeVariables binding = do
      let binder = Fc.bindBinder binding
      representation <- binderRep env binder
      if isLiftedRuntimeRep representation
        then (: []) <$> freshVar (Fc.nameText (Fc.binderName binder)) representation
        else throwLower ("GRIN does not support an unlifted recursive binding: " <> show (Fc.binderName binder))
    bindOne current (binding, vars) = bindLocal current (Fc.bindBinder binding) vars
    makeBindingNode recursiveEnv binding = lazyNode recursiveEnv (Fc.nameText (Fc.binderName (Fc.bindBinder binding))) (Fc.bindRhs binding)

lowerCase :: LowerEnv -> Fc.Expr -> Fc.Binder -> [Fc.Alt] -> LowerM GrinExpr
lowerCase env scrutinee binder alternatives = do
  representation <- expressionRuntimeRep env scrutinee
  case representation of
    TupleRep _ -> lowerTupleCase env scrutinee binder alternatives
    _ ->
      bindExpression env "case_value" scrutinee $ \case
        [value] -> do
          caseBinder <- freshVar (Fc.nameText (Fc.binderName binder)) representation
          loweredAlternatives <- mapM (lowerAlt (bindLocal env binder [caseBinder])) alternatives
          pure (GrinCase value caseBinder loweredAlternatives)
        _ -> throwLower "GRIN case expected one scrutinee value"

lowerTupleCase :: LowerEnv -> Fc.Expr -> Fc.Binder -> [Fc.Alt] -> LowerM GrinExpr
lowerTupleCase env scrutinee binder alternatives = do
  alternative <-
    case alternatives of
      first : _ -> pure first
      [] -> throwLower "GRIN cannot lower an empty unboxed tuple case"
  let typeEnv = foldl extendTypeBinder env (Fc.altTypeBinders alternative)
  fieldVariables <- mapM (freshVarsForBinder typeEnv) (Fc.altBinders alternative)
  let values = concat fieldVariables
      binderEnv = bindLocal typeEnv binder values
      alternativeEnv = foldl bindPair binderEnv (zip (Fc.altBinders alternative) fieldVariables)
  loweredRhs <- lowerExpr alternativeEnv (Fc.altRhs alternative)
  loweredScrutinee <- lowerExpr env scrutinee
  pure (GrinBind values loweredScrutinee loweredRhs)
  where
    bindPair current (fieldBinder, vars) = bindLocal current fieldBinder vars

lowerAlt :: LowerEnv -> Fc.Alt -> LowerM GrinAlt
lowerAlt env alternative = do
  let typeEnv = foldl extendTypeBinder env (Fc.altTypeBinders alternative)
  binderGroups <- mapM (freshVarsForBinder typeEnv) (Fc.altBinders alternative)
  let bodyEnv = foldl bindPair typeEnv (zip (Fc.altBinders alternative) binderGroups)
  body <- lowerExpr bodyEnv (Fc.altRhs alternative)
  alternativeConstructor <- lowerAltCon typeEnv (Fc.altCon alternative)
  pure
    GrinAlt
      { grinAltCon = alternativeConstructor,
        grinAltBinders = concat binderGroups,
        grinAltRhs = body
      }
  where
    bindPair current (binder, vars) = bindLocal current binder vars

lowerAltCon :: LowerEnv -> Fc.AltCon -> LowerM GrinAltCon
lowerAltCon env alternative =
  case alternative of
    Fc.AltData name -> pure (GrinDataAlt (constructorTag name))
    Fc.AltLit literal -> GrinLitAlt <$> lowerLiteral env literal
    Fc.AltDefault -> pure GrinDefaultAlt

-- | Suspend an expression in a function of its own. This is the last resort
-- of 'lowerLazy': only an expression whose lazy form needs code gets here.
makeThunk :: LowerEnv -> Text -> Fc.Expr -> LowerM GrinNode
makeThunk env hint expression = do
  representation <- expressionRuntimeRep env expression
  if not (isLiftedRuntimeRep representation)
    then throwLower ("GRIN cannot suspend an unlifted expression with representation " <> show representation)
    else do
      let captures = capturedVariables env expression
      functionName <- freshFunction (hint <> "_thunk")
      body <- lowerExpr env expression
      emitFunction
        GrinFunction
          { grinFunctionName = functionName,
            grinFunctionParameters = captures,
            grinFunctionResultRep = representation,
            grinFunctionBody = body
          }
      pure (GrinNode (GrinThunk functionName) (map GrinVarValue captures))

-- | Drop the type applications and casts that carry no runtime value.
stripValueWrappers :: Fc.Expr -> Fc.Expr
stripValueWrappers expression =
  case expression of
    Fc.ExTyApp inner _ -> stripValueWrappers inner
    Fc.ExCast inner _ -> stripValueWrappers inner
    _ -> expression

-- | The parameters, result and body of a lambda expression.
data ClosureShape = ClosureShape
  { closureBodyEnv :: !LowerEnv,
    closureParameters :: ![[GrinVar]],
    closureResultRep :: !GrinRep,
    closureBody :: !Fc.Expr
  }

closureLayouts :: ClosureShape -> [[GrinRep]]
closureLayouts = map (map grinVarRuntimeRep) . closureParameters

closureShape :: LowerEnv -> Fc.Expr -> LowerM ClosureShape
closureShape env expression = do
  let (bodyEnv0, binders, body) = collectLambdas env expression
  parameterGroups <- mapM (freshVarsForBinder bodyEnv0) binders
  let bodyEnv = foldl bindPair bodyEnv0 (zip binders parameterGroups)
  bodyRep <- expressionRuntimeRep bodyEnv body
  pure (ClosureShape bodyEnv parameterGroups bodyRep body)
  where
    bindPair current (binder, vars) = bindLocal current binder vars

-- | Emit the entry function of a lambda expression, under the given name when
-- 'localFunctionTable' has already assigned one, and build its closure node.
makeClosure :: LowerEnv -> Maybe FunctionName -> Fc.Expr -> LowerM GrinNode
makeClosure env entry expression = do
  let captures = capturedVariables env expression
  functionName <- maybe (freshFunction "closure") pure entry
  shape <- closureShape env expression
  loweredBody <- lowerExpr (closureBodyEnv shape) (closureBody shape)
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = captures <> concat (closureParameters shape),
        grinFunctionResultRep = closureResultRep shape,
        grinFunctionBody = loweredBody
      }
  pure (GrinNode (GrinClosure functionName (closureLayouts shape)) (map GrinVarValue captures))

-- | An expression that is a call of a primitive that never returns.
divergingExpression :: Fc.Expr -> Bool
divergingExpression expression =
  case applicationHead expression of
    Just name -> Fc.nameText name `elem` ["raise#", "aihcExit#"]
    Nothing -> False
  where
    applicationHead current =
      case current of
        Fc.ExApp function _ -> applicationHead function
        Fc.ExTyApp function _ -> applicationHead function
        Fc.ExVar name -> Just name
        Fc.ExForeignCall call _ _ -> Just (Fc.foreignCallName call)
        _ -> Nothing

collectLambdas :: LowerEnv -> Fc.Expr -> (LowerEnv, [Fc.Binder], Fc.Expr)
collectLambdas env expression =
  case expression of
    Fc.ExLam binder body ->
      let (bodyEnv, binders, result) = collectLambdas env body
       in (bodyEnv, binder : binders, result)
    Fc.ExTyLam binder body -> collectLambdas (extendTypeBinder env binder) body
    _ -> (env, [], expression)

capturedVariables :: LowerEnv -> Fc.Expr -> [GrinVar]
capturedVariables env expression =
  concat
    [ variables
    | name <- Set.toAscList (freeVariables expression),
      Just variables <- [Map.lookup name (lowerLocals env)]
    ]

freeVariables :: Fc.Expr -> Set Fc.Name
freeVariables expression =
  case expression of
    Fc.ExVar name -> Set.singleton name
    Fc.ExLit {} -> Set.empty
    Fc.ExApp function argument -> freeVariables function <> freeVariables argument
    Fc.ExTyApp function _ -> freeVariables function
    Fc.ExLam binder body -> Set.delete (Fc.binderName binder) (freeVariables body)
    Fc.ExTyLam _ body -> freeVariables body
    Fc.ExLet binding body -> freeVariables (Fc.bindRhs binding) <> Set.delete (Fc.binderName (Fc.bindBinder binding)) (freeVariables body)
    Fc.ExRec bindings body ->
      let names = Set.fromList (map (Fc.binderName . Fc.bindBinder) bindings)
       in (foldMap (freeVariables . Fc.bindRhs) bindings <> freeVariables body) `Set.difference` names
    Fc.ExCase scrutinee binder _ alternatives ->
      freeVariables scrutinee
        <> Set.delete (Fc.binderName binder) (foldMap freeAltVariables alternatives)
    Fc.ExCast inner _ -> freeVariables inner
    Fc.ExForeignCall _ _ arguments -> foldMap freeVariables arguments

freeAltVariables :: Fc.Alt -> Set Fc.Name
freeAltVariables alternative =
  freeVariables (Fc.altRhs alternative)
    `Set.difference` Set.fromList (map Fc.binderName (Fc.altBinders alternative))

expressionRuntimeRep :: LowerEnv -> Fc.Expr -> LowerM GrinRep
expressionRuntimeRep env expression =
  case expression of
    Fc.ExLit literal -> literalRep env literal
    -- A call that always raises never returns a value, so its runtime
    -- representation can stay polymorphic. This is what makes a
    -- representation-polymorphic @error@ possible.
    _ | divergingExpression expression -> pure liftedGrinRep
    _ -> expressionType env expression >>= liftEither . runtimeRep env

expressionType :: LowerEnv -> Fc.Expr -> LowerM Fc.Type
expressionType env expression =
  case expression of
    Fc.ExVar name -> lookupNameType env name
    Fc.ExLit {} -> throwLower "GRIN cannot infer a source type for this literal"
    Fc.ExApp function _ -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc.TyFun _ _ _ result -> pure result
        other -> throwLower ("GRIN application has a non-function type: " <> show other <> " for " <> show function)
    Fc.ExTyApp function argument -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc.TyForAll binder body -> pure (TypeOf.substType (Fc.binderName binder) (applySubstitution env argument) body)
        other -> throwLower ("GRIN type application has a non-forall type: " <> show other)
    Fc.ExLam binder body -> do
      bodyType <- expressionType (extendTypeBinder env binder) body
      argumentRep <- repType env (Fc.binderType binder)
      resultRep <- repType env bodyType
      pure (Fc.TyFun argumentRep resultRep (applySubstitution env (Fc.binderType binder)) bodyType)
    Fc.ExTyLam binder body -> Fc.TyForAll binder <$> expressionType (extendTypeBinder env binder) body
    Fc.ExLet binding body -> expressionType (extendTermBinder (Fc.bindBinder binding) env) body
    Fc.ExRec bindings body -> expressionType (foldl (flip (extendTermBinder . Fc.bindBinder)) env bindings) body
    Fc.ExCase _ _ resultType _ -> pure (applySubstitution env resultType)
    -- The foreign type is closed, so the environment substitution does not
    -- apply to it. The type arguments go into it directly.
    Fc.ExForeignCall call types arguments -> do
      instantiated <- foldM instantiate (Fc.foreignCallType call) types
      foldM apply instantiated arguments
      where
        instantiate functionType argument =
          case functionType of
            Fc.TyForAll binder body -> pure (TypeOf.substType (Fc.binderName binder) (applySubstitution env argument) body)
            other -> throwLower ("GRIN foreign call type application has a non-forall type: " <> show other)
        apply functionType _ =
          case reduce env functionType of
            Fc.TyFun _ _ _ result -> pure result
            other -> throwLower ("GRIN foreign call has a non-function type: " <> show other)
    Fc.ExCast _ coercion ->
      case TypeOf.coercionEndpoints (lowerTypes env) coercion of
        Just (_, target) -> pure (applySubstitution env target)
        Nothing -> throwLower ("GRIN cannot determine coercion endpoints: " <> show coercion)

runtimeRep :: LowerEnv -> Fc.Type -> Either String GrinRep
runtimeRep env sourceType = do
  representation <-
    maybe
      (Left ("GRIN cannot find a runtime representation for type: " <> show appliedType))
      pure
      (TypeOf.repOf (lowerTypes env) appliedType)
  convertRep env representation
  where
    appliedType = applySubstitution env sourceType

repType :: LowerEnv -> Fc.Type -> LowerM Fc.Type
repType env sourceType =
  maybe
    (throwLower ("GRIN cannot find a runtime representation type for: " <> show sourceType))
    pure
    (TypeOf.repOf (lowerTypes env) (applySubstitution env sourceType))

runtimeComponents :: LowerEnv -> Fc.Type -> Either String [GrinRep]
runtimeComponents env sourceType = runtimeRepComponents <$> runtimeRep env sourceType

convertRep :: LowerEnv -> Fc.Type -> Either String GrinRep
convertRep env sourceRep =
  case reduce env sourceRep of
    Fc.TyVar name -> Left ("GRIN does not support a variable runtime representation: " <> show name)
    Fc.TyCon name -> simpleRep (Fc.nameText name)
    Fc.TyApp (Fc.TyCon name) levity
      | Fc.nameText name == "BoxedRep" -> BoxedRep <$> convertLevity levity
    Fc.TyApp (Fc.TyCon name) fields
      | Fc.nameText name == "TupleRep" -> TupleRep <$> convertRepList env fields
      | Fc.nameText name == "SumRep" -> SumRep <$> convertRepList env fields
    Fc.TyApp (Fc.TyApp (Fc.TyCon name) count) element
      | Fc.nameText name == "VecRep" -> VecRep <$> readNamed "vector count" count <*> readNamed "vector element" element
    other -> Left ("GRIN does not support runtime representation: " <> show other)

simpleRep :: Text -> Either String GrinRep
simpleRep name =
  case name of
    "LiftedRep" -> pure liftedGrinRep
    "UnliftedRep" -> pure (BoxedRep Unlifted)
    "IntRep" -> pure IntRep
    "Int8Rep" -> pure Int8Rep
    "Int16Rep" -> pure Int16Rep
    "Int32Rep" -> pure Int32Rep
    "Int64Rep" -> pure Int64Rep
    "WordRep" -> pure WordRep
    "Word8Rep" -> pure Word8Rep
    "Word16Rep" -> pure Word16Rep
    "Word32Rep" -> pure Word32Rep
    "Word64Rep" -> pure Word64Rep
    "AddrRep" -> pure AddrRep
    "FloatRep" -> pure FloatRep
    "DoubleRep" -> pure DoubleRep
    _ -> Left ("GRIN does not know runtime representation: " <> T.unpack name)

convertLevity :: Fc.Type -> Either String GrinLevity
convertLevity levity =
  case levity of
    Fc.TyCon name
      | Fc.nameText name == "Lifted" -> pure Lifted
      | Fc.nameText name == "Unlifted" -> pure Unlifted
    _ -> Left ("GRIN does not support levity: " <> show levity)

convertRepList :: LowerEnv -> Fc.Type -> Either String [GrinRep]
convertRepList env list =
  case reduce env list of
    Fc.TyApp (Fc.TyCon name) _
      | Fc.nameText name == "[]" -> pure []
    Fc.TyApp (Fc.TyApp (Fc.TyApp (Fc.TyCon name) _) item) rest
      | Fc.nameText name == ":" -> (:) <$> convertRep env item <*> convertRepList env rest
    other -> Left ("GRIN does not support this runtime representation list: " <> show other)

readNamed :: (Read value) => String -> Fc.Type -> Either String value
readNamed label ty =
  case ty of
    Fc.TyCon name ->
      maybe (Left ("GRIN does not know " <> label <> ": " <> T.unpack (Fc.nameText name))) pure (readMaybe (T.unpack (Fc.nameText name)))
    _ -> Left ("GRIN does not support " <> label <> ": " <> show ty)

literalRep :: LowerEnv -> Fc.Literal -> LowerM GrinRep
literalRep env literal =
  case literal of
    Fc.LitInt representation _ -> liftEither (convertRep env representation)
    Fc.LitChar representation _ -> liftEither (convertRep env representation)
    Fc.LitAddr {} -> pure AddrRep

lowerLiteral :: LowerEnv -> Fc.Literal -> LowerM GrinLiteral
lowerLiteral env literal =
  case literal of
    Fc.LitInt representation value -> GrinLitInt <$> liftEither (convertRep env representation) <*> pure value
    Fc.LitChar representation value -> GrinLitChar <$> liftEither (convertRep env representation) <*> pure value
    Fc.LitAddr _ value -> pure (GrinLitAddr value)

lowerForeignCall :: Fc.Name -> Fc.CCallSpec -> GrinForeignCall
lowerForeignCall name specification =
  GrinForeignCall
    { grinForeignCallName = Fc.nameText name,
      grinForeignCallSymbol = Fc.ccallSymbol specification,
      grinForeignCallTarget = lowerForeignTarget (Fc.ccallTarget specification),
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = map lowerForeignType (Fc.ccallArgumentTypes specification),
            grinForeignResultType = lowerForeignType (Fc.ccallResultType specification),
            grinForeignEffect =
              case Fc.ccallEffect specification of
                Fc.ForeignPure -> GrinForeignPure
                Fc.ForeignRealWorld -> GrinForeignRealWorld
          }
    }

lowerForeignTarget :: Fc.CCallTarget -> GrinForeignTarget
lowerForeignTarget target =
  case target of
    Fc.CCallFunction -> GrinForeignFunction
    Fc.CCallAddress -> GrinForeignAddress

lowerForeignType :: Fc.CAbiType -> GrinForeignType
lowerForeignType foreignType =
  case foreignType of
    Fc.CAbiInt -> GrinForeignInt
    Fc.CAbiInt8 -> GrinForeignInt8
    Fc.CAbiInt16 -> GrinForeignInt16
    Fc.CAbiInt32 -> GrinForeignInt32
    Fc.CAbiInt64 -> GrinForeignInt64
    Fc.CAbiWord -> GrinForeignWord
    Fc.CAbiWord8 -> GrinForeignWord8
    Fc.CAbiWord16 -> GrinForeignWord16
    Fc.CAbiWord32 -> GrinForeignWord32
    Fc.CAbiWord64 -> GrinForeignWord64
    Fc.CAbiFloat -> GrinForeignFloat
    Fc.CAbiDouble -> GrinForeignDouble
    Fc.CAbiAddr -> GrinForeignAddr
    Fc.CAbiVoid -> GrinForeignVoid

splitFunctionType :: Fc.Type -> Either String ([Fc.Type], Fc.Type)
splitFunctionType sourceType =
  case sourceType of
    Fc.TyForAll _ body -> splitFunctionType body
    Fc.TyFun _ _ argument result -> do
      (arguments, finalResult) <- splitFunctionType result
      pure (argument : arguments, finalResult)
    _ -> pure ([], sourceType)

splitOperationalFunctionType :: LowerEnv -> [Fc.AxiomDecl] -> Fc.Type -> LowerM ([Fc.Type], Fc.Type)
splitOperationalFunctionType env axioms sourceType =
  case reduce env sourceType of
    Fc.TyForAll binder body -> splitOperationalFunctionType (extendTypeBinder env binder) axioms body
    Fc.TyFun _ _ argument result -> do
      (arguments, finalResult) <- splitOperationalFunctionType env axioms result
      pure (argument : arguments, finalResult)
    other ->
      let unwrapped = applyForeignAxioms env axioms other
       in if TypeOf.typesEqual (lowerTypes env) other unwrapped
            then pure ([], other)
            else splitOperationalFunctionType env axioms unwrapped

-- | Split a fixed number of arrows off an instantiated type. A newtype
-- around a function, such as @IO@, unwraps through its axiom.
splitOperationalArrows :: LowerEnv -> [Fc.AxiomDecl] -> Int -> Fc.Type -> LowerM ([Fc.Type], Fc.Type)
splitOperationalArrows env axioms count sourceType
  | count <= 0 = pure ([], sourceType)
  | otherwise =
      case reduce env sourceType of
        Fc.TyFun _ _ argument result -> do
          (arguments, finalResult) <- splitOperationalArrows env axioms (count - 1) result
          pure (argument : arguments, finalResult)
        other ->
          let unwrapped = applyForeignAxioms env axioms other
           in if TypeOf.typesEqual (lowerTypes env) other unwrapped
                then throwLower ("GRIN foreign call type has too few arrows: " <> show sourceType)
                else splitOperationalArrows env axioms count unwrapped

applyForeignAxioms :: LowerEnv -> [Fc.AxiomDecl] -> Fc.Type -> Fc.Type
applyForeignAxioms env axioms = go Set.empty
  where
    go visited sourceType
      | sourceType `Set.member` visited = sourceType
      | otherwise =
          case listToMaybe (mapMaybe (\axiom -> TypeOf.applyRepresentationalAxiom (lowerTypes env) axiom sourceType) axioms) of
            Just target -> go (Set.insert sourceType visited) target
            Nothing -> sourceType

splitForAlls :: Fc.Type -> ([Fc.Binder], Fc.Type)
splitForAlls sourceType =
  case sourceType of
    Fc.TyForAll binder body ->
      let (binders, result) = splitForAlls body
       in (binder : binders, result)
    _ -> ([], sourceType)

constructorArgumentTypes :: Fc.Type -> Either String [Fc.Type]
constructorArgumentTypes sourceType = fst <$> splitFunctionType sourceType

constructorResultType :: Fc.Type -> Either String Fc.Type
constructorResultType sourceType = snd <$> splitFunctionType sourceType

globalNameTable :: TypeOf.TypeEnv -> Map Fc.Name Text
globalNameTable types =
  Map.fromList
    [ (name, stableGlobalName name)
    | name <- Map.keys (TypeOf.teHeaders types),
      Fc.nameSort name `elem` [Fc.SortValue, Fc.SortDataConstructor]
    ]

constructorArityTable :: TypeOf.TypeEnv -> Map Fc.Name Int
constructorArityTable types =
  Map.mapMaybeWithKey constructorArity (TypeOf.teHeaders types)
  where
    constructorArity name sourceType
      | Fc.nameSort name == Fc.SortDataConstructor =
          either (const Nothing) (Just . length) (constructorArgumentTypes sourceType)
      | otherwise = Nothing

-- | Name the entry function of every top-level function before any code is
-- lowered, so that a call of one compiles to a direct call and a suspension
-- of one to a plain thunk node.
localFunctionTable :: LowerEnv -> Fc.Program -> LowerM (Map Fc.Name LocalFunction)
localFunctionTable env program =
  Map.fromList
    <$> sequence
      [ withLowerContext ("value " <> show (Fc.valName declaration)) $ do
          entry <- withCurrentValue (Fc.valName declaration) (freshFunction "")
          shape <- closureShape env (Fc.valBody declaration)
          pure (Fc.valName declaration, LocalFunction entry (closureLayouts shape) (closureResultRep shape))
      | Fc.DeclVal declaration <- Fc.programDecls program,
        isFunctionExpression (Fc.valBody declaration)
      ]

-- | GRIN identifies a top-level name by its package, its module, and its text.
-- Globals and constructor tags use the same encoding, so that the printer, the
-- linker, and the backends all split a name in one way.
stableGlobalName :: Fc.Name -> Text
stableGlobalName name =
  case Fc.nameOrigin name of
    Fc.OriginTop (PackageId packageName) moduleName ->
      grinScopedName packageName moduleName (Fc.nameText name)
    Fc.OriginLocal (Unique unique) -> Fc.nameText name <> "\0" <> T.pack (show unique)

constructorTag :: Fc.Name -> Text
constructorTag = stableGlobalName

lookupGlobalName :: LowerEnv -> Fc.Name -> LowerM Text
lookupGlobalName env name =
  maybe (throwLower ("GRIN has no global name for: " <> show name)) pure (Map.lookup name (lowerGlobalNames env))

lookupNameType :: LowerEnv -> Fc.Name -> LowerM Fc.Type
lookupNameType env name =
  case Map.lookup name (TypeOf.teBinders (lowerTypes env)) <|> TypeOf.lookupHeaderType (lowerTypes env) name of
    Just sourceType -> pure (applySubstitution env sourceType)
    Nothing -> throwLower ("GRIN has no type for: " <> show name)

applySubstitution :: LowerEnv -> Fc.Type -> Fc.Type
applySubstitution env = TypeOf.substTypes (lowerTypeSubstitution env)

reduce :: LowerEnv -> Fc.Type -> Fc.Type
reduce env = TypeOf.reduceType (lowerTypes env) . applySubstitution env

extendTypeBinder :: LowerEnv -> Fc.Binder -> LowerEnv
extendTypeBinder env binder = env {lowerTypes = TypeOf.extendBinder (lowerTypes env) binder}

-- | Substitute a type argument for the binder of an applied type lambda.
substituteTypeBinder :: LowerEnv -> Fc.Binder -> Fc.Type -> LowerEnv
substituteTypeBinder env binder argument =
  env {lowerTypeSubstitution = Map.insert (Fc.binderName binder) (applySubstitution env argument) (lowerTypeSubstitution env)}

defaultRuntimeReps :: LowerEnv -> [Fc.Binder] -> LowerEnv
defaultRuntimeReps = foldl defaultOne
  where
    defaultOne env binder =
      case reduce env (Fc.binderType binder) of
        Fc.TyCon name
          | Fc.nameText name == "RuntimeRep" ->
              env
                { lowerTypeSubstitution =
                    Map.insert
                      (Fc.binderName binder)
                      (Fc.TyCon (Wired.liftedRepName (TypeOf.tePrimPackage (lowerTypes env))))
                      (lowerTypeSubstitution env)
                }
        _ -> env

extendTermBinder :: Fc.Binder -> LowerEnv -> LowerEnv
extendTermBinder binder env = env {lowerTypes = TypeOf.extendBinder (lowerTypes env) binder}

bindLocal :: LowerEnv -> Fc.Binder -> [GrinVar] -> LowerEnv
bindLocal env binder variables =
  (extendTermBinder binder env)
    { lowerLocals = Map.insert (Fc.binderName binder) variables (lowerLocals env)
    }

freshVarsForBinder :: LowerEnv -> Fc.Binder -> LowerM [GrinVar]
freshVarsForBinder env binder = freshVarsForType env (Fc.nameText (Fc.binderName binder), applySubstitution env (Fc.binderType binder))

freshVarsForType :: LowerEnv -> (Text, Fc.Type) -> LowerM [GrinVar]
freshVarsForType env (hint, sourceType) = liftEither (runtimeRep env sourceType) >>= freshVars hint

freshVars :: Text -> GrinRep -> LowerM [GrinVar]
freshVars hint representation = mapM (freshVar hint) (runtimeRepComponents representation)

freshVar :: Text -> GrinRep -> LowerM GrinVar
freshVar hint representation = do
  state <- get
  let unique = lowerNextUnique state
  modify' (\current -> current {lowerNextUnique = unique - 1})
  pure (GrinVar hint unique representation)

-- | Name one generated function after the top-level value that needs it. An
-- empty hint names the entry of the value itself. A name that is already in
-- use gets a number, so that no two functions share a name.
freshFunction :: Text -> LowerM FunctionName
freshFunction hint = do
  state <- get
  let candidate = unusedFunctionName ("$" <> qualifiedHint (lowerCurrentValue state) hint) (lowerUsedFunctions state)
  modify' (\current -> current {lowerUsedFunctions = Set.insert candidate (lowerUsedFunctions current)})
  pure candidate

-- | Put the value name in front of the hint. A hint that already starts with
-- the value name keeps its own text, so that no name repeats itself.
qualifiedHint :: Text -> Text -> Text
qualifiedHint value hint
  | T.null value = hint
  | T.null hint = value
  | hint == value || (value <> "_") `T.isPrefixOf` hint = hint
  | otherwise = value <> "_" <> hint

emitFunction :: GrinFunction -> LowerM ()
emitFunction function = modify' (\state -> state {lowerFunctionsRev = function : lowerFunctionsRev state})

liftEither :: Either String value -> LowerM value
liftEither = either throwLower pure

throwLower :: String -> LowerM value
throwLower = lift . Left
