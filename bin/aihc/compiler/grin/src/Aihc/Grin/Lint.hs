-- | Structural validation for GRIN programs.
module Aihc.Grin.Lint
  ( GrinLintError (..),
    lintProgram,
    lintCpsProgram,
    lintGcProgram,
  )
where

import Aihc.Grin.Cps (CpsGrinProgram, cpsFunctionContinuations, cpsGrinProgram)
import Aihc.Grin.Gc (GcGrinProgram, gcFunctionContinuations, gcGrinProgram)
import Aihc.Grin.Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data GrinLintError
  = GrinLintDuplicateFunction !FunctionName
  | GrinLintDuplicateGlobal !Text
  | GrinLintUnboundVariable !GrinVar
  | GrinLintUnknownFunction !FunctionName
  | GrinLintUnknownPrimitive !Text
  | GrinLintFunctionArity !FunctionName !Int !Int
  | GrinLintSaturatedClosure !FunctionName
  | GrinLintContinuationParameter !FunctionName
  | GrinLintThunkResult !FunctionName !GrinRep
  | GrinLintRepresentationMismatch !String !GrinRep !GrinRep
  | GrinLintResultLayout !String ![GrinRep] ![GrinRep]
  | GrinLintEvalNonLifted !GrinRep
  | GrinLintUpdateNonLifted !GrinRep
  | GrinLintForeignArity !Text !Int !Int
  | GrinLintUnknownForeignCall !Text
  | GrinLintForeignCallDescriptorMismatch !Text
  | GrinLintConstructorLayout !Text ![GrinRep] ![GrinRep]
  deriving (Eq, Show)

data LintEnv = LintEnv
  { lintFunctionArities :: !(Map FunctionName Int),
    lintFunctionNodeArities :: !(Map FunctionName Int),
    lintFunctionResults :: !(Map FunctionName GrinRep),
    lintPrimitiveArities :: !(Map Text Int),
    lintConstructorLayouts :: !(Map Text [[GrinRep]]),
    lintForeignCalls :: !(Map Text GrinForeignCall)
  }

-- | Validate direct GRIN. No function has a hidden continuation parameter.
lintProgram :: GrinProgram -> [GrinLintError]
lintProgram = lintProgramWith Map.empty

-- | Validate CPS-GRIN. The program metadata gives the hidden continuation
-- parameter of every computation entry.
lintCpsProgram :: CpsGrinProgram -> [GrinLintError]
lintCpsProgram cps = lintProgramWith (cpsFunctionContinuations cps) (cpsGrinProgram cps)

-- | Validate GC-GRIN. The GC phase keeps the CPS metadata unchanged.
lintGcProgram :: GcGrinProgram -> [GrinLintError]
lintGcProgram gc = lintProgramWith (gcFunctionContinuations gc) (gcGrinProgram gc)

-- | Validate one GRIN program. The first argument gives the hidden
-- continuation parameter of each computation entry. The CPS transformation
-- adds that parameter to the entry, but it does not add a field to the thunk
-- and closure nodes that name the entry. A node therefore supplies one value
-- less than the entry has parameters.
lintProgramWith :: Map FunctionName GrinVar -> GrinProgram -> [GrinLintError]
lintProgramWith continuations program =
  duplicateFunctionErrors
    <> duplicateGlobalErrors
    <> continuationParameterErrors
    <> concatMap (lintGlobal env) (grinGlobals program)
    <> concatMap (lintFunction env) (grinFunctions program)
  where
    functions = grinFunctions program
    globals = grinGlobals program
    functionNames = map grinFunctionName functions
    globalNames = map fst globals
    duplicateFunctionErrors = map GrinLintDuplicateFunction (duplicates functionNames)
    duplicateGlobalErrors = map GrinLintDuplicateGlobal (duplicates globalNames)
    env =
      LintEnv
        { lintFunctionArities =
            Map.fromList
              [ (grinFunctionName function, length (grinFunctionParameters function))
              | function <- functions
              ],
          lintFunctionResults =
            Map.fromList
              [(grinFunctionName function, grinFunctionResultRep function) | function <- functions],
          lintFunctionNodeArities =
            Map.fromList
              [ (grinFunctionName function, semanticFunctionArity function)
              | function <- functions
              ],
          lintPrimitiveArities = Map.fromList [(grinVarName var, arity) | (var, arity) <- grinPrimitives program],
          lintConstructorLayouts = Map.fromList (grinConstructors program),
          lintForeignCalls = Map.fromList [(grinForeignCallName call, call) | call <- grinForeignCalls program]
        }
    -- The number of values that a thunk or closure node must supply.
    semanticFunctionArity function =
      length (grinFunctionParameters function) - hiddenParameterCount function
    hiddenParameterCount function
      | Map.member (grinFunctionName function) continuations = 1
      | otherwise = 0
    -- A recorded continuation must be the last parameter of its entry.
    -- Nothing else keeps the node arities and the calling convention in step.
    continuationParameterErrors =
      [ GrinLintContinuationParameter (grinFunctionName function)
      | function <- functions,
        Just continuation <- [Map.lookup (grinFunctionName function) continuations],
        lastParameter function /= Just continuation
      ]
    lastParameter function =
      case reverse (grinFunctionParameters function) of
        parameter : _ -> Just parameter
        [] -> Nothing

lintGlobal :: LintEnv -> (Text, GrinNode) -> [GrinLintError]
lintGlobal env (_, node) = lintNode env Set.empty node

lintFunction :: LintEnv -> GrinFunction -> [GrinLintError]
lintFunction env function =
  resultErrors
    <> lintFunctionResult env (grinFunctionResultRep function) (grinFunctionBody function)
    <> lintExpr env bound (grinFunctionBody function)
  where
    bound = Set.fromList (grinFunctionParameters function)
    resultErrors =
      case exprRuntimeReps (grinFunctionBody function) of
        Just actual
          | actual /= expected ->
              [GrinLintResultLayout "function result" expected actual]
        _ -> []
    expected = runtimeRepComponents (grinFunctionResultRep function)

lintFunctionResult :: LintEnv -> GrinRep -> GrinExpr -> [GrinLintError]
lintFunctionResult env resultRep expr =
  case expr of
    GrinBind _ _ body -> lintFunctionResult env resultRep body
    GrinStore (GrinNode (GrinClosure functionName []) _) ->
      [ GrinLintSaturatedClosure functionName
      | Map.lookup functionName (lintFunctionResults env) == Just resultRep
      ]
    GrinStore {} -> []
    GrinEnsureHeap {} -> []
    GrinStoreUnchecked (GrinNode (GrinClosure functionName []) _) ->
      [ GrinLintSaturatedClosure functionName
      | Map.lookup functionName (lintFunctionResults env) == Just resultRep
      ]
    GrinStoreUnchecked {} -> []
    GrinStoreRec _ body -> lintFunctionResult env resultRep body
    GrinStoreRecUnchecked _ body -> lintFunctionResult env resultRep body
    GrinCase _ _ alternatives -> concatMap (lintFunctionResult env resultRep . grinAltRhs) alternatives
    _ -> []

lintExpr :: LintEnv -> Set GrinVar -> GrinExpr -> [GrinLintError]
lintExpr env bound expr =
  case expr of
    GrinConstant values -> concatMap (lintValue bound) values
    GrinBind vars valueExpr body ->
      bindRepresentationErrors vars valueExpr
        <> lintExpr env bound valueExpr
        <> lintExpr env (Set.fromList vars <> bound) body
    GrinStore node -> lintNode env bound node
    GrinEnsureHeap requiredWords roots -> lintValue bound requiredWords <> concatMap (lintValue bound) roots
    GrinStoreUnchecked node -> lintNode env bound node
    GrinStoreRec bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinStoreRecUnchecked bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinUpdate pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue bound pointer
        <> lintValue bound value
    GrinUpdateBlackhole pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue bound pointer
        <> lintValue bound value
    GrinEval _ value ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedGrinRep]
        <> lintValue bound value
    GrinCpsEval _ value continuation updateContinuation ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedGrinRep]
        <> lintValue bound value
        <> lintValue bound continuation
        <> lintValue bound updateContinuation
    GrinCall _ functionName arguments ->
      lintKnownCall env bound functionName arguments
    GrinPrimitiveCall _ name arguments ->
      [GrinLintUnknownPrimitive name | name `Map.notMember` lintPrimitiveArities env]
        <> concatMap (lintValue bound) arguments
    GrinCpsPrimitiveCall _ name arguments continuation ->
      [GrinLintUnknownPrimitive name | name `Map.notMember` lintPrimitiveArities env]
        <> concatMap (lintValue bound) arguments
        <> lintValue bound continuation
    GrinApply _ function arguments -> lintValue bound function <> concatMap (lintValue bound) arguments
    GrinCpsApply _ function arguments continuation ->
      lintValue bound function
        <> concatMap (lintValue bound) arguments
        <> lintValue bound continuation
    GrinContinue continuation values ->
      lintValue bound continuation <> concatMap (lintValue bound) values
    GrinCpsRaise exception continuation ->
      lintValue bound exception <> lintValue bound continuation
    GrinHalt values -> concatMap (lintValue bound) values
    GrinExit status ->
      [GrinLintRepresentationMismatch "exit status" (grinValueRuntimeRep status) IntRep | grinValueRuntimeRep status /= IntRep]
        <> lintValue bound status
    GrinCase scrutinee binder alternatives ->
      lintValue bound scrutinee
        <> caseRepresentationErrors alternatives
        <> concatMap (lintAlt env (Set.insert binder bound)) alternatives
    GrinThrow exception -> lintValue bound exception
    GrinCatch _ action handler state ->
      lintValue bound action
        <> lintValue bound handler
        <> concatMap (lintValue bound) state
    GrinForeignCallExpr foreignCall arguments ->
      let expectedReps = grinForeignOperandReps (grinForeignCallSignature foreignCall)
          actualReps = map grinValueRuntimeRep arguments
          descriptorErrors =
            case Map.lookup (grinForeignCallName foreignCall) (lintForeignCalls env) of
              Nothing -> [GrinLintUnknownForeignCall (grinForeignCallName foreignCall)]
              Just declared
                | declared /= foreignCall -> [GrinLintForeignCallDescriptorMismatch (grinForeignCallName foreignCall)]
                | otherwise -> []
       in descriptorErrors
            <> [ GrinLintForeignArity (grinForeignCallName foreignCall) (length expectedReps) (length actualReps)
               | length expectedReps /= length actualReps
               ]
            <> [ GrinLintRepresentationMismatch "foreign call argument" expected actual
               | (expected, actual) <- zip expectedReps actualReps,
                 expected /= actual
               ]
            <> concatMap (lintValue bound) arguments

lintKnownCall :: LintEnv -> Set GrinVar -> FunctionName -> [GrinValue] -> [GrinLintError]
lintKnownCall env bound functionName arguments =
  functionErrors <> concatMap (lintValue bound) arguments
  where
    functionErrors =
      case Map.lookup functionName (lintFunctionArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected /= length arguments -> [GrinLintFunctionArity functionName expected (length arguments)]
        Just _ -> []

bindRepresentationErrors :: [GrinVar] -> GrinExpr -> [GrinLintError]
bindRepresentationErrors vars valueExpr =
  case exprRuntimeReps valueExpr of
    Just actual
      | actual /= expected ->
          [GrinLintResultLayout "bind" expected actual]
    _ -> []
  where
    expected = map grinVarRuntimeRep vars

caseRepresentationErrors :: [GrinAlt] -> [GrinLintError]
caseRepresentationErrors alternatives =
  case mapMaybe (exprRuntimeReps . grinAltRhs) alternatives of
    expected : rest ->
      [ GrinLintResultLayout "case alternative" expected actual
      | actual <- rest,
        actual /= expected
      ]
    [] -> []

lintAlt :: LintEnv -> Set GrinVar -> GrinAlt -> [GrinLintError]
lintAlt env bound alt =
  lintExpr env (Set.fromList (grinAltBinders alt) <> bound) (grinAltRhs alt)

lintValue :: Set GrinVar -> GrinValue -> [GrinLintError]
lintValue bound value =
  case value of
    GrinVarValue var
      | var `Set.member` bound -> []
      | otherwise -> [GrinLintUnboundVariable var]
    GrinGlobalValue _ -> []
    GrinLitValue _ -> []

lintNode :: LintEnv -> Set GrinVar -> GrinNode -> [GrinLintError]
lintNode env bound node =
  concatMap (lintValue bound) (grinNodeFields node)
    <> lintNodeFunction env node
    <> lintConstructorFields env node

lintConstructorFields :: LintEnv -> GrinNode -> [GrinLintError]
lintConstructorFields env node =
  case grinNodeTag node of
    GrinConstructor name remaining ->
      case Map.lookup name (lintConstructorLayouts env) of
        Just layouts
          | let suppliedCount = length layouts - remaining,
            suppliedCount < 0
              || actual /= concat (take suppliedCount layouts) ->
              [GrinLintConstructorLayout name (concat layouts) actual]
        _ -> []
    _ -> []
  where
    actual = map grinValueRuntimeRep (grinNodeFields node)

lintNodeFunction :: LintEnv -> GrinNode -> [GrinLintError]
lintNodeFunction env node =
  case grinNodeTag node of
    GrinThunk functionName -> checkFunctionArity functionName fieldCount <> checkThunkResult functionName
    GrinClosure functionName argumentLayouts -> checkClosureArity functionName argumentLayouts
    _ -> []
  where
    fieldCount = length (grinNodeFields node)
    checkFunctionArity functionName actual =
      case Map.lookup functionName (lintFunctionNodeArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected == actual -> []
          | otherwise -> [GrinLintFunctionArity functionName expected actual]
    checkClosureArity functionName argumentLayouts =
      checkFunctionArity functionName (fieldCount + length (concat argumentLayouts))
    checkThunkResult functionName =
      case Map.lookup functionName (lintFunctionResults env) of
        Just runtimeRep
          | not (isLiftedRuntimeRep runtimeRep) -> [GrinLintThunkResult functionName runtimeRep]
        _ -> []

duplicates :: (Ord a) => [a] -> [a]
duplicates = go Set.empty Set.empty
  where
    go _ repeated [] = Set.toAscList repeated
    go seen repeated (value : rest)
      | value `Set.member` seen = go seen (Set.insert value repeated) rest
      | otherwise = go (Set.insert value seen) repeated rest

exprRuntimeReps :: GrinExpr -> Maybe [GrinRep]
exprRuntimeReps expr =
  case expr of
    GrinConstant values -> Just (map grinValueRuntimeRep values)
    GrinBind _ _ body -> exprRuntimeReps body
    GrinStore {} -> Just [liftedGrinRep]
    GrinEnsureHeap _ roots -> Just (map grinValueRuntimeRep roots)
    GrinStoreUnchecked {} -> Just [liftedGrinRep]
    GrinStoreRec _ body -> exprRuntimeReps body
    GrinStoreRecUnchecked _ body -> exprRuntimeReps body
    GrinUpdate _ value -> Just [grinValueRuntimeRep value]
    GrinUpdateBlackhole _ value -> Just [grinValueRuntimeRep value]
    GrinEval runtimeRep _ -> Just (runtimeRepComponents runtimeRep)
    GrinCpsEval {} -> Nothing
    GrinCall runtimeRep _ _ ->
      case runtimeRepComponents runtimeRep of
        [] -> Nothing
        components -> Just components
    GrinPrimitiveCall runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinCpsPrimitiveCall {} -> Nothing
    GrinApply runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinCpsApply {} -> Nothing
    GrinContinue {} -> Nothing
    GrinCpsRaise {} -> Nothing
    GrinHalt {} -> Nothing
    GrinExit {} -> Nothing
    GrinCase _ _ alternatives ->
      listToMaybe (mapMaybe (exprRuntimeReps . grinAltRhs) alternatives)
    GrinThrow {} -> Nothing
    GrinCatch runtimeRep _ _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinForeignCallExpr foreignCall _ ->
      Just (grinForeignCallResultReps (grinForeignCallSignature foreignCall))
