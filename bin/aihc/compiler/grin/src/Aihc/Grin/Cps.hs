{-# LANGUAGE OverloadedStrings #-}

-- | Convert direct-style GRIN into explicit continuation-passing style.
--
-- Computation entries receive an ordinary heap closure as their hidden final
-- parameter. Generated continuation entries consume one logical result and
-- never return. Consequently every potentially transferring operation is in
-- tail position and the runtime never needs a continuation stack.
module Aihc.Grin.Cps
  ( CpsGrinProgram,
    CpsGrinError (..),
    ContinuationFrameKind (..),
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
  )
where

import Aihc.Grin.Analysis (freeExprVars, maximumProgramVarUnique)
import Aihc.Grin.Anf (normalizeGrinProgram)
import Aihc.Grin.Syntax
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | A GRIN program whose computation entries and control transfers obey the
-- CPS calling convention. The metadata distinguishes computation entries from
-- continuation entries without polluting direct GRIN syntax.
data CpsGrinProgram = CpsGrinProgram
  { cpsGrinProgram :: !GrinProgram,
    cpsContinuationFunctions :: !(Set FunctionName),
    cpsContinuationFrames :: !(Map FunctionName ContinuationFrameKind),
    cpsFunctionContinuations :: !(Map FunctionName GrinVar),
    cpsUpdateFunction :: !FunctionName
  }
  deriving (Eq, Show, Read)

-- | Runtime-visible kinds of continuation frame. Field zero of every such
-- closure is its parent continuation. The explicit kind lets exception
-- unwinding inspect frames without relying on backend labels or code pointers.
data ContinuationFrameKind
  = ContinuationFrameNormal
  | ContinuationFrameCatch
  | ContinuationFrameUpdate
  | ContinuationFrameRestoreMask
  | ContinuationFrameStop
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Stable value stored in the shared runtime info-table ABI. Zero is reserved
-- for closures that are not continuation frames.
continuationFrameKindCode :: Maybe ContinuationFrameKind -> Int
continuationFrameKindCode frameKind =
  case frameKind of
    Nothing -> 0
    Just ContinuationFrameNormal -> 1
    Just ContinuationFrameCatch -> 2
    Just ContinuationFrameUpdate -> 3
    Just ContinuationFrameRestoreMask -> 4
    Just ContinuationFrameStop -> 5

data CpsGrinError
  = CpsGrinAlreadyTransformed !FunctionName
  | CpsGrinInvalidContinuationParent !FunctionName
  deriving (Eq, Show)

data CpsState = CpsState
  { cpsNextVarUnique :: !Int,
    cpsUsedFunctionNames :: !(Set FunctionName),
    cpsGeneratedFunctionsRev :: ![GrinFunction],
    cpsContinuationFramesState :: !(Map FunctionName ContinuationFrameKind),
    cpsComputationContinuations :: !(Map FunctionName GrinVar)
  }

type CpsM = StateT CpsState (Either CpsGrinError)

toCpsGrin :: GrinProgram -> Either CpsGrinError CpsGrinProgram
toCpsGrin sourceProgram = do
  ((functions, updateFunction), finalState) <- runStateT transform initialState
  let continuationFrames =
        Map.insert (grinFunctionName updateFunction) ContinuationFrameUpdate (cpsContinuationFramesState finalState)
  pure
    CpsGrinProgram
      { cpsGrinProgram =
          program
            { grinFunctions =
                functions
                  <> reverse (cpsGeneratedFunctionsRev finalState)
                  <> [updateFunction]
            },
        cpsContinuationFunctions = Map.keysSet continuationFrames,
        cpsContinuationFrames = continuationFrames,
        cpsFunctionContinuations = cpsComputationContinuations finalState,
        cpsUpdateFunction = grinFunctionName updateFunction
      }
  where
    program = normalizeGrinProgram sourceProgram
    sourceFunctions = grinFunctions program
    initialState =
      CpsState
        { cpsNextVarUnique = 1 + maximumProgramVarUnique program,
          cpsUsedFunctionNames = Set.fromList (map grinFunctionName sourceFunctions),
          cpsGeneratedFunctionsRev = [],
          cpsContinuationFramesState = Map.empty,
          cpsComputationContinuations = Map.empty
        }
    transform = do
      updateName <- freshFunctionName "$cps_update"
      functions <- mapM (transformFunction updateName) sourceFunctions
      updateFunction <- makeUpdateFunction updateName
      pure (functions, updateFunction)

transformFunction :: FunctionName -> GrinFunction -> CpsM GrinFunction
transformFunction updateName function = do
  continuation <- freshVar "$cps_return" liftedGrinRep
  let parameters = grinFunctionParameters function
      bound = Set.fromList (continuation : parameters)
  body <-
    transformTail
      updateName
      (grinFunctionName function)
      bound
      (grinFunctionResultRep function)
      (GrinVarValue continuation)
      (grinFunctionBody function)
  modify' $ \state ->
    state
      { cpsComputationContinuations =
          Map.insert (grinFunctionName function) continuation (cpsComputationContinuations state)
      }
  pure
    function
      { grinFunctionParameters = parameters <> [continuation],
        grinFunctionBody = body
      }

transformTail :: FunctionName -> FunctionName -> Set GrinVar -> GrinRep -> GrinValue -> GrinExpr -> CpsM GrinExpr
transformTail updateName parent bound resultRep continuation expression =
  case expression of
    GrinConstant values -> pure (GrinContinue continuation values)
    GrinBind resultVars (GrinCase scrutinee binder alternatives) body ->
      GrinCase scrutinee binder <$> mapM transformBoundAlternative alternatives
      where
        transformBoundAlternative alternative = do
          let alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)
          rhs <-
            transformTail
              updateName
              parent
              alternativeBound
              resultRep
              continuation
              (GrinBind resultVars (grinAltRhs alternative) body)
          pure alternative {grinAltRhs = rhs}
    GrinBind resultVars valueExpression body
      | isDirectExpression valueExpression -> do
          transformedBody <-
            transformTail
              updateName
              parent
              (bound <> Set.fromList resultVars)
              resultRep
              continuation
              body
          pure (GrinBind resultVars valueExpression transformedBody)
      | otherwise -> do
          (nextVar, nextNode) <-
            reifyContinuation
              updateName
              parent
              bound
              resultRep
              continuation
              resultVars
              body
          transformedValue <-
            transformTail
              updateName
              parent
              (Set.insert nextVar bound)
              (varsRuntimeRep resultVars)
              (GrinVarValue nextVar)
              valueExpression
          pure (GrinBind [nextVar] (GrinStore nextNode) transformedValue)
    GrinStore node -> continueDirect resultRep continuation (GrinStore node)
    GrinEnsureHeap requiredWords roots -> continueDirect resultRep continuation (GrinEnsureHeap requiredWords roots)
    GrinStoreUnchecked {} -> alreadyTransformed
    GrinStoreRec bindings body -> do
      let recursiveVars = Set.fromList (map fst bindings)
      GrinStoreRec bindings
        <$> transformTail updateName parent (bound <> recursiveVars) resultRep continuation body
    GrinStoreRecUnchecked {} -> alreadyTransformed
    GrinUpdate pointer value ->
      continueDirect (grinValueRuntimeRep value) continuation (GrinUpdate pointer value)
    GrinUpdateBlackhole pointer value ->
      continueDirect (grinValueRuntimeRep value) continuation (GrinUpdateBlackhole pointer value)
    GrinEval runtimeRep value -> do
      (updateVar, updateNode) <- makeUpdateContinuation updateName value continuation
      pure
        ( GrinBind
            [updateVar]
            (GrinStore updateNode)
            (GrinCpsEval runtimeRep value continuation (GrinVarValue updateVar))
        )
    GrinCpsEval {} -> alreadyTransformed
    GrinCall _ functionName arguments ->
      pure (GrinCall cpsResultRep functionName (arguments <> [continuation]))
    GrinPrimitiveCall runtimeRep name arguments
      | isControlPrimitive name ->
          pure (GrinCpsPrimitiveCall runtimeRep name arguments continuation)
      | otherwise ->
          continueDirect runtimeRep continuation (GrinPrimitiveCall runtimeRep name arguments)
    GrinCpsPrimitiveCall {} -> alreadyTransformed
    GrinApply runtimeRep function arguments ->
      pure (GrinCpsApply runtimeRep function arguments continuation)
    GrinCpsApply {} -> alreadyTransformed
    GrinContinue {} -> alreadyTransformed
    GrinCpsRaise {} -> alreadyTransformed
    GrinHalt {} -> alreadyTransformed
    GrinExit status -> pure (GrinExit status)
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM transformAlternative alternatives
      where
        transformAlternative alternative = do
          let alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)
          rhs <-
            transformTail
              updateName
              parent
              alternativeBound
              resultRep
              continuation
              (grinAltRhs alternative)
          pure alternative {grinAltRhs = rhs}
    GrinThrow exception -> pure (GrinCpsRaise exception continuation)
    GrinCatch runtimeRep action handler state -> do
      (catchVar, catchNode) <- makeCatchContinuation parent runtimeRep continuation handler
      evaluatedAction <- freshVar "$cps_catch_action" (grinValueRuntimeRep action)
      protectedAction <-
        transformTail
          updateName
          parent
          (Set.insert catchVar bound)
          runtimeRep
          (GrinVarValue catchVar)
          ( GrinBind
              [evaluatedAction]
              (GrinEval (grinValueRuntimeRep action) action)
              (GrinApply runtimeRep (GrinVarValue evaluatedAction) state)
          )
      pure
        ( GrinBind
            [catchVar]
            (GrinStore catchNode)
            protectedAction
        )
    GrinForeignCallExpr foreignCall arguments ->
      continueDirect resultRep continuation (GrinForeignCallExpr foreignCall arguments)
  where
    alreadyTransformed = lift (Left (CpsGrinAlreadyTransformed parent))

reifyContinuation :: FunctionName -> FunctionName -> Set GrinVar -> GrinRep -> GrinValue -> [GrinVar] -> GrinExpr -> CpsM (GrinVar, GrinNode)
reifyContinuation updateName parent bound resultRep outerContinuation resultVars body = do
  transformedBody <-
    transformTail
      updateName
      parent
      (bound <> Set.fromList resultVars)
      resultRep
      outerContinuation
      body
  continuationName <- freshContinuationName parent
  pointer <- freshVar "$cps_continuation" liftedGrinRep
  parentContinuation <-
    case outerContinuation of
      GrinVarValue var -> pure var
      GrinGlobalValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))
      GrinLitValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))
  let freeCaptures = freeExprVars transformedBody `Set.intersection` bound
      captures = parentContinuation : Set.toAscList (Set.delete parentContinuation freeCaptures)
      continuationFunction =
        GrinFunction
          { grinFunctionName = continuationName,
            grinFunctionParameters = captures <> resultVars,
            grinFunctionResultRep = resultRep,
            grinFunctionBody = transformedBody
          }
      continuationNode =
        GrinNode
          (GrinClosure continuationName [map grinVarRuntimeRep resultVars])
          (map GrinVarValue captures)
  addContinuationFunction ContinuationFrameNormal continuationFunction
  pure (pointer, continuationNode)

continueDirect :: GrinRep -> GrinValue -> GrinExpr -> CpsM GrinExpr
continueDirect runtimeRep continuation directExpression = do
  resultVars <- mapM (freshVar "$cps_result") (runtimeRepComponents runtimeRep)
  pure
    ( GrinBind
        resultVars
        directExpression
        (GrinContinue continuation (map GrinVarValue resultVars))
    )

makeUpdateContinuation :: FunctionName -> GrinValue -> GrinValue -> CpsM (GrinVar, GrinNode)
makeUpdateContinuation updateName blackhole continuation = do
  pointer <- freshVar "$cps_update" liftedGrinRep
  pure
    ( pointer,
      GrinNode (GrinClosure updateName [[liftedGrinRep]]) [continuation, blackhole]
    )

makeCatchContinuation :: FunctionName -> GrinRep -> GrinValue -> GrinValue -> CpsM (GrinVar, GrinNode)
makeCatchContinuation parent resultRep outerContinuation handler = do
  parentContinuation <-
    case outerContinuation of
      GrinVarValue var -> pure var
      GrinGlobalValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))
      GrinLitValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))
  catchName <- freshContinuationName parent
  pointer <- freshVar "$cps_catch" liftedGrinRep
  capturedHandler <- freshVar "$cps_handler" (grinValueRuntimeRep handler)
  resultVars <- mapM (freshVar "$cps_catch_result") (runtimeRepComponents resultRep)
  let catchFunction =
        GrinFunction
          { grinFunctionName = catchName,
            grinFunctionParameters = parentContinuation : capturedHandler : resultVars,
            grinFunctionResultRep = resultRep,
            grinFunctionBody = GrinContinue (GrinVarValue parentContinuation) (map GrinVarValue resultVars)
          }
      catchNode =
        GrinNode
          (GrinClosure catchName [runtimeRepComponents resultRep])
          [outerContinuation, handler]
  addContinuationFunction ContinuationFrameCatch catchFunction
  pure (pointer, catchNode)

makeUpdateFunction :: FunctionName -> CpsM GrinFunction
makeUpdateFunction updateName = do
  outerContinuation <- freshVar "$cps_outer" liftedGrinRep
  blackhole <- freshVar "$cps_blackhole" liftedGrinRep
  result <- freshVar "$cps_thunk_result" liftedGrinRep
  updated <- freshVar "$cps_updated" liftedGrinRep
  nextUpdate <- freshVar "$cps_next_update" liftedGrinRep
  let nextUpdateNode =
        GrinNode
          (GrinClosure updateName [[liftedGrinRep]])
          [GrinVarValue outerContinuation, GrinVarValue result]
  pure
    GrinFunction
      { grinFunctionName = updateName,
        grinFunctionParameters = [outerContinuation, blackhole, result],
        grinFunctionResultRep = liftedGrinRep,
        grinFunctionBody =
          GrinBind
            [updated]
            (GrinUpdateBlackhole (GrinVarValue blackhole) (GrinVarValue result))
            ( GrinBind
                [nextUpdate]
                (GrinStore nextUpdateNode)
                ( GrinCpsEval
                    liftedGrinRep
                    (GrinVarValue result)
                    (GrinVarValue outerContinuation)
                    (GrinVarValue nextUpdate)
                )
            )
      }

cpsResultRep :: GrinRep
cpsResultRep = TupleRep []

isDirectExpression :: GrinExpr -> Bool
isDirectExpression expression =
  case expression of
    GrinConstant {} -> True
    GrinStore {} -> True
    GrinUpdate {} -> True
    GrinUpdateBlackhole {} -> True
    GrinPrimitiveCall _ name _ -> not (isControlPrimitive name)
    GrinCpsPrimitiveCall {} -> False
    GrinForeignCallExpr {} -> True
    _ -> False

-- | Name a continuation after the function that needs it, so that a reader
-- can find the code that the continuation returns to.
freshContinuationName :: FunctionName -> CpsM FunctionName
freshContinuationName parent =
  freshFunctionName (unFunctionName parent <> "_cont")

freshFunctionName :: T.Text -> CpsM FunctionName
freshFunctionName base = do
  state <- get
  let candidate = unusedFunctionName base (cpsUsedFunctionNames state)
  put state {cpsUsedFunctionNames = Set.insert candidate (cpsUsedFunctionNames state)}
  pure candidate

freshVar :: T.Text -> GrinRep -> CpsM GrinVar
freshVar name runtimeRep = do
  state <- get
  let unique = cpsNextVarUnique state
  put state {cpsNextVarUnique = unique + 1}
  pure (GrinVar name unique runtimeRep)

addContinuationFunction :: ContinuationFrameKind -> GrinFunction -> CpsM ()
addContinuationFunction frameKind function = do
  modify' $ \state ->
    state
      { cpsGeneratedFunctionsRev = function : cpsGeneratedFunctionsRev state
      }
  modify' $ \state ->
    state
      { cpsContinuationFramesState =
          Map.insert (grinFunctionName function) frameKind (cpsContinuationFramesState state)
      }

varsRuntimeRep :: [GrinVar] -> GrinRep
varsRuntimeRep vars =
  case map grinVarRuntimeRep vars of
    [runtimeRep] -> runtimeRep
    runtimeReps -> TupleRep runtimeReps

isControlPrimitive :: T.Text -> Bool
isControlPrimitive name =
  name `elem` ["awaitIO#", "fork#", "newMVar#", "putMVar#", "readMVar#", "takeMVar#", "yield#"]
