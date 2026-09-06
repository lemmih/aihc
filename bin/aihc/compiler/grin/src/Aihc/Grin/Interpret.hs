{-# LANGUAGE OverloadedStrings #-}

-- | Reference interpreter for strict GRIN programs.
module Aihc.Grin.Interpret
  ( InterpretError (..),
    RuntimeValue (..),
    interpretProgramBinding,
    interpretProgramIoBinding,
    interpretProgramFunctionSnapshot,
  )
where

import Aihc.Grin.Snapshot
import Aihc.Grin.Syntax
import Control.Exception (SomeException, displayException, try)
import Control.Monad (when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, StateT, execState, get, gets, modify', runState, runStateT)
import Data.Bits (complement, countLeadingZeros, countTrailingZeros, popCount, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.LibFFI (Arg, RetType, argCDouble, argCFloat, argInt16, argInt32, argInt64, argInt8, argPtr, argWord16, argWord32, argWord64, argWord8, callFFI, retCDouble, retCFloat, retInt16, retInt32, retInt64, retInt8, retPtr, retVoid, retWord16, retWord32, retWord64, retWord8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Array (newArray0, peekArray, pokeArray, withArray0)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (FunPtr, IntPtr (..), Ptr, alignPtr, castFunPtrToPtr, castPtr, intPtrToPtr, minusPtr, plusPtr, ptrToIntPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble, double2Float, float2Double)
import System.IO (Handle, IOMode (..), hClose, hFlush, openBinaryFile, stderr, stdin, stdout)
import System.Mem.StableName qualified as Host
import System.Posix.DynamicLinker (DL (Default), dlsym)

data InterpretError
  = InterpretUnboundVariable !GrinVar
  | InterpretMissingBinding !Text
  | InterpretUnknownFunction !FunctionName
  | InterpretFunctionArity !FunctionName !Int !Int
  | InterpretConstructorArity !Text !Int !Int
  | InterpretApplyNonFunction !RuntimeValue
  | InterpretNoMatchingAlternative !RuntimeValue
  | InterpretPrimitiveArity !Text !Int
  | InterpretPrimitiveTypeError !Text !RuntimeValue
  | InterpretForeignArity !Text !Int !Int
  | InterpretForeignTypeError !Text !RuntimeValue
  | InterpretForeignLookupError !Text !Text
  | InterpretInvalidArrayIndex !Text !Integer !Int
  | InterpretInvalidByteArrayRange !Text !Integer !Integer !Int
  | InterpretResultArity !Int !Int
  | InterpretInvalidThunkResult ![RuntimeValue]
  | InterpretInvalidThunkResultRep !FunctionName !GrinRep
  | InterpretInvalidUpdateValue !RuntimeValue
  | InterpretExpectedLocation !RuntimeValue
  | InterpretInvalidLocation !Int
  | InterpretBlackhole !Int
  | InterpretNoRunnableThreads
  | InterpretCpsExpression !GrinExpr
  | InterpretProcessExit !Integer
  | InterpretRaisedException !Text
  deriving (Eq, Show)

data RuntimeValue
  = RuntimeLit !GrinLiteral
  | RuntimeAddress !(Ptr ())
  | RuntimeArray !GrinArray
  | RuntimeByteArray !GrinByteArray
  | RuntimeIOError !Integer
  | RuntimeIOHandle !GrinIOHandle
  | RuntimeIORequest !GrinIORequest
  | RuntimeMVar !Int
  | RuntimeNode !GrinNodeTag ![RuntimeValue]
  | RuntimeLocation !Int
  | RuntimeMutVar !GrinMutVar
  | RuntimeStableName !GrinStableName
  | RuntimeStateToken
  deriving (Eq, Show)

newtype GrinMutVar = GrinMutVar (IORef RuntimeValue)

newtype GrinStableName = GrinStableName (Host.StableName RuntimeValue)

instance Eq GrinStableName where
  GrinStableName left == GrinStableName right = Host.eqStableName left right

instance Show GrinStableName where
  show _ = "<stable-name>"

newtype GrinArray = GrinArray (IORef [RuntimeValue])

instance Eq GrinArray where
  GrinArray left == GrinArray right = left == right

instance Show GrinArray where
  show _ = "<array>"

instance Eq GrinMutVar where
  GrinMutVar left == GrinMutVar right = left == right

instance Show GrinMutVar where
  show _ = "<mutvar>"

data GrinByteArray = GrinByteArray
  { grinByteArraySize :: !(IORef Int),
    grinByteArrayContents :: !(Ptr ()),
    grinByteArrayPinned :: !Bool,
    grinByteArrayAlignment :: !Int
  }

instance Eq GrinByteArray where
  left == right = grinByteArraySize left == grinByteArraySize right

instance Show GrinByteArray where
  show _ = "<byte-array>"

data GrinIOHandle = GrinIOHandle !Int !Handle

instance Eq GrinIOHandle where
  GrinIOHandle left _ == GrinIOHandle right _ = left == right

instance Show GrinIOHandle where
  show _ = "<io-handle>"

data GrinIOOperation
  = GrinRead !GrinIOHandle !(Ptr ()) !Int !Int
  | GrinWrite !GrinIOHandle !(Ptr ()) !Int !Int
  | GrinOpen !Text !Integer
  deriving (Eq, Show)

data GrinIOResult
  = GrinIOInt !Integer
  | GrinIOOpenResult !(Either Integer GrinIOHandle)
  deriving (Eq, Show)

data GrinIOState
  = GrinIOSubmitted !GrinIOOperation
  | GrinIOCompleted !GrinIOResult
  | GrinIOConsumed
  deriving (Eq, Show)

newtype GrinIORequest = GrinIORequest (IORef GrinIOState)

instance Eq GrinIORequest where
  GrinIORequest left == GrinIORequest right = left == right

instance Show GrinIORequest where
  show _ = "<io-request>"

type Env = Map GrinVar RuntimeValue

data HeapCell
  = HeapSuspended !FunctionName ![RuntimeValue]
  | HeapValue !RuntimeValue
  | HeapRaised !RuntimeValue
  | HeapBlackhole
  | HeapThread

data Machine = Machine
  { machineProgram :: !GrinProgram,
    machineFunctions :: !(Map FunctionName GrinFunction),
    machineGlobals :: !(Map Text RuntimeValue),
    machineHeap :: !(IntMap HeapCell),
    machineNextLocation :: !Int,
    machineMVars :: !(IntMap GrinMVarState),
    machineNextMVar :: !Int,
    machineRunQueue :: !(Seq ThreadAction)
  }

data EvalFailure
  = EvalInterpret !InterpretError
  | EvalRaised !RuntimeValue

type EvalM = ExceptT EvalFailure (StateT Machine IO)

-- | A suspended direct-style continuation. Keeping the continuation as an
-- interpreter action lets yield# switch threads without relying on the host
-- call stack to represent the resumed computation.
newtype ThreadAction = ThreadAction (EvalM [RuntimeValue])

newtype MVarValueWaiter = MVarValueWaiter (RuntimeValue -> ThreadAction)

data GrinMVarState = GrinMVarState
  { grinMVarValue :: !(Maybe RuntimeValue),
    grinMVarReaders :: !(Seq MVarValueWaiter),
    grinMVarTakers :: !(Seq MVarValueWaiter),
    grinMVarPutters :: !(Seq (RuntimeValue, ThreadAction))
  }

type ScheduledContinuation = [RuntimeValue] -> EvalM [RuntimeValue]

data SnapshotBuild = SnapshotBuild
  { snapshotBuildSource :: !(IntMap HeapCell),
    snapshotBuildValueSources :: ![(RuntimeValue, Int)],
    snapshotBuildLocations :: !(IntMap Int),
    snapshotBuildSources :: !(IntMap Int),
    snapshotBuildNextLocation :: !Int,
    snapshotBuildCells :: !(IntMap SnapshotCell)
  }

-- | Interpret and render a named top-level binding using the raw constructor
-- representation shared by the compiler pipeline evaluation fixtures.
interpretProgramBinding :: Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramBinding = interpretProgramBindingWith pure

-- | Interpret a named top-level binding and explicitly run its value as an IO
-- action. The caller, rather than GRIN, owns the decision to use this entry
-- point.
interpretProgramIoBinding :: Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramIoBinding = interpretProgramBindingWith runIOValue

-- | Execute a nullary GRIN function and snapshot its raw return values and
-- reachable heap. Snapshotting reads cells but never enters a thunk or forces
-- a location; only 'GrinEval' nodes executed by the function may do that.
interpretProgramFunctionSnapshot :: FunctionName -> GrinProgram -> IO (Either InterpretError HeapSnapshot)
interpretProgramFunctionSnapshot functionName program = do
  let machine = initialMachine program
  (result, finalMachine) <- runStateT (runExceptT (callFunction functionName [])) machine
  pure $
    case result of
      Right values -> Right (buildHeapSnapshot (machineHeap finalMachine) values)
      Left (EvalInterpret err) -> Left err
      Left (EvalRaised exception) -> Left (InterpretRaisedException (T.pack (show exception)))

interpretProgramBindingWith :: (RuntimeValue -> EvalM RuntimeValue) -> Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramBindingWith enterValue name program = do
  let machine = initialMachine program
  (result, finalMachine) <- runStateT (runExceptT action) machine
  case result of
    Right rendered -> pure (Right rendered)
    Left (EvalInterpret err) -> pure (Left err)
    Left (EvalRaised exception) -> do
      (renderResult, _) <- runStateT (runExceptT (renderRawValueM exception)) finalMachine
      pure $
        case renderResult of
          Right rendered -> Left (InterpretRaisedException rendered)
          Left _ -> Left (InterpretRaisedException (T.pack (show exception)))
  where
    action = do
      globals <- getsMachine machineGlobals
      value <-
        case Map.lookup name globals of
          Just binding -> pure binding
          Nothing -> throwInterpret (InterpretMissingBinding name)
      forced <- forceValue value
      result <- enterValue forced
      renderRawValueM result

initialMachine :: GrinProgram -> Machine
initialMachine program =
  Machine
    { machineProgram = program,
      machineFunctions =
        Map.fromList
          [ (grinFunctionName function, function)
          | function <- grinFunctions program
          ],
      machineGlobals = globals,
      machineHeap =
        IntMap.fromList
          [ (location, storedCell (staticNode node))
          | ((_, node), location) <- zip globalNodes [0 ..]
          ],
      machineNextLocation = length globalNodes,
      machineMVars = IntMap.empty,
      machineNextMVar = 0,
      machineRunQueue = Seq.empty
    }
  where
    globalNodes = Map.toAscList globalNodeMap
    globalNodeMap =
      Map.unions
        [ Map.fromList (grinGlobals program),
          Map.fromList
            [ (constructor, GrinNode (GrinConstructor constructor 0) [])
            | (constructor, layouts) <- grinConstructors program,
              null layouts
            ]
        ]
    globals =
      Map.fromList
        [ (name, RuntimeLocation location)
        | ((name, _), location) <- zip globalNodes [0 ..]
        ]
    staticNode (GrinNode tag fields) = RuntimeNode tag (map staticValue fields)
    staticValue value =
      case value of
        GrinVarValue var ->
          case Map.lookup (grinVarName var) globals of
            Just runtimeValue -> runtimeValue
            Nothing -> error ("GRIN interpreter found an unbound static global " <> T.unpack (grinVarName var))
        GrinGlobalValue name ->
          case Map.lookup name globals of
            Just runtimeValue -> runtimeValue
            Nothing -> error ("GRIN interpreter found an external static global " <> T.unpack name)
        GrinLitValue literal -> RuntimeLit literal

evalScheduledExpr :: Env -> GrinExpr -> ScheduledContinuation -> EvalM [RuntimeValue]
evalScheduledExpr env expr continue =
  case expr of
    GrinConstant values -> continue =<< mapM (materializeValue env) values
    GrinBind vars valueExpr body ->
      evalScheduledExpr env valueExpr $ \values ->
        if length vars == length values
          then evalScheduledExpr (Map.fromList (zip vars values) `Map.union` env) body continue
          else throwInterpret (InterpretResultArity (length vars) (length values))
    GrinStore node -> do
      value <- materializeNode env node >>= allocateCell . storedCell
      continue [value]
    GrinEnsureHeap _ roots -> continue =<< mapM (materializeValue env) roots
    GrinStoreUnchecked node -> do
      value <- materializeNode env node >>= allocateCell . storedCell
      continue [value]
    GrinStoreRec bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      runtimeNodes <- mapM (materializeNode recursiveEnv . snd) bindings
      mapM_ (uncurry writeCell) (zip locations (map storedCell runtimeNodes))
      evalScheduledExpr recursiveEnv body continue
    GrinStoreRecUnchecked bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      runtimeNodes <- mapM (materializeNode recursiveEnv . snd) bindings
      mapM_ (uncurry writeCell) (zip locations (map storedCell runtimeNodes))
      evalScheduledExpr recursiveEnv body continue
    GrinUpdate pointer value -> do
      pointerValue <- materializeValue env pointer
      updatedValue <- materializeValue env value
      result <- updateValue pointerValue updatedValue
      continue [result]
    GrinUpdateBlackhole {} -> rejectCpsExpression
    GrinEval _ value -> do
      runtimeValue <- materializeValue env value
      forceScheduledValue runtimeValue (continue . (: []))
    GrinCpsEval {} -> rejectCpsExpression
    GrinCall _ functionName arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      callScheduledFunction functionName argumentValues continue
    GrinPrimitiveCall _ name arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      evalScheduledPrimitive name argumentValues continue
    GrinCpsPrimitiveCall {} -> rejectCpsExpression
    GrinApply _ function arguments -> do
      functionValue <- materializeValue env function
      argumentValues <- mapM (materializeValue env) arguments
      applyScheduledValue functionValue argumentValues continue
    GrinCpsApply {} -> rejectCpsExpression
    GrinContinue {} -> rejectCpsExpression
    GrinCpsRaise {} -> rejectCpsExpression
    GrinHalt {} -> rejectCpsExpression
    GrinExit status -> do
      statusValue <- materializeValue env status
      throwInterpret . InterpretProcessExit =<< expectIntPrimitiveArgument "exit" statusValue
    GrinCase scrutinee binder alternatives -> do
      value <- materializeValue env scrutinee
      matchScheduledAlternative (Map.insert binder value env) value alternatives continue
    GrinThrow exception -> do
      exceptionValue <- materializeValue env exception
      throwE (EvalRaised exceptionValue)
    GrinCatch runtimeRep action handler state -> do
      actionValue <- materializeValue env action
      handlerValue <- materializeValue env handler
      stateValues <- mapM (materializeValue env) state
      let receive results = do
            let expectedCount = length (runtimeRepComponents runtimeRep)
            case length results - expectedCount of
              0 -> continue results
              1 -> continue (drop 1 results)
              _ -> throwInterpret (InterpretResultArity expectedCount (length results))
      forceScheduledValue actionValue (\forcedAction -> applyScheduledValue forcedAction stateValues receive)
        `catchE` handleScheduledRaised handlerValue stateValues receive
    GrinForeignCallExpr foreignCall arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      continue =<< executeForeignCall foreignCall argumentValues
  where
    rejectCpsExpression = throwInterpret (InterpretCpsExpression expr)

evalScheduledPrimitive :: Text -> [RuntimeValue] -> ScheduledContinuation -> EvalM [RuntimeValue]
evalScheduledPrimitive "fork#" [action] continue = do
  threadId <- allocateCell HeapThread
  enqueueThread
    ( -- The child thread enters its own closure. Forking a thunk is legal, and
      -- an already forced action may sit behind an indirection, so neither is
      -- a value 'applyScheduledValue' can consume directly.
      forceScheduledValue action (\entered -> applyScheduledValue entered [] (const scheduleNextThread))
        `catchE` finishChild
    )
  continue [threadId]
evalScheduledPrimitive "yield#" [] continue = do
  enqueueThread (continue [])
  scheduleNextThread
evalScheduledPrimitive "awaitIO#" [RuntimeIORequest request] continue = do
  completeIORequest request
  continue []
evalScheduledPrimitive "newMVar#" [] continue = do
  identifier <- getsMachine machineNextMVar
  let mvar =
        GrinMVarState
          { grinMVarValue = Nothing,
            grinMVarReaders = Seq.empty,
            grinMVarTakers = Seq.empty,
            grinMVarPutters = Seq.empty
          }
  modifyMachine $ \machine ->
    machine
      { machineMVars = IntMap.insert identifier mvar (machineMVars machine),
        machineNextMVar = identifier + 1
      }
  continue [RuntimeMVar identifier]
evalScheduledPrimitive "readMVar#" [mvarValue] continue = do
  (identifier, mvar) <- expectMVarPrimitiveArgument "readMVar#" mvarValue
  case grinMVarValue mvar of
    Just value -> continue [value]
    Nothing -> do
      let waiter = MVarValueWaiter (\value -> ThreadAction (continue [value]))
      writeMVarState identifier mvar {grinMVarReaders = grinMVarReaders mvar |> waiter}
      scheduleNextThread
evalScheduledPrimitive "takeMVar#" [mvarValue] continue = do
  (identifier, mvar) <- expectMVarPrimitiveArgument "takeMVar#" mvarValue
  case grinMVarValue mvar of
    Nothing -> do
      let waiter = MVarValueWaiter (\value -> ThreadAction (continue [value]))
      writeMVarState identifier mvar {grinMVarTakers = grinMVarTakers mvar |> waiter}
      scheduleNextThread
    Just value -> do
      case Seq.viewl (grinMVarPutters mvar) of
        EmptyL -> writeMVarState identifier mvar {grinMVarValue = Nothing}
        (nextValue, ThreadAction putter) :< remaining -> do
          writeMVarState
            identifier
            mvar
              { grinMVarValue = Just nextValue,
                grinMVarPutters = remaining
              }
          enqueueThread putter
      continue [value]
evalScheduledPrimitive "putMVar#" [mvarValue, value] continue = do
  (identifier, mvar) <- expectMVarPrimitiveArgument "putMVar#" mvarValue
  case grinMVarValue mvar of
    Just _ -> do
      let putter = ThreadAction (continue [])
      writeMVarState identifier mvar {grinMVarPutters = grinMVarPutters mvar |> (value, putter)}
      scheduleNextThread
    Nothing -> do
      mapM_ (enqueueValueWaiter value) (grinMVarReaders mvar)
      case Seq.viewl (grinMVarTakers mvar) of
        EmptyL ->
          writeMVarState
            identifier
            mvar
              { grinMVarValue = Just value,
                grinMVarReaders = Seq.empty
              }
        taker :< remaining -> do
          enqueueValueWaiter value taker
          writeMVarState
            identifier
            mvar
              { grinMVarReaders = Seq.empty,
                grinMVarTakers = remaining
              }
      continue []
evalScheduledPrimitive name arguments continue =
  continue =<< evalPrimitive name arguments

enqueueValueWaiter :: RuntimeValue -> MVarValueWaiter -> EvalM ()
enqueueValueWaiter value (MVarValueWaiter resume) =
  case resume value of
    ThreadAction action -> enqueueThread action

expectMVarPrimitiveArgument :: Text -> RuntimeValue -> EvalM (Int, GrinMVarState)
expectMVarPrimitiveArgument name value =
  case value of
    RuntimeMVar identifier -> do
      mvars <- getsMachine machineMVars
      case IntMap.lookup identifier mvars of
        Just mvar -> pure (identifier, mvar)
        Nothing -> throwInterpret (InterpretPrimitiveTypeError name value)
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

writeMVarState :: Int -> GrinMVarState -> EvalM ()
writeMVarState identifier mvar =
  modifyMachine $ \machine -> machine {machineMVars = IntMap.insert identifier mvar (machineMVars machine)}

finishChild :: EvalFailure -> EvalM [RuntimeValue]
finishChild failure =
  case failure of
    -- An uncaught Haskell exception terminates only the forked thread.
    EvalRaised _ -> scheduleNextThread
    EvalInterpret _ -> throwE failure

enqueueThread :: EvalM [RuntimeValue] -> EvalM ()
enqueueThread action =
  modifyMachine $ \machine ->
    machine {machineRunQueue = machineRunQueue machine |> ThreadAction action}

scheduleNextThread :: EvalM [RuntimeValue]
scheduleNextThread = do
  queue <- getsMachine machineRunQueue
  case Seq.viewl queue of
    EmptyL -> throwInterpret InterpretNoRunnableThreads
    ThreadAction action :< remaining -> do
      modifyMachine $ \machine -> machine {machineRunQueue = remaining}
      action

callScheduledFunction :: FunctionName -> [RuntimeValue] -> ScheduledContinuation -> EvalM [RuntimeValue]
callScheduledFunction functionName arguments continue = do
  function <- lookupFunction functionName
  let parameters = grinFunctionParameters function
  if length parameters == length arguments
    then evalScheduledExpr (Map.fromList (zip parameters arguments)) (grinFunctionBody function) continue
    else throwInterpret (InterpretFunctionArity functionName (length parameters) (length arguments))

applyScheduledValue :: RuntimeValue -> [RuntimeValue] -> ScheduledContinuation -> EvalM [RuntimeValue]
applyScheduledValue function arguments continue = do
  (tag, fields) <- appliedNode function
  case tag of
    GrinClosure functionName remainingLayouts ->
      case remainingLayouts of
        [] -> throwInterpret (InterpretFunctionArity functionName 0 1)
        layout : rest ->
          let normalizedArguments
                | layout == [BoxedRep Lifted], null arguments = [RuntimeStateToken]
                | otherwise = arguments
              appliedFields = fields <> normalizedArguments
           in case rest of
                [] -> callScheduledFunction functionName appliedFields continue
                _ -> do
                  applied <- allocateCell (HeapValue (RuntimeNode (GrinClosure functionName rest) appliedFields))
                  continue [applied]
    GrinConstructor name remaining ->
      case compare remaining 1 of
        GT -> do
          applied <- allocateCell (HeapValue (RuntimeNode (GrinConstructor name (remaining - 1)) (fields <> arguments)))
          continue [applied]
        EQ -> do
          applied <- allocateCell (HeapValue (RuntimeNode (GrinConstructor name 0) (fields <> arguments)))
          continue [applied]
        LT -> throwInterpret (InterpretConstructorArity name 0 1)
    GrinThunk _ -> throwInterpret (InterpretApplyNonFunction function)

forceScheduledValue :: RuntimeValue -> (RuntimeValue -> EvalM [RuntimeValue]) -> EvalM [RuntimeValue]
forceScheduledValue value continue =
  case value of
    RuntimeLocation location -> forceScheduledLocation location continue
    _ -> continue value

forceScheduledLocation :: Int -> (RuntimeValue -> EvalM [RuntimeValue]) -> EvalM [RuntimeValue]
forceScheduledLocation location continue = do
  cell <- readCell location
  case cell of
    HeapSuspended functionName fields -> do
      function <- lookupFunction functionName
      let resultRep = grinFunctionResultRep function
      if isLiftedRuntimeRep resultRep
        then pure ()
        else throwInterpret (InterpretInvalidThunkResultRep functionName resultRep)
      writeCell location HeapBlackhole
      callScheduledFunction functionName fields (updateThunk cell)
        `catchE` \failure -> writeCell location cell >> throwE failure
    HeapValue (RuntimeLocation target) -> forceScheduledLocation target continue
    HeapValue _ -> continue (RuntimeLocation location)
    HeapRaised exception -> throwE (EvalRaised exception)
    HeapBlackhole -> throwInterpret (InterpretBlackhole location)
    HeapThread -> continue (RuntimeLocation location)
  where
    updateThunk original values =
      case values of
        [value] -> do
          writeCell location (HeapValue value)
          forceScheduledLocation location continue
        _ -> do
          writeCell location original
          throwInterpret (InterpretInvalidThunkResult values)

matchScheduledAlternative :: Env -> RuntimeValue -> [GrinAlt] -> ScheduledContinuation -> EvalM [RuntimeValue]
matchScheduledAlternative env value alternatives continue = do
  inspected <- inspectCaseValue value
  go inspected alternatives
  where
    go _ [] = throwInterpret (InterpretNoMatchingAlternative value)
    go inspected (alt : rest) =
      case matchAlt value inspected alt of
        Just bindings -> evalScheduledExpr (bindings `Map.union` env) (grinAltRhs alt) continue
        Nothing -> go inspected rest

handleScheduledRaised :: RuntimeValue -> [RuntimeValue] -> ScheduledContinuation -> EvalFailure -> EvalM [RuntimeValue]
handleScheduledRaised handler state continue failure =
  case failure of
    EvalRaised exception -> applyScheduledValue handler (exception : state) continue
    EvalInterpret err -> throwE (EvalInterpret err)

handleRaised :: RuntimeValue -> [RuntimeValue] -> EvalFailure -> EvalM [RuntimeValue]
handleRaised handler state failure =
  case failure of
    EvalRaised exception -> applyValue handler (exception : state)
    EvalInterpret err -> throwE (EvalInterpret err)

-- | Resolve an atomic GRIN value into its runtime representation. This only
-- captures variables and constructs nodes; it never forces a heap location or
-- enters a thunk.
materializeValue :: Env -> GrinValue -> EvalM RuntimeValue
materializeValue env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var env of
        Just runtimeValue -> pure runtimeValue
        Nothing -> do
          globals <- getsMachine machineGlobals
          case Map.lookup (grinVarName var) globals of
            Just runtimeValue -> pure runtimeValue
            Nothing -> throwInterpret (InterpretUnboundVariable var)
    GrinGlobalValue name -> do
      globals <- getsMachine machineGlobals
      case Map.lookup name globals of
        Just runtimeValue -> pure runtimeValue
        Nothing -> throwInterpret (InterpretMissingBinding name)
    GrinLitValue literal -> pure (RuntimeLit literal)

materializeNode :: Env -> GrinNode -> EvalM RuntimeValue
materializeNode env node =
  RuntimeNode (grinNodeTag node) <$> mapM (materializeValue env) (grinNodeFields node)

storedCell :: RuntimeValue -> HeapCell
storedCell value =
  case value of
    RuntimeNode (GrinThunk functionName) fields -> HeapSuspended functionName fields
    _ -> HeapValue value

allocateCell :: HeapCell -> EvalM RuntimeValue
allocateCell cell = RuntimeLocation <$> allocateLocation cell

allocateLocation :: HeapCell -> EvalM Int
allocateLocation cell = do
  location <- getsMachine machineNextLocation
  modifyMachine $ \machine ->
    machine
      { machineHeap = IntMap.insert location cell (machineHeap machine),
        machineNextLocation = location + 1
      }
  pure location

readCell :: Int -> EvalM HeapCell
readCell location = do
  heap <- getsMachine machineHeap
  case IntMap.lookup location heap of
    Just cell -> pure cell
    Nothing -> throwInterpret (InterpretInvalidLocation location)

writeCell :: Int -> HeapCell -> EvalM ()
writeCell location cell = do
  heap <- getsMachine machineHeap
  if IntMap.member location heap
    then modifyMachine $ \machine -> machine {machineHeap = IntMap.insert location cell heap}
    else throwInterpret (InterpretInvalidLocation location)

fetchValue :: RuntimeValue -> EvalM RuntimeValue
fetchValue value =
  case value of
    RuntimeLocation location -> do
      cell <- readCell location
      case cell of
        HeapSuspended functionName fields -> pure (RuntimeNode (GrinThunk functionName) fields)
        HeapValue result -> pure result
        HeapRaised exception -> throwE (EvalRaised exception)
        HeapBlackhole -> throwInterpret (InterpretBlackhole location)
        HeapThread -> pure (RuntimeLocation location)
    other -> throwInterpret (InterpretExpectedLocation other)

updateValue :: RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
updateValue pointer value =
  if isLiftedRuntimeValue value
    then case pointer of
      RuntimeLocation location -> writeCell location (HeapValue value) >> pure value
      other -> throwInterpret (InterpretExpectedLocation other)
    else throwInterpret (InterpretInvalidUpdateValue value)

forceValue :: RuntimeValue -> EvalM RuntimeValue
forceValue value = expectSingle =<< forceScheduledValue value (pure . (: []))

applyValue :: RuntimeValue -> [RuntimeValue] -> EvalM [RuntimeValue]
applyValue function arguments = applyScheduledValue function arguments pure

appliedNode :: RuntimeValue -> EvalM (GrinNodeTag, [RuntimeValue])
appliedNode function =
  case function of
    RuntimeLocation location -> do
      cell <- readCell location
      case cell of
        HeapValue (RuntimeNode tag fields) -> pure (tag, fields)
        _ -> throwInterpret (InterpretApplyNonFunction function)
    _ -> throwInterpret (InterpretApplyNonFunction function)

callFunction :: FunctionName -> [RuntimeValue] -> EvalM [RuntimeValue]
callFunction functionName arguments = callScheduledFunction functionName arguments pure

lookupFunction :: FunctionName -> EvalM GrinFunction
lookupFunction functionName = do
  functions <- getsMachine machineFunctions
  case Map.lookup functionName functions of
    Nothing -> throwInterpret (InterpretUnknownFunction functionName)
    Just function -> pure function

isLiftedRuntimeValue :: RuntimeValue -> Bool
isLiftedRuntimeValue value =
  case value of
    RuntimeLit literal -> isLiftedRuntimeRep (grinValueRuntimeRep (GrinLitValue literal))
    RuntimeAddress {} -> False
    RuntimeArray {} -> False
    RuntimeIOHandle {} -> False
    RuntimeByteArray {} -> False
    RuntimeIOError {} -> False
    RuntimeIORequest {} -> False
    RuntimeMVar {} -> False
    RuntimeNode {} -> True
    RuntimeLocation {} -> True
    RuntimeMutVar {} -> False
    RuntimeStableName {} -> False
    RuntimeStateToken -> False

evalPrimitive :: Text -> [RuntimeValue] -> EvalM [RuntimeValue]
evalPrimitive "+#" [left, right] = evalIntPrimitive "+#" (+) left right
evalPrimitive "-#" [left, right] = evalIntPrimitive "-#" (-) left right
evalPrimitive "*#" [left, right] = evalIntPrimitive "*#" (*) left right
evalPrimitive "addIntC#" [left, right] = evalIntCarryPrimitive "addIntC#" (+) left right
evalPrimitive "subIntC#" [left, right] = evalIntCarryPrimitive "subIntC#" (-) left right
evalPrimitive "plusWord#" [left, right] = evalWordPrimitive "plusWord#" (+) left right
evalPrimitive "minusWord#" [left, right] = evalWordPrimitive "minusWord#" (-) left right
evalPrimitive "timesWord#" [left, right] = evalWordPrimitive "timesWord#" (*) left right
evalPrimitive "addWordC#" [left, right] = evalWordCarryPrimitive "addWordC#" left right
evalPrimitive "subWordC#" [left, right] = evalWordBorrowPrimitive "subWordC#" left right
evalPrimitive "timesInt2#" [left, right] = do
  leftInt <- expectIntPrimitiveArgument "timesInt2#" left
  rightInt <- expectIntPrimitiveArgument "timesInt2#" right
  let doubleWord = leftInt * rightInt
      low = normalizeInt doubleWord
      high = normalizeInt (shiftR doubleWord wordBits)
      highNeeded = if high == shiftR low (wordBits - 1) then 0 else 1
  pure [intRuntimeValue highNeeded, intRuntimeValue high, intRuntimeValue low]
evalPrimitive "timesWord2#" [left, right] = do
  leftWord <- expectWordPrimitiveArgument "timesWord2#" left
  rightWord <- expectWordPrimitiveArgument "timesWord2#" right
  let productValue = leftWord * rightWord
  pure [wordRuntimeValue (productValue `shiftR` wordBits), wordRuntimeValue productValue]
evalPrimitive "quotWord#" [left, right] = evalWordPrimitive "quotWord#" quot left right
evalPrimitive "remWord#" [left, right] = evalWordPrimitive "remWord#" rem left right
evalPrimitive "quotRemWord#" [left, right] = do
  leftWord <- expectWordPrimitiveArgument "quotRemWord#" left
  rightWord <- expectWordPrimitiveArgument "quotRemWord#" right
  let (quotient, remainder) = leftWord `quotRem` rightWord
  pure [wordRuntimeValue quotient, wordRuntimeValue remainder]
evalPrimitive "quotRemWord2#" [high, low, divisor] = do
  highWord <- expectWordPrimitiveArgument "quotRemWord2#" high
  lowWord <- expectWordPrimitiveArgument "quotRemWord2#" low
  divisorWord <- expectWordPrimitiveArgument "quotRemWord2#" divisor
  let dividend = highWord `shiftL` wordBits .|. lowWord
      (quotient, remainder) = dividend `quotRem` divisorWord
  pure [wordRuntimeValue quotient, wordRuntimeValue remainder]
evalPrimitive "and#" [left, right] = evalWordPrimitive "and#" (.&.) left right
evalPrimitive "or#" [left, right] = evalWordPrimitive "or#" (.|.) left right
evalPrimitive "xor#" [left, right] = evalWordPrimitive "xor#" xor left right
evalPrimitive "not#" [value] = do
  word <- expectWordPrimitiveArgument "not#" value
  pure [wordRuntimeValue (complement word)]
evalPrimitive "uncheckedShiftL#" [value, amount] = evalWordShift "uncheckedShiftL#" shiftL value amount
evalPrimitive "uncheckedShiftRL#" [value, amount] = evalWordShift "uncheckedShiftRL#" shiftR value amount
evalPrimitive "int2Word#" [value] = do
  int <- expectIntPrimitiveArgument "int2Word#" value
  pure [wordRuntimeValue int]
evalPrimitive "word2Int#" [value] = do
  word <- expectWordPrimitiveArgument "word2Int#" value
  pure [intRuntimeValue word]
evalPrimitive "word8ToWord#" [value] =
  (: []) . wordRuntimeValue <$> expectRuntimeRepPrimitiveArgument "word8ToWord#" Word8Rep value
evalPrimitive "word32ToWord#" [value] =
  (: []) . wordRuntimeValue <$> expectRuntimeRepPrimitiveArgument "word32ToWord#" Word32Rep value
evalPrimitive "word64ToWord#" [value] =
  (: []) . wordRuntimeValue <$> expectRuntimeRepPrimitiveArgument "word64ToWord#" Word64Rep value
evalPrimitive "eqWord#" [left, right] = evalWordComparison "eqWord#" (==) left right
evalPrimitive "neWord#" [left, right] = evalWordComparison "neWord#" (/=) left right
evalPrimitive "ltWord#" [left, right] = evalWordComparison "ltWord#" (<) left right
evalPrimitive "leWord#" [left, right] = evalWordComparison "leWord#" (<=) left right
evalPrimitive "gtWord#" [left, right] = evalWordComparison "gtWord#" (>) left right
evalPrimitive "geWord#" [left, right] = evalWordComparison "geWord#" (>=) left right
evalPrimitive "clz#" [value] = evalWordCount "clz#" countLeadingZeros value
evalPrimitive "intToInt8#" [value] = evalIntNarrow "intToInt8#" Int8Rep 8 value
evalPrimitive "intToInt16#" [value] = evalIntNarrow "intToInt16#" Int16Rep 16 value
evalPrimitive "intToInt32#" [value] = evalIntNarrow "intToInt32#" Int32Rep 32 value
evalPrimitive "intToInt64#" [value] = evalIntNarrow "intToInt64#" Int64Rep 64 value
evalPrimitive "int8ToInt#" [value] =
  (: []) . intRuntimeValue <$> expectRuntimeRepPrimitiveArgument "int8ToInt#" Int8Rep value
evalPrimitive "int16ToInt#" [value] =
  (: []) . intRuntimeValue <$> expectRuntimeRepPrimitiveArgument "int16ToInt#" Int16Rep value
evalPrimitive "int32ToInt#" [value] =
  (: []) . intRuntimeValue <$> expectRuntimeRepPrimitiveArgument "int32ToInt#" Int32Rep value
evalPrimitive "int64ToInt#" [value] =
  (: []) . intRuntimeValue <$> expectRuntimeRepPrimitiveArgument "int64ToInt#" Int64Rep value
evalPrimitive "plusFloat#" [left, right] = evalFloatBinary "plusFloat#" (+) left right
evalPrimitive "minusFloat#" [left, right] = evalFloatBinary "minusFloat#" (-) left right
evalPrimitive "timesFloat#" [left, right] = evalFloatBinary "timesFloat#" (*) left right
evalPrimitive "negateFloat#" [value] = evalFloatUnary "negateFloat#" negate value
evalPrimitive "fabsFloat#" [value] = evalFloatUnary "fabsFloat#" abs value
evalPrimitive "int2Float#" [value] = do
  int <- expectIntPrimitiveArgument "int2Float#" value
  pure [floatRuntimeValue (fromInteger int)]
evalPrimitive "float2Int#" [value] = do
  float <- expectFloatPrimitiveArgument "float2Int#" value
  pure [intRuntimeValue (truncate float)]
evalPrimitive "gtFloat#" [left, right] = evalFloatComparison "gtFloat#" (>) left right
evalPrimitive "ltFloat#" [left, right] = evalFloatComparison "ltFloat#" (<) left right
evalPrimitive "eqFloat#" [left, right] = evalFloatComparison "eqFloat#" (==) left right
evalPrimitive "+##" [left, right] = evalDoubleBinary "+##" (+) left right
evalPrimitive "-##" [left, right] = evalDoubleBinary "-##" (-) left right
evalPrimitive "*##" [left, right] = evalDoubleBinary "*##" (*) left right
evalPrimitive "negateDouble#" [value] = evalDoubleUnary "negateDouble#" negate value
evalPrimitive "fabsDouble#" [value] = evalDoubleUnary "fabsDouble#" abs value
evalPrimitive "sqrtDouble#" [value] = evalDoubleUnary "sqrtDouble#" sqrt value
evalPrimitive "expDouble#" [value] = evalDoubleUnary "expDouble#" exp value
evalPrimitive "logDouble#" [value] = evalDoubleUnary "logDouble#" log value
evalPrimitive "sinDouble#" [value] = evalDoubleUnary "sinDouble#" sin value
evalPrimitive "cosDouble#" [value] = evalDoubleUnary "cosDouble#" cos value
evalPrimitive "tanDouble#" [value] = evalDoubleUnary "tanDouble#" tan value
evalPrimitive "asinDouble#" [value] = evalDoubleUnary "asinDouble#" asin value
evalPrimitive "acosDouble#" [value] = evalDoubleUnary "acosDouble#" acos value
evalPrimitive "atanDouble#" [value] = evalDoubleUnary "atanDouble#" atan value
evalPrimitive "sinhDouble#" [value] = evalDoubleUnary "sinhDouble#" sinh value
evalPrimitive "coshDouble#" [value] = evalDoubleUnary "coshDouble#" cosh value
evalPrimitive "tanhDouble#" [value] = evalDoubleUnary "tanhDouble#" tanh value
evalPrimitive "asinhDouble#" [value] = evalDoubleUnary "asinhDouble#" asinh value
evalPrimitive "acoshDouble#" [value] = evalDoubleUnary "acoshDouble#" acosh value
evalPrimitive "atanhDouble#" [value] = evalDoubleUnary "atanhDouble#" atanh value
evalPrimitive "/##" [left, right] = evalDoubleBinary "/##" (/) left right
evalPrimitive "**##" [left, right] = evalDoubleBinary "**##" (**) left right
evalPrimitive "sqrtFloat#" [value] = evalFloatUnary "sqrtFloat#" sqrt value
evalPrimitive "expFloat#" [value] = evalFloatUnary "expFloat#" exp value
evalPrimitive "logFloat#" [value] = evalFloatUnary "logFloat#" log value
evalPrimitive "sinFloat#" [value] = evalFloatUnary "sinFloat#" sin value
evalPrimitive "cosFloat#" [value] = evalFloatUnary "cosFloat#" cos value
evalPrimitive "tanFloat#" [value] = evalFloatUnary "tanFloat#" tan value
evalPrimitive "asinFloat#" [value] = evalFloatUnary "asinFloat#" asin value
evalPrimitive "acosFloat#" [value] = evalFloatUnary "acosFloat#" acos value
evalPrimitive "atanFloat#" [value] = evalFloatUnary "atanFloat#" atan value
evalPrimitive "sinhFloat#" [value] = evalFloatUnary "sinhFloat#" sinh value
evalPrimitive "coshFloat#" [value] = evalFloatUnary "coshFloat#" cosh value
evalPrimitive "tanhFloat#" [value] = evalFloatUnary "tanhFloat#" tanh value
evalPrimitive "asinhFloat#" [value] = evalFloatUnary "asinhFloat#" asinh value
evalPrimitive "acoshFloat#" [value] = evalFloatUnary "acoshFloat#" acosh value
evalPrimitive "atanhFloat#" [value] = evalFloatUnary "atanhFloat#" atanh value
evalPrimitive "divideFloat#" [left, right] = evalFloatBinary "divideFloat#" (/) left right
evalPrimitive "powerFloat#" [left, right] = evalFloatBinary "powerFloat#" (**) left right
evalPrimitive "int2Double#" [value] = do
  int <- expectIntPrimitiveArgument "int2Double#" value
  pure [doubleRuntimeValue (fromInteger int)]
evalPrimitive "double2Int#" [value] = do
  double <- expectDoublePrimitiveArgument "double2Int#" value
  pure [intRuntimeValue (truncate double)]
evalPrimitive "float2Double#" [value] = do
  float <- expectFloatPrimitiveArgument "float2Double#" value
  pure [doubleRuntimeValue (float2Double float)]
evalPrimitive "double2Float#" [value] = do
  double <- expectDoublePrimitiveArgument "double2Float#" value
  pure [floatRuntimeValue (double2Float double)]
evalPrimitive "castFloatToWord32#" [value] =
  (: []) . RuntimeLit . GrinLitInt Word32Rep <$> expectRuntimeRepPrimitiveArgument "castFloatToWord32#" FloatRep value
evalPrimitive "castWord32ToFloat#" [value] =
  (: []) . RuntimeLit . GrinLitInt FloatRep <$> expectRuntimeRepPrimitiveArgument "castWord32ToFloat#" Word32Rep value
evalPrimitive "castDoubleToWord64#" [value] =
  (: []) . RuntimeLit . GrinLitInt Word64Rep <$> expectRuntimeRepPrimitiveArgument "castDoubleToWord64#" DoubleRep value
evalPrimitive "castWord64ToDouble#" [value] =
  (: []) . RuntimeLit . GrinLitInt DoubleRep <$> expectRuntimeRepPrimitiveArgument "castWord64ToDouble#" Word64Rep value
evalPrimitive ">##" [left, right] = evalDoubleComparison ">##" (>) left right
evalPrimitive "<##" [left, right] = evalDoubleComparison "<##" (<) left right
evalPrimitive "==##" [left, right] = evalDoubleComparison "==##" (==) left right
evalPrimitive "ctz#" [value] = evalWordCount "ctz#" countTrailingZeros value
evalPrimitive "popCnt#" [value] = evalWordCount "popCnt#" popCount value
evalPrimitive "byteSwap16#" [value] = evalByteSwap "byteSwap16#" WordRep 2 value
evalPrimitive "byteSwap32#" [value] = evalByteSwap "byteSwap32#" WordRep 4 value
evalPrimitive "byteSwap64#" [value] = evalByteSwap "byteSwap64#" Word64Rep 8 value
evalPrimitive "byteSwap#" [value] = evalByteSwap "byteSwap#" WordRep 8 value
evalPrimitive "compareInt#" [left, right] = evalIntPrimitive "compareInt#" compareInts left right
evalPrimitive "<#" [left, right] =
  evalIntPrimitive "<#" (\leftInt rightInt -> if leftInt < rightInt then 1 else 0) left right
evalPrimitive "==#" [left, right] =
  evalIntPrimitive "==#" (\leftInt rightInt -> if leftInt == rightInt then 1 else 0) left right
evalPrimitive ">#" [left, right] =
  evalIntPrimitive ">#" (\leftInt rightInt -> if leftInt > rightInt then 1 else 0) left right
evalPrimitive ">=#" [left, right] =
  evalIntPrimitive ">=#" (\leftInt rightInt -> if leftInt >= rightInt then 1 else 0) left right
evalPrimitive "<=#" [left, right] =
  evalIntPrimitive "<=#" (\leftInt rightInt -> if leftInt <= rightInt then 1 else 0) left right
evalPrimitive "/=#" [left, right] =
  evalIntPrimitive "/=#" (\leftInt rightInt -> if leftInt /= rightInt then 1 else 0) left right
evalPrimitive "eqWord64#" [left, right] = evalWord64Comparison "eqWord64#" (==) left right
evalPrimitive "neWord64#" [left, right] = evalWord64Comparison "neWord64#" (/=) left right
evalPrimitive "ltWord64#" [left, right] = evalWord64Comparison "ltWord64#" (<) left right
evalPrimitive "leWord64#" [left, right] = evalWord64Comparison "leWord64#" (<=) left right
evalPrimitive "gtWord64#" [left, right] = evalWord64Comparison "gtWord64#" (>) left right
evalPrimitive "geWord64#" [left, right] = evalWord64Comparison "geWord64#" (>=) left right
evalPrimitive "wordToWord8#" [value] = evalWordNarrow "wordToWord8#" Word8Rep 0xff value
evalPrimitive "wordToWord16#" [value] = evalWordNarrow "wordToWord16#" Word16Rep 0xffff value
evalPrimitive "wordToWord32#" [value] = evalWordNarrow "wordToWord32#" Word32Rep 0xffffffff value
evalPrimitive "wordToWord64#" [value] = evalWordNarrow "wordToWord64#" Word64Rep wordMask value
evalPrimitive "word16ToWord#" [value] =
  (: []) . wordRuntimeValue <$> expectRuntimeRepPrimitiveArgument "word16ToWord#" Word16Rep value
evalPrimitive "touch#" [_] = pure []
evalPrimitive "ord#" [value] = do
  charValue <- expectCharPrimitiveArgument "ord#" value
  pure [RuntimeLit (GrinLitInt IntRep (fromIntegral (Char.ord charValue)))]
evalPrimitive "chr#" [value] = do
  intValue <- expectIntPrimitiveArgument "chr#" value
  if intValue >= 0 && intValue <= 0x10ffff
    then pure [RuntimeLit (GrinLitChar WordRep (Char.chr (fromIntegral intValue)))]
    else throwInterpret (InterpretPrimitiveTypeError "chr#" (RuntimeLit (GrinLitInt IntRep intValue)))
evalPrimitive "realWorld#" [] = pure []
evalPrimitive "noDuplicate#" [] = pure []
evalPrimitive "makeStableName#" [value] = do
  name <- liftEvalIO (Host.makeStableName value)
  pure [RuntimeStableName (GrinStableName name)]
evalPrimitive "stableNameToInt#" [name] = do
  GrinStableName stableName <- expectStableNamePrimitiveArgument "stableNameToInt#" name
  pure [intRuntimeValue (toInteger (Host.hashStableName stableName))]
evalPrimitive "eqStableName#" [left, right] = do
  GrinStableName leftName <- expectStableNamePrimitiveArgument "eqStableName#" left
  GrinStableName rightName <- expectStableNamePrimitiveArgument "eqStableName#" right
  pure [intRuntimeValue (if Host.eqStableName leftName rightName then 1 else 0)]
evalPrimitive "raise#" [exception] =
  throwE (EvalRaised exception)
evalPrimitive "catch#" [action, handler] =
  applyValue action [] `catchE` handleRaised handler []
evalPrimitive "runRW#" [action] =
  applyValue action []
evalPrimitive "newMutVar#" [initialValue] = do
  mutVar <- GrinMutVar <$> liftEvalIO (newIORef initialValue)
  pure [RuntimeMutVar mutVar]
evalPrimitive "readMutVar#" [mutVar] = do
  GrinMutVar reference <- expectMutVarPrimitiveArgument "readMutVar#" mutVar
  value <- liftEvalIO (readIORef reference)
  pure [value]
evalPrimitive "writeMutVar#" [mutVar, value] = do
  GrinMutVar reference <- expectMutVarPrimitiveArgument "writeMutVar#" mutVar
  liftEvalIO (writeIORef reference value)
  pure []
evalPrimitive "casMutVar#" [mutVar, expected, replacement] = do
  GrinMutVar reference <- expectMutVarPrimitiveArgument "casMutVar#" mutVar
  current <- liftEvalIO (readIORef reference)
  let succeeded = current == expected
  when succeeded (liftEvalIO (writeIORef reference replacement))
  pure [intRuntimeValue (if succeeded then 0 else 1), if succeeded then replacement else current]
evalPrimitive "sameMutVar#" [left, right] = do
  leftReference <- expectMutVarPrimitiveArgument "sameMutVar#" left
  rightReference <- expectMutVarPrimitiveArgument "sameMutVar#" right
  pure [intRuntimeValue (if leftReference == rightReference then 1 else 0)]
evalPrimitive "newArray#" [size, initialValue] = do
  count <- checkedArraySize "newArray#" =<< expectIntPrimitiveArgument "newArray#" size
  array <- GrinArray <$> liftEvalIO (newIORef (replicate count initialValue))
  pure [RuntimeArray array]
evalPrimitive "indexArray#" [arrayValue, indexValue] = do
  array <- expectArrayPrimitiveArgument "indexArray#" arrayValue
  index <- expectIntPrimitiveArgument "indexArray#" indexValue
  (: []) <$> readArrayElement "indexArray#" array index
evalPrimitive "readArray#" [arrayValue, indexValue] = do
  array <- expectArrayPrimitiveArgument "readArray#" arrayValue
  index <- expectIntPrimitiveArgument "readArray#" indexValue
  (: []) <$> readArrayElement "readArray#" array index
evalPrimitive "writeArray#" [arrayValue, indexValue, value] = do
  array <- expectArrayPrimitiveArgument "writeArray#" arrayValue
  index <- expectIntPrimitiveArgument "writeArray#" indexValue
  writeArrayElement "writeArray#" array index value
  pure []
evalPrimitive name [arrayValue]
  | name == "unsafeFreezeArray#" || name == "unsafeThawArray#" = do
      array <- expectArrayPrimitiveArgument name arrayValue
      pure [RuntimeArray array]
evalPrimitive "sameMutableArray#" [left, right] = do
  leftArray <- expectArrayPrimitiveArgument "sameMutableArray#" left
  rightArray <- expectArrayPrimitiveArgument "sameMutableArray#" right
  pure [intRuntimeValue (if leftArray == rightArray then 1 else 0)]
evalPrimitive "newByteArray#" [size] = do
  byteArray <- allocateByteArray "newByteArray#" False 8 =<< expectIntPrimitiveArgument "newByteArray#" size
  pure [RuntimeByteArray byteArray]
evalPrimitive "newPinnedByteArray#" [size] = do
  byteArray <- allocateByteArray "newPinnedByteArray#" True 8 =<< expectIntPrimitiveArgument "newPinnedByteArray#" size
  pure [RuntimeByteArray byteArray]
evalPrimitive "newAlignedPinnedByteArray#" [size, alignment] = do
  byteCount <- expectIntPrimitiveArgument "newAlignedPinnedByteArray#" size
  byteAlignment <- expectIntPrimitiveArgument "newAlignedPinnedByteArray#" alignment
  checkedAlignment <- checkedByteArrayAlignment "newAlignedPinnedByteArray#" byteAlignment
  byteArray <- allocateByteArray "newAlignedPinnedByteArray#" True checkedAlignment byteCount
  pure [RuntimeByteArray byteArray]
evalPrimitive "isMutableByteArrayPinned#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "isMutableByteArrayPinned#" value
  pure [RuntimeLit (GrinLitInt IntRep (if grinByteArrayPinned byteArray then 1 else 0))]
evalPrimitive "isByteArrayPinned#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "isByteArrayPinned#" value
  pure [RuntimeLit (GrinLitInt IntRep (if grinByteArrayPinned byteArray then 1 else 0))]
evalPrimitive "byteArrayContents#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "byteArrayContents#" value
  pure [RuntimeAddress (grinByteArrayContents byteArray)]
evalPrimitive "mutableByteArrayContents#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "mutableByteArrayContents#" value
  pure [RuntimeAddress (grinByteArrayContents byteArray)]
evalPrimitive "shrinkMutableByteArray#" [value, newSize] = do
  byteArray <- expectByteArrayPrimitiveArgument "shrinkMutableByteArray#" value
  byteCount <- checkedByteArraySize "shrinkMutableByteArray#" =<< expectIntPrimitiveArgument "shrinkMutableByteArray#" newSize
  oldSize <- liftEvalIO (readIORef (grinByteArraySize byteArray))
  if byteCount > oldSize
    then throwInterpret (InterpretInvalidByteArrayRange "shrinkMutableByteArray#" 0 (toInteger byteCount) oldSize)
    else liftEvalIO (writeIORef (grinByteArraySize byteArray) byteCount)
  pure []
evalPrimitive "resizeMutableByteArray#" [value, newSize] = do
  byteArray <- expectByteArrayPrimitiveArgument "resizeMutableByteArray#" value
  byteCount <- expectIntPrimitiveArgument "resizeMutableByteArray#" newSize
  resized <- resizeByteArray "resizeMutableByteArray#" byteArray byteCount
  pure [RuntimeByteArray resized]
evalPrimitive "unsafeFreezeByteArray#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "unsafeFreezeByteArray#" value
  pure [RuntimeByteArray byteArray]
evalPrimitive "unsafeThawByteArray#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "unsafeThawByteArray#" value
  pure [RuntimeByteArray byteArray]
evalPrimitive "sizeofByteArray#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "sizeofByteArray#" value
  size <- liftEvalIO (readIORef (grinByteArraySize byteArray))
  pure [RuntimeLit (GrinLitInt IntRep (toInteger size))]
evalPrimitive "getSizeofMutableByteArray#" [value] = do
  byteArray <- expectByteArrayPrimitiveArgument "getSizeofMutableByteArray#" value
  size <- liftEvalIO (readIORef (grinByteArraySize byteArray))
  pure [RuntimeLit (GrinLitInt IntRep (toInteger size))]
evalPrimitive "indexWord8OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word8Rep <$> indexAddressPrimitive "indexWord8OffAddr#" 1 readAddressWord8 address index
evalPrimitive "indexWord32OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word32Rep <$> indexAddressPrimitive "indexWord32OffAddr#" 4 readAddressWord32 address index
evalPrimitive "indexWord64OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word64Rep <$> indexAddressPrimitive "indexWord64OffAddr#" 8 readAddressWord64 address index
evalPrimitive "indexWord16OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word16Rep <$> indexAddressPrimitive "indexWord16OffAddr#" 2 readAddressWord16 address index
evalPrimitive "readWord8OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word8Rep <$> indexAddressPrimitive "readWord8OffAddr#" 1 readAddressWord8 address index
evalPrimitive "readWord16OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word16Rep <$> indexAddressPrimitive "readWord16OffAddr#" 2 readAddressWord16 address index
evalPrimitive "readWord32OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word32Rep <$> indexAddressPrimitive "readWord32OffAddr#" 4 readAddressWord32 address index
evalPrimitive "readWord64OffAddr#" [address, index] =
  (: []) . RuntimeLit . GrinLitInt Word64Rep <$> indexAddressPrimitive "readWord64OffAddr#" 8 readAddressWord64 address index
evalPrimitive "indexWord8OffAddrAsWord16#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt Word16Rep <$> readAddressPrimitive "indexWord8OffAddrAsWord16#" 1 2 readAddressWord16 address offset
evalPrimitive "indexWord8OffAddrAsWord32#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt Word32Rep <$> readAddressPrimitive "indexWord8OffAddrAsWord32#" 1 4 readAddressWord32 address offset
evalPrimitive "indexWord8OffAddrAsWord64#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt Word64Rep <$> readAddressPrimitive "indexWord8OffAddrAsWord64#" 1 8 readAddressWord64 address offset
evalPrimitive "readWord8OffAddrAsWord16#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt Word16Rep <$> readAddressPrimitive "readWord8OffAddrAsWord16#" 1 2 readAddressWord16 address offset
evalPrimitive "readWord8OffAddrAsWord32#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt Word32Rep <$> readAddressPrimitive "readWord8OffAddrAsWord32#" 1 4 readAddressWord32 address offset
evalPrimitive "readWord8OffAddrAsWord64#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt Word64Rep <$> readAddressPrimitive "readWord8OffAddrAsWord64#" 1 8 readAddressWord64 address offset
evalPrimitive "indexWord8OffAddrAsFloat#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt FloatRep <$> readAddressPrimitive "indexWord8OffAddrAsFloat#" 1 4 readAddressWord32 address offset
evalPrimitive "indexWord8OffAddrAsDouble#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt DoubleRep <$> readAddressPrimitive "indexWord8OffAddrAsDouble#" 1 8 readAddressWord64 address offset
evalPrimitive "readWord8OffAddrAsFloat#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt FloatRep <$> readAddressPrimitive "readWord8OffAddrAsFloat#" 1 4 readAddressWord32 address offset
evalPrimitive "readWord8OffAddrAsDouble#" [address, offset] =
  (: []) . RuntimeLit . GrinLitInt DoubleRep <$> readAddressPrimitive "readWord8OffAddrAsDouble#" 1 8 readAddressWord64 address offset
evalPrimitive "writeWord8OffAddr#" [address, index, value] =
  writeAddressPrimitive "writeWord8OffAddr#" 1 Word8Rep writeAddressWord8 address index value
evalPrimitive "writeWord16OffAddr#" [address, index, value] =
  writeAddressPrimitive "writeWord16OffAddr#" 2 Word16Rep writeAddressWord16 address index value
evalPrimitive "writeWord32OffAddr#" [address, index, value] =
  writeAddressPrimitive "writeWord32OffAddr#" 4 Word32Rep writeAddressWord32 address index value
evalPrimitive "writeWord64OffAddr#" [address, index, value] =
  writeAddressPrimitive "writeWord64OffAddr#" 8 Word64Rep writeAddressWord64 address index value
evalPrimitive "writeWord8OffAddrAsWord16#" [address, offset, value] =
  writeAddressPrimitive "writeWord8OffAddrAsWord16#" 1 Word16Rep writeAddressWord16 address offset value
evalPrimitive "writeWord8OffAddrAsWord32#" [address, offset, value] =
  writeAddressPrimitive "writeWord8OffAddrAsWord32#" 1 Word32Rep writeAddressWord32 address offset value
evalPrimitive "writeWord8OffAddrAsWord64#" [address, offset, value] =
  writeAddressPrimitive "writeWord8OffAddrAsWord64#" 1 Word64Rep writeAddressWord64 address offset value
evalPrimitive "writeWord8OffAddrAsFloat#" [address, offset, value] =
  writeAddressPrimitive "writeWord8OffAddrAsFloat#" 1 FloatRep writeAddressWord32 address offset value
evalPrimitive "writeWord8OffAddrAsDouble#" [address, offset, value] =
  writeAddressPrimitive "writeWord8OffAddrAsDouble#" 1 DoubleRep writeAddressWord64 address offset value
evalPrimitive "plusAddr#" [address, offset] = do
  byteOffset <- expectIntPrimitiveArgument "plusAddr#" offset
  (: []) <$> addressPlus "plusAddr#" address byteOffset
evalPrimitive "minusAddr#" [left, right] = do
  leftPointer <- expectAddress "minusAddr#" left
  rightPointer <- expectAddress "minusAddr#" right
  pure [intRuntimeValue (toInteger (leftPointer `minusPtr` rightPointer))]
evalPrimitive "eqAddr#" [left, right] = evalAddressComparison "eqAddr#" (==) left right
-- Structural equality is stronger than object identity. Two equal values
-- give 1#, which is a safe answer for the callers that use the result
-- only to skip work.
evalPrimitive "reallyUnsafePtrEquality#" [left, right] = pure [intRuntimeValue (if left == right then 1 else 0)]
evalPrimitive "neAddr#" [left, right] = evalAddressComparison "neAddr#" (/=) left right
evalPrimitive "ltAddr#" [left, right] = evalAddressComparison "ltAddr#" (<) left right
evalPrimitive "leAddr#" [left, right] = evalAddressComparison "leAddr#" (<=) left right
evalPrimitive "gtAddr#" [left, right] = evalAddressComparison "gtAddr#" (>) left right
evalPrimitive "geAddr#" [left, right] = evalAddressComparison "geAddr#" (>=) left right
evalPrimitive "addr2Int#" [address] = do
  pointer <- expectAddress "addr2Int#" address
  pure [intRuntimeValue (addressOrdinal pointer)]
evalPrimitive "int2Addr#" [value] = do
  ordinal <- expectIntPrimitiveArgument "int2Addr#" value
  pure [RuntimeAddress (intPtrToPtr (IntPtr (fromInteger ordinal)))]
evalPrimitive "cstringLength#" [address] =
  case address of
    RuntimeLit (GrinLitAddr bytes) -> pure [intRuntimeValue (toInteger (BS.length (BS.takeWhile (/= 0) bytes)))]
    RuntimeAddress pointer -> do
      bytes <- liftEvalIO (BS.packCString (castPtr pointer))
      pure [intRuntimeValue (toInteger (BS.length bytes))]
    other -> throwInterpret (InterpretForeignTypeError "cstringLength#" other)
evalPrimitive "indexWordArray#" [value, index] = do
  byteArray <- expectByteArrayPrimitiveArgument "indexWordArray#" value
  wordIndex <- expectIntPrimitiveArgument "indexWordArray#" index
  byteOffset <- checkedWordArrayIndex "indexWordArray#" byteArray wordIndex
  word <- liftEvalIO (peekByteOff (grinByteArrayContents byteArray) byteOffset :: IO Word64)
  pure [wordRuntimeValue (toInteger word)]
evalPrimitive "readWordArray#" [value, index] = do
  byteArray <- expectByteArrayPrimitiveArgument "readWordArray#" value
  wordIndex <- expectIntPrimitiveArgument "readWordArray#" index
  byteOffset <- checkedWordArrayIndex "readWordArray#" byteArray wordIndex
  word <- liftEvalIO (peekByteOff (grinByteArrayContents byteArray) byteOffset :: IO Word64)
  pure [wordRuntimeValue (toInteger word)]
evalPrimitive "indexCharArray#" [value, index] = do
  byte <- readByteArrayElement "indexCharArray#" 1 1 readAddressWord8 value index
  pure [RuntimeLit (GrinLitChar WordRep (Char.chr (fromInteger byte)))]
evalPrimitive "indexWord8ArrayAsWord16#" [value, offset] =
  (: []) . RuntimeLit . GrinLitInt Word16Rep <$> readByteArrayElement "indexWord8ArrayAsWord16#" 1 2 readAddressWord16 value offset
evalPrimitive "indexWord8ArrayAsWord32#" [value, offset] =
  (: []) . RuntimeLit . GrinLitInt Word32Rep <$> readByteArrayElement "indexWord8ArrayAsWord32#" 1 4 readAddressWord32 value offset
evalPrimitive "indexWord8ArrayAsWord64#" [value, offset] =
  (: []) . RuntimeLit . GrinLitInt Word64Rep <$> readByteArrayElement "indexWord8ArrayAsWord64#" 1 8 readAddressWord64 value offset
evalPrimitive "writeWordArray#" [value, index, wordValue] = do
  byteArray <- expectByteArrayPrimitiveArgument "writeWordArray#" value
  wordIndex <- expectIntPrimitiveArgument "writeWordArray#" index
  word <- expectWordPrimitiveArgument "writeWordArray#" wordValue
  byteOffset <- checkedWordArrayIndex "writeWordArray#" byteArray wordIndex
  liftEvalIO (pokeByteOff (grinByteArrayContents byteArray) byteOffset (fromInteger word :: Word64))
  pure []
evalPrimitive "copyByteArray#" [sourceValue, sourceOffset, destinationValue, destinationOffset, byteCount] = do
  source <- expectByteArrayPrimitiveArgument "copyByteArray#" sourceValue
  destination <- expectByteArrayPrimitiveArgument "copyByteArray#" destinationValue
  checkedSourceOffset <- expectIntPrimitiveArgument "copyByteArray#" sourceOffset
  checkedDestinationOffset <- expectIntPrimitiveArgument "copyByteArray#" destinationOffset
  checkedLength <- expectIntPrimitiveArgument "copyByteArray#" byteCount
  (sourceStart, sourceLength) <- checkedByteArrayRange "copyByteArray#" source checkedSourceOffset checkedLength
  (destinationStart, destinationLength) <- checkedByteArrayRange "copyByteArray#" destination checkedDestinationOffset checkedLength
  liftEvalIO
    ( copyBytes
        (grinByteArrayContents destination `plusPtr` destinationStart)
        (grinByteArrayContents source `plusPtr` sourceStart)
        (min sourceLength destinationLength)
    )
  pure []
evalPrimitive "copyMutableByteArray#" arguments = evalPrimitive "copyByteArray#" arguments
evalPrimitive "copyMutableByteArrayToAddr#" arguments = evalPrimitive "copyByteArrayToAddr#" arguments
evalPrimitive "copyByteArrayToAddr#" [value, offset, destination, byteCount] = do
  byteArray <- expectByteArrayPrimitiveArgument "copyByteArrayToAddr#" value
  checkedOffset <- expectIntPrimitiveArgument "copyByteArrayToAddr#" offset
  checkedLength <- expectIntPrimitiveArgument "copyByteArrayToAddr#" byteCount
  (sourceOffset, sourceLength) <- checkedByteArrayRange "copyByteArrayToAddr#" byteArray checkedOffset checkedLength
  pointer <- expectAddress "copyByteArrayToAddr#" destination
  liftEvalIO (copyBytes pointer (grinByteArrayContents byteArray `plusPtr` sourceOffset) sourceLength)
  pure []
evalPrimitive "compareByteArrays#" [leftValue, leftOffset, rightValue, rightOffset, byteCount] = do
  left <- expectByteArrayPrimitiveArgument "compareByteArrays#" leftValue
  right <- expectByteArrayPrimitiveArgument "compareByteArrays#" rightValue
  checkedLeftOffset <- expectIntPrimitiveArgument "compareByteArrays#" leftOffset
  checkedRightOffset <- expectIntPrimitiveArgument "compareByteArrays#" rightOffset
  checkedLength <- expectIntPrimitiveArgument "compareByteArrays#" byteCount
  (leftStart, leftLength) <- checkedByteArrayRange "compareByteArrays#" left checkedLeftOffset checkedLength
  (rightStart, rightLength) <- checkedByteArrayRange "compareByteArrays#" right checkedRightOffset checkedLength
  leftBytes <- liftEvalIO (peekArray leftLength (castPtr (grinByteArrayContents left `plusPtr` leftStart)) :: IO [Word8])
  rightBytes <- liftEvalIO (peekArray rightLength (castPtr (grinByteArrayContents right `plusPtr` rightStart)) :: IO [Word8])
  pure [intRuntimeValue (compareOrdinal (compare leftBytes rightBytes))]
evalPrimitive "copyAddrToByteArray#" [source, value, offset, byteCount] = do
  byteArray <- expectByteArrayPrimitiveArgument "copyAddrToByteArray#" value
  checkedOffset <- expectIntPrimitiveArgument "copyAddrToByteArray#" offset
  checkedLength <- expectIntPrimitiveArgument "copyAddrToByteArray#" byteCount
  (destinationOffset, destinationLength) <- checkedByteArrayRange "copyAddrToByteArray#" byteArray checkedOffset checkedLength
  sourceBytes <- readAddressBytes "copyAddrToByteArray#" destinationLength source
  liftEvalIO (pokeArray (castPtr (grinByteArrayContents byteArray `plusPtr` destinationOffset)) (BS.unpack sourceBytes))
  pure []
evalPrimitive name arguments =
  throwInterpret (InterpretPrimitiveArity name (length arguments))

allocateByteArray :: Text -> Bool -> Int -> Integer -> EvalM GrinByteArray
allocateByteArray symbol pinned alignment requestedSize = do
  size <- checkedByteArraySize symbol requestedSize
  raw <- liftEvalIO (mallocBytes (max 1 size + alignment - 1))
  let contents = alignPtr raw alignment
  liftEvalIO (fillBytes contents 0 size)
  sizeReference <- liftEvalIO (newIORef size)
  pure
    GrinByteArray
      { grinByteArraySize = sizeReference,
        grinByteArrayContents = contents,
        grinByteArrayPinned = pinned,
        grinByteArrayAlignment = alignment
      }

resizeByteArray :: Text -> GrinByteArray -> Integer -> EvalM GrinByteArray
resizeByteArray symbol byteArray requestedSize = do
  resized <- allocateByteArray symbol (grinByteArrayPinned byteArray) (grinByteArrayAlignment byteArray) requestedSize
  oldSize <- liftEvalIO (readIORef (grinByteArraySize byteArray))
  newSize <- liftEvalIO (readIORef (grinByteArraySize resized))
  liftEvalIO (copyBytes (grinByteArrayContents resized) (grinByteArrayContents byteArray) (min oldSize newSize))
  pure resized

checkedByteArraySize :: Text -> Integer -> EvalM Int
checkedByteArraySize symbol size
  | size < 0 || size > toInteger (maxBound :: Int) =
      throwInterpret (InterpretInvalidByteArrayRange symbol 0 size 0)
  | otherwise = pure (fromInteger size)

checkedArraySize :: Text -> Integer -> EvalM Int
checkedArraySize name size
  | size < 0 || size > toInteger (maxBound :: Int) =
      throwInterpret (InterpretInvalidArrayIndex name size 0)
  | otherwise = pure (fromInteger size)

expectArrayPrimitiveArgument :: Text -> RuntimeValue -> EvalM GrinArray
expectArrayPrimitiveArgument name value =
  case value of
    RuntimeArray array -> pure array
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

readArrayElement :: Text -> GrinArray -> Integer -> EvalM RuntimeValue
readArrayElement name (GrinArray reference) index = do
  values <- liftEvalIO (readIORef reference)
  case listElement index values of
    Just value -> pure value
    Nothing -> throwInterpret (InterpretInvalidArrayIndex name index (length values))

writeArrayElement :: Text -> GrinArray -> Integer -> RuntimeValue -> EvalM ()
writeArrayElement name (GrinArray reference) index value = do
  values <- liftEvalIO (readIORef reference)
  case replaceListElement index value values of
    Just updated -> liftEvalIO (writeIORef reference updated)
    Nothing -> throwInterpret (InterpretInvalidArrayIndex name index (length values))

listElement :: Integer -> [a] -> Maybe a
listElement index _ | index < 0 = Nothing
listElement _ [] = Nothing
listElement 0 (value : _) = Just value
listElement index (_ : values) = listElement (index - 1) values

replaceListElement :: Integer -> a -> [a] -> Maybe [a]
replaceListElement index _ _ | index < 0 = Nothing
replaceListElement _ _ [] = Nothing
replaceListElement 0 value (_ : values) = Just (value : values)
replaceListElement index value (current : values) =
  (current :) <$> replaceListElement (index - 1) value values

checkedByteArrayAlignment :: Text -> Integer -> EvalM Int
checkedByteArrayAlignment symbol alignment
  | alignment <= 0 || alignment > toInteger (maxBound :: Int) || alignment .&. (alignment - 1) /= 0 =
      throwInterpret (InterpretInvalidByteArrayRange symbol 0 alignment 0)
  | otherwise = pure (fromInteger alignment)

expectByteArrayPrimitiveArgument :: Text -> RuntimeValue -> EvalM GrinByteArray
expectByteArrayPrimitiveArgument name value =
  case value of
    RuntimeByteArray byteArray -> pure byteArray
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

checkedByteArrayRange :: Text -> GrinByteArray -> Integer -> Integer -> EvalM (Int, Int)
checkedByteArrayRange symbol byteArray offset byteCount = do
  size <- liftEvalIO (readIORef (grinByteArraySize byteArray))
  if offset < 0 || byteCount < 0 || offset > toInteger size || byteCount > toInteger size - offset
    then throwInterpret (InterpretInvalidByteArrayRange symbol offset byteCount size)
    else pure (fromInteger offset, fromInteger byteCount)

-- | Reverse the low bytes of a word and clear the bytes above them.
evalByteSwap :: Text -> GrinRep -> Int -> RuntimeValue -> EvalM [RuntimeValue]
evalByteSwap symbol rep byteCount value = do
  word <- expectRuntimeRepPrimitiveArgument symbol rep value
  pure [RuntimeLit (GrinLitInt rep (swapBytes byteCount word))]

swapBytes :: Int -> Integer -> Integer
swapBytes byteCount value = go byteCount 0
  where
    go 0 accumulated = accumulated
    go remaining accumulated =
      go (remaining - 1) (shiftL accumulated 8 .|. (shiftR value ((byteCount - remaining) * 8) .&. 0xff))

-- | Read one element of a byte array at a scaled offset with a bounds check.
readByteArrayElement :: Text -> Int -> Int -> (Ptr () -> Int -> IO Integer) -> RuntimeValue -> RuntimeValue -> EvalM Integer
readByteArrayElement symbol stride elementSize readElement value indexValue = do
  byteArray <- expectByteArrayPrimitiveArgument symbol value
  index <- expectIntPrimitiveArgument symbol indexValue
  (byteOffset, _) <- checkedByteArrayRange symbol byteArray (index * toInteger stride) (toInteger elementSize)
  liftEvalIO (readElement (grinByteArrayContents byteArray) byteOffset)

checkedWordArrayIndex :: Text -> GrinByteArray -> Integer -> EvalM Int
checkedWordArrayIndex symbol byteArray index = do
  (offset, _) <- checkedByteArrayRange symbol byteArray (index * 8) 8
  pure offset

evalIntPrimitive :: Text -> (Integer -> Integer -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalIntPrimitive name operation left right = do
  leftInt <- expectIntPrimitiveArgument name left
  rightInt <- expectIntPrimitiveArgument name right
  pure [RuntimeLit (GrinLitInt IntRep (operation leftInt rightInt))]

evalIntCarryPrimitive :: Text -> (Integer -> Integer -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalIntCarryPrimitive name operation left right = do
  leftInt <- expectIntPrimitiveArgument name left
  rightInt <- expectIntPrimitiveArgument name right
  let exactResult = operation (normalizeInt leftInt) (normalizeInt rightInt)
      overflow = exactResult < intMin || exactResult > intMax
  pure [intRuntimeValue exactResult, intRuntimeValue (if overflow then 1 else 0)]

evalWordPrimitive :: Text -> (Integer -> Integer -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalWordPrimitive name operation left right = do
  leftWord <- expectWordPrimitiveArgument name left
  rightWord <- expectWordPrimitiveArgument name right
  pure [wordRuntimeValue (operation leftWord rightWord)]

evalWordCarryPrimitive :: Text -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalWordCarryPrimitive name left right = do
  leftWord <- expectWordPrimitiveArgument name left
  rightWord <- expectWordPrimitiveArgument name right
  let exactResult = leftWord + rightWord
  pure [wordRuntimeValue exactResult, intRuntimeValue (if exactResult >= wordModulus then 1 else 0)]

evalWordBorrowPrimitive :: Text -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalWordBorrowPrimitive name left right = do
  leftWord <- expectWordPrimitiveArgument name left
  rightWord <- expectWordPrimitiveArgument name right
  pure [wordRuntimeValue (leftWord - rightWord), intRuntimeValue (if leftWord < rightWord then 1 else 0)]

evalWordComparison :: Text -> (Integer -> Integer -> Bool) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalWordComparison name comparison left right = do
  leftWord <- expectWordPrimitiveArgument name left
  rightWord <- expectWordPrimitiveArgument name right
  pure [intRuntimeValue (if comparison leftWord rightWord then 1 else 0)]

evalWordShift :: Text -> (Integer -> Int -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalWordShift name operation value amount = do
  word <- expectWordPrimitiveArgument name value
  shiftAmount <- expectIntPrimitiveArgument name amount
  pure [wordRuntimeValue (operation word (fromInteger shiftAmount))]

-- | Keep the low bits of an int as a sized signed value.
evalIntNarrow :: Text -> GrinRep -> Int -> RuntimeValue -> EvalM [RuntimeValue]
evalIntNarrow name rep bits value = do
  int <- expectIntPrimitiveArgument name value
  let modulus = shiftL 1 bits
      low = int .&. (modulus - 1)
      signed = if low >= shiftL 1 (bits - 1) then low - modulus else low
  pure [RuntimeLit (GrinLitInt rep signed)]

-- | A float value is its IEEE bit pattern in a 'FloatRep' literal.
floatRuntimeValue :: Float -> RuntimeValue
floatRuntimeValue = RuntimeLit . GrinLitInt FloatRep . toInteger . castFloatToWord32

expectFloatPrimitiveArgument :: Text -> RuntimeValue -> EvalM Float
expectFloatPrimitiveArgument name value =
  castWord32ToFloat . fromInteger <$> expectRuntimeRepPrimitiveArgument name FloatRep value

evalFloatBinary :: Text -> (Float -> Float -> Float) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalFloatBinary name operation left right = do
  leftFloat <- expectFloatPrimitiveArgument name left
  rightFloat <- expectFloatPrimitiveArgument name right
  pure [floatRuntimeValue (operation leftFloat rightFloat)]

evalFloatUnary :: Text -> (Float -> Float) -> RuntimeValue -> EvalM [RuntimeValue]
evalFloatUnary name operation value = do
  float <- expectFloatPrimitiveArgument name value
  pure [floatRuntimeValue (operation float)]

evalFloatComparison :: Text -> (Float -> Float -> Bool) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalFloatComparison name operation left right = do
  leftFloat <- expectFloatPrimitiveArgument name left
  rightFloat <- expectFloatPrimitiveArgument name right
  pure [intRuntimeValue (if operation leftFloat rightFloat then 1 else 0)]

-- | A double value is its IEEE bit pattern in a 'DoubleRep' literal.
doubleRuntimeValue :: Double -> RuntimeValue
doubleRuntimeValue = RuntimeLit . GrinLitInt DoubleRep . toInteger . castDoubleToWord64

expectDoublePrimitiveArgument :: Text -> RuntimeValue -> EvalM Double
expectDoublePrimitiveArgument name value =
  castWord64ToDouble . fromInteger <$> expectRuntimeRepPrimitiveArgument name DoubleRep value

evalDoubleBinary :: Text -> (Double -> Double -> Double) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalDoubleBinary name operation left right = do
  leftDouble <- expectDoublePrimitiveArgument name left
  rightDouble <- expectDoublePrimitiveArgument name right
  pure [doubleRuntimeValue (operation leftDouble rightDouble)]

evalDoubleUnary :: Text -> (Double -> Double) -> RuntimeValue -> EvalM [RuntimeValue]
evalDoubleUnary name operation value = do
  double <- expectDoublePrimitiveArgument name value
  pure [doubleRuntimeValue (operation double)]

evalDoubleComparison :: Text -> (Double -> Double -> Bool) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalDoubleComparison name operation left right = do
  leftDouble <- expectDoublePrimitiveArgument name left
  rightDouble <- expectDoublePrimitiveArgument name right
  pure [intRuntimeValue (if operation leftDouble rightDouble then 1 else 0)]

evalWordCount :: Text -> (Word64 -> Int) -> RuntimeValue -> EvalM [RuntimeValue]
evalWordCount name operation value = do
  word <- expectWordPrimitiveArgument name value
  pure [wordRuntimeValue (toInteger (operation (fromInteger word)))]

wordRuntimeValue :: Integer -> RuntimeValue
wordRuntimeValue = RuntimeLit . GrinLitInt WordRep . normalizeWord

expectRuntimeRepPrimitiveArgument :: Text -> GrinRep -> RuntimeValue -> EvalM Integer
expectRuntimeRepPrimitiveArgument name expectedRep value =
  case value of
    RuntimeLit (GrinLitInt actualRep intValue)
      | actualRep == expectedRep -> pure intValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

intRuntimeValue :: Integer -> RuntimeValue
intRuntimeValue = RuntimeLit . GrinLitInt IntRep . normalizeInt

wordBits :: Int
wordBits = 64

wordModulus :: Integer
wordModulus = 1 `shiftL` wordBits

wordMask :: Integer
wordMask = wordModulus - 1

intMin :: Integer
intMin = negate (1 `shiftL` (wordBits - 1))

intMax :: Integer
intMax = (1 `shiftL` (wordBits - 1)) - 1

normalizeWord :: Integer -> Integer
normalizeWord value = value .&. wordMask

normalizeInt :: Integer -> Integer
normalizeInt value =
  let word = normalizeWord value
   in if word > intMax then word - wordModulus else word

expectIntPrimitiveArgument :: Text -> RuntimeValue -> EvalM Integer
expectIntPrimitiveArgument name value =
  case value of
    RuntimeLit (GrinLitInt _ intValue) -> pure intValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

expectWordPrimitiveArgument :: Text -> RuntimeValue -> EvalM Integer
expectWordPrimitiveArgument name value = normalizeWord <$> expectIntPrimitiveArgument name value

expectCharPrimitiveArgument :: Text -> RuntimeValue -> EvalM Char
expectCharPrimitiveArgument name value =
  case value of
    RuntimeLit (GrinLitChar _ charValue) -> pure charValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

expectMutVarPrimitiveArgument :: Text -> RuntimeValue -> EvalM GrinMutVar
expectMutVarPrimitiveArgument name value =
  case value of
    RuntimeMutVar mutVar -> pure mutVar
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

expectStableNamePrimitiveArgument :: Text -> RuntimeValue -> EvalM GrinStableName
expectStableNamePrimitiveArgument name value =
  case value of
    RuntimeStableName stableName -> pure stableName
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

compareInts :: Integer -> Integer -> Integer
compareInts left right =
  case compare left right of
    LT -> -1
    EQ -> 0
    GT -> 1

executeForeignCall :: GrinForeignCall -> [RuntimeValue] -> EvalM [RuntimeValue]
executeForeignCall foreignCall arguments
  | actualArity /= expectedArity = throwInterpret (InterpretForeignArity name expectedArity actualArity)
  | otherwise =
      callForeign foreignCall arguments
  where
    name = grinForeignCallName foreignCall
    signature = grinForeignCallSignature foreignCall
    actualArity = length arguments
    expectedArity = length (grinForeignOperandReps signature)

callForeign :: GrinForeignCall -> [RuntimeValue] -> EvalM [RuntimeValue]
callForeign foreignCall arguments
  -- An address import names static data; its value is the symbol address.
  | GrinForeignAddress <- grinForeignCallTarget foreignCall =
      (: []) . RuntimeAddress . castFunPtrToPtr <$> lookupForeignFunction foreignCall
  | symbol == "aihc_io_stdin",
    [] <- arguments =
      pure [RuntimeIOHandle (GrinIOHandle 0 stdin)]
  | symbol == "aihc_io_stdout",
    [] <- arguments =
      pure [RuntimeIOHandle (GrinIOHandle 1 stdout)]
  | symbol == "aihc_io_stderr",
    [] <- arguments =
      pure [RuntimeIOHandle (GrinIOHandle 2 stderr)]
  | symbol == "aihc_memory_write_byte",
    [bufferValue, offsetValue, byteValue] <- arguments = do
      buffer <- expectAddress symbol bufferValue
      offset <- expectForeignInt symbol offsetValue
      byte <- expectForeignInt symbol byteValue
      if offset < 0 || offset > toInteger (maxBound :: Int) || byte < 0 || byte > 255
        then pure [RuntimeLit (GrinLitInt IntRep (-23))]
        else do
          liftEvalIO (pokeArray (castPtr (buffer `plusPtr` fromInteger offset)) [fromInteger byte :: Word8])
          pure [RuntimeLit (GrinLitInt IntRep 0)]
  | symbol == "aihc_io_submit_open",
    [pathValue, lengthValue, modeValue] <- arguments = do
      pathLength <- expectForeignInt symbol lengthValue
      modeNumber <- expectForeignInt symbol modeValue
      if pathLength < 0 || pathLength > toInteger (maxBound :: Int)
        then (: []) <$> completedOpenRequest (Left 22)
        else do
          bytes <- readAddressBytes symbol (fromInteger pathLength) pathValue
          case TE.decodeUtf8' bytes of
            Left _ -> (: []) <$> completedOpenRequest (Left 84)
            Right path ->
              (: []) . RuntimeIORequest . GrinIORequest
                <$> liftEvalIO (newIORef (GrinIOSubmitted (GrinOpen path modeNumber)))
  | symbol == "aihc_io_open_result_error",
    [openResult] <- arguments =
      case openResult of
        RuntimeIOError errorNumber -> pure [RuntimeLit (GrinLitInt IntRep errorNumber)]
        RuntimeIOHandle {} -> pure [RuntimeLit (GrinLitInt IntRep 0)]
        _ -> throwInterpret (InterpretForeignTypeError symbol openResult)
  | symbol == "aihc_io_close",
    [handleValue] <- arguments = do
      GrinIOHandle _ handle <- expectIOHandle symbol handleValue
      result <- liftEvalIO (tryForeign (hClose handle))
      case result of
        Left _ -> pure [RuntimeLit (GrinLitInt IntRep (-6))]
        Right () -> pure [RuntimeLit (GrinLitInt IntRep 0)]
  | symbol == "aihc_io_raise_error",
    [errorValue] <- arguments = do
      errorNumber <- expectForeignInt symbol errorValue
      throwInterpret (InterpretRaisedException (T.pack (show errorNumber)))
  | symbol == "aihc_io_submit_read",
    [handleValue, bufferValue, offsetValue, lengthValue] <- arguments = do
      handle <- expectIOHandle symbol handleValue
      buffer <- expectAddress symbol bufferValue
      offset <- expectForeignInt symbol offsetValue
      byteCount <- expectForeignInt symbol lengthValue
      (checkedOffset, checkedLength) <- checkedAddressRange symbol offset byteCount
      (: []) . RuntimeIORequest . GrinIORequest <$> liftEvalIO (newIORef (GrinIOSubmitted (GrinRead handle buffer checkedOffset checkedLength)))
  | symbol == "aihc_io_submit_write",
    [handleValue, bufferValue, offsetValue, lengthValue] <- arguments = do
      handle <- expectIOHandle symbol handleValue
      buffer <- expectAddress symbol bufferValue
      offset <- expectForeignInt symbol offsetValue
      byteCount <- expectForeignInt symbol lengthValue
      (checkedOffset, checkedLength) <- checkedAddressRange symbol offset byteCount
      (: []) . RuntimeIORequest . GrinIORequest <$> liftEvalIO (newIORef (GrinIOSubmitted (GrinWrite handle buffer checkedOffset checkedLength)))
  | symbol == "aihc_io_take_result",
    [request] <- arguments =
      (: []) <$> takeIOResult symbol request
  | symbol == "aihc_io_take_open_result",
    [request] <- arguments =
      (: []) <$> takeOpenIOResult symbol request
  | otherwise = do
      marshalledArguments <-
        zipWithM
          (marshalForeignArgument (grinForeignCallSymbol foreignCall))
          (grinForeignArgumentTypes (grinForeignCallSignature foreignCall))
          arguments
      functionPointer <- lookupForeignFunction foreignCall
      let resultType = grinForeignResultType (grinForeignCallSignature foreignCall)
          integerResult :: (Integral result) => RetType result -> EvalM [RuntimeValue]
          integerResult returnType = do
            result <- liftEvalIO (callFFI functionPointer returnType marshalledArguments)
            pure [RuntimeLit (GrinLitInt (foreignTypeRuntimeRep resultType) (toInteger result))]
      case resultType of
        GrinForeignInt -> integerResult retInt64
        GrinForeignInt8 -> integerResult retInt8
        GrinForeignInt16 -> integerResult retInt16
        GrinForeignInt32 -> integerResult retInt32
        GrinForeignInt64 -> integerResult retInt64
        GrinForeignWord -> integerResult retWord64
        GrinForeignWord8 -> integerResult retWord8
        GrinForeignWord16 -> integerResult retWord16
        GrinForeignWord32 -> integerResult retWord32
        GrinForeignWord64 -> integerResult retWord64
        GrinForeignFloat ->
          (: []) . RuntimeLit . GrinLitInt FloatRep . toInteger . castFloatToWord32 . realToFrac
            <$> liftEvalIO (callFFI functionPointer retCFloat marshalledArguments)
        GrinForeignDouble ->
          (: []) . RuntimeLit . GrinLitInt DoubleRep . toInteger . castDoubleToWord64 . realToFrac
            <$> liftEvalIO (callFFI functionPointer retCDouble marshalledArguments)
        GrinForeignAddr ->
          (: []) . RuntimeAddress <$> liftEvalIO (callFFI functionPointer (retPtr retVoid) marshalledArguments)
        GrinForeignVoid -> do
          liftEvalIO (callFFI functionPointer retVoid marshalledArguments)
          pure []
  where
    symbol = grinForeignCallSymbol foreignCall

hostIOMode :: Integer -> IOMode
hostIOMode mode =
  case mode of
    0 -> ReadMode
    1 -> WriteMode
    2 -> AppendMode
    _ -> ReadWriteMode

expectIOHandle :: Text -> RuntimeValue -> EvalM GrinIOHandle
expectIOHandle symbol value =
  case value of
    RuntimeIOHandle handle -> pure handle
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

expectAddress :: Text -> RuntimeValue -> EvalM (Ptr ())
expectAddress symbol value =
  case value of
    RuntimeAddress address -> pure address
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

checkedAddressRange :: Text -> Integer -> Integer -> EvalM (Int, Int)
checkedAddressRange symbol offset byteCount =
  if offset < 0 || byteCount < 0 || offset > intLimit || byteCount > intLimit - offset
    then throwInterpret (InterpretInvalidByteArrayRange symbol offset byteCount 0)
    else pure (fromInteger offset, fromInteger byteCount)
  where
    intLimit = toInteger (maxBound :: Int)

indexAddressPrimitive :: Text -> Int -> (Ptr () -> Int -> IO Integer) -> RuntimeValue -> RuntimeValue -> EvalM Integer
indexAddressPrimitive symbol elementSize = readAddressPrimitive symbol elementSize elementSize

-- | Read one element at a scaled index. Literal addresses stay bounds checked.
-- Runtime addresses trust the program in the same way as native code.
readAddressPrimitive :: Text -> Int -> Int -> (Ptr () -> Int -> IO Integer) -> RuntimeValue -> RuntimeValue -> EvalM Integer
readAddressPrimitive symbol stride elementSize readElement address indexValue = do
  index <- expectIntPrimitiveArgument symbol indexValue
  (byteOffset, _) <- checkedAddressRange symbol (index * toInteger stride) (toInteger elementSize)
  case address of
    RuntimeLit (GrinLitAddr bytes)
      | byteOffset + elementSize <= BS.length bytes ->
          liftEvalIO (BS.useAsCString bytes (\pointer -> readElement (castPtr pointer) byteOffset))
      | otherwise ->
          throwInterpret (InterpretInvalidByteArrayRange symbol (toInteger byteOffset) (toInteger elementSize) (BS.length bytes))
    RuntimeAddress pointer -> liftEvalIO (readElement pointer byteOffset)
    other -> throwInterpret (InterpretPrimitiveTypeError symbol other)

writeAddressPrimitive :: Text -> Int -> GrinRep -> (Ptr () -> Int -> Integer -> IO ()) -> RuntimeValue -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
writeAddressPrimitive symbol stride valueRep writeElement address indexValue value = do
  index <- expectIntPrimitiveArgument symbol indexValue
  element <- expectRuntimeRepPrimitiveArgument symbol valueRep value
  pointer <- expectAddress symbol address
  liftEvalIO (writeElement pointer (fromInteger (index * toInteger stride)) element)
  pure []

addressPlus :: Text -> RuntimeValue -> Integer -> EvalM RuntimeValue
addressPlus symbol address byteOffset =
  case address of
    RuntimeAddress pointer -> pure (RuntimeAddress (pointer `plusPtr` fromInteger byteOffset))
    RuntimeLit (GrinLitAddr bytes)
      | byteOffset >= 0 && byteOffset <= toInteger (BS.length bytes) ->
          pure (RuntimeLit (GrinLitAddr (BS.drop (fromInteger byteOffset) bytes)))
      | otherwise ->
          throwInterpret (InterpretInvalidByteArrayRange symbol byteOffset 0 (BS.length bytes))
    other -> throwInterpret (InterpretForeignTypeError symbol other)

evalAddressComparison :: Text -> (Integer -> Integer -> Bool) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalAddressComparison symbol comparison left right =
  case (left, right) of
    (RuntimeLit (GrinLitAddr leftBytes), RuntimeLit (GrinLitAddr rightBytes)) ->
      -- Literal addresses have no runtime location. Their bytes give a
      -- consistent order for the comparison.
      pure [intRuntimeValue (if comparison 0 (compareOrdinal (compare rightBytes leftBytes)) then 1 else 0)]
    _ -> do
      leftPointer <- expectAddress symbol left
      rightPointer <- expectAddress symbol right
      pure [intRuntimeValue (if comparison (addressOrdinal leftPointer) (addressOrdinal rightPointer) then 1 else 0)]

addressOrdinal :: Ptr () -> Integer
addressOrdinal pointer =
  case ptrToIntPtr pointer of
    IntPtr value -> toInteger value

compareOrdinal :: Ordering -> Integer
compareOrdinal ordering =
  case ordering of
    LT -> -1
    EQ -> 0
    GT -> 1

evalWord64Comparison :: Text -> (Integer -> Integer -> Bool) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalWord64Comparison name comparison left right = do
  leftWord <- expectRuntimeRepPrimitiveArgument name Word64Rep left
  rightWord <- expectRuntimeRepPrimitiveArgument name Word64Rep right
  pure [intRuntimeValue (if comparison leftWord rightWord then 1 else 0)]

evalWordNarrow :: Text -> GrinRep -> Integer -> RuntimeValue -> EvalM [RuntimeValue]
evalWordNarrow name resultRep mask value = do
  word <- expectWordPrimitiveArgument name value
  pure [RuntimeLit (GrinLitInt resultRep (word .&. mask))]

readAddressWord8 :: Ptr () -> Int -> IO Integer
readAddressWord8 pointer offset = toInteger <$> (peekByteOff pointer offset :: IO Word8)

readAddressWord16 :: Ptr () -> Int -> IO Integer
readAddressWord16 pointer offset = toInteger <$> (peekByteOff pointer offset :: IO Word16)

writeAddressWord8 :: Ptr () -> Int -> Integer -> IO ()
writeAddressWord8 pointer offset value = pokeByteOff pointer offset (fromInteger value :: Word8)

writeAddressWord16 :: Ptr () -> Int -> Integer -> IO ()
writeAddressWord16 pointer offset value = pokeByteOff pointer offset (fromInteger value :: Word16)

writeAddressWord32 :: Ptr () -> Int -> Integer -> IO ()
writeAddressWord32 pointer offset value = pokeByteOff pointer offset (fromInteger value :: Word32)

writeAddressWord64 :: Ptr () -> Int -> Integer -> IO ()
writeAddressWord64 pointer offset value = pokeByteOff pointer offset (fromInteger value :: Word64)

readAddressWord32 :: Ptr () -> Int -> IO Integer
readAddressWord32 pointer offset = toInteger <$> (peekByteOff pointer offset :: IO Word32)

readAddressWord64 :: Ptr () -> Int -> IO Integer
readAddressWord64 pointer offset = toInteger <$> (peekByteOff pointer offset :: IO Word64)

readAddressBytes :: Text -> Int -> RuntimeValue -> EvalM BS.ByteString
readAddressBytes symbol byteCount value =
  case value of
    RuntimeLit (GrinLitAddr bytes) ->
      liftEvalIO (withArray0 0 (BS.unpack bytes) (fmap BS.pack . peekArray byteCount))
    RuntimeAddress pointer -> liftEvalIO (BS.pack <$> peekArray byteCount (castPtr pointer))
    other -> throwInterpret (InterpretForeignTypeError symbol other)

completeIORequest :: GrinIORequest -> EvalM ()
completeIORequest (GrinIORequest reference) = do
  state <- liftEvalIO (readIORef reference)
  case state of
    GrinIOSubmitted operation -> do
      result <- performIOOperation operation
      liftEvalIO (writeIORef reference (GrinIOCompleted result))
    GrinIOCompleted {} -> pure ()
    GrinIOConsumed -> throwInterpret (InterpretPrimitiveTypeError "awaitIO#" (RuntimeIORequest (GrinIORequest reference)))

performIOOperation :: GrinIOOperation -> EvalM GrinIOResult
performIOOperation operation =
  case operation of
    GrinRead (GrinIOHandle _ handle) buffer offset byteCount -> do
      result <- liftEvalIO (tryForeign (BS.hGet handle byteCount))
      case result of
        Left _ -> pure (GrinIOInt (-6))
        Right input -> do
          liftEvalIO (pokeArray (castPtr (buffer `plusPtr` offset)) (BS.unpack input))
          pure (GrinIOInt (toInteger (BS.length input)))
    GrinWrite (GrinIOHandle _ handle) buffer offset byteCount -> do
      bytes <- liftEvalIO (BS.pack <$> peekArray byteCount (castPtr (buffer `plusPtr` offset)))
      result <- liftEvalIO (tryForeign (BS.hPut handle bytes >> hFlush handle))
      case result of
        Left _ -> pure (GrinIOInt (-6))
        Right () -> pure (GrinIOInt (toInteger byteCount))
    GrinOpen path modeNumber -> do
      result <- liftEvalIO (tryForeign (openBinaryFile (T.unpack path) (hostIOMode modeNumber)))
      pure
        ( GrinIOOpenResult
            ( case result of
                Left _ -> Left 5
                Right handle -> Right (GrinIOHandle 3 handle)
            )
        )

takeIOResult :: Text -> RuntimeValue -> EvalM RuntimeValue
takeIOResult symbol value =
  case value of
    RuntimeIORequest (GrinIORequest reference) -> do
      state <- liftEvalIO (readIORef reference)
      case state of
        GrinIOCompleted (GrinIOInt result) -> do
          liftEvalIO (writeIORef reference GrinIOConsumed)
          pure (RuntimeLit (GrinLitInt IntRep result))
        _ -> throwInterpret (InterpretForeignTypeError symbol value)
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

takeOpenIOResult :: Text -> RuntimeValue -> EvalM RuntimeValue
takeOpenIOResult symbol value =
  case value of
    RuntimeIORequest (GrinIORequest reference) -> do
      state <- liftEvalIO (readIORef reference)
      case state of
        GrinIOCompleted (GrinIOOpenResult result) -> do
          liftEvalIO (writeIORef reference GrinIOConsumed)
          pure (either RuntimeIOError RuntimeIOHandle result)
        _ -> throwInterpret (InterpretForeignTypeError symbol value)
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

completedOpenRequest :: Either Integer GrinIOHandle -> EvalM RuntimeValue
completedOpenRequest result =
  RuntimeIORequest . GrinIORequest
    <$> liftEvalIO (newIORef (GrinIOCompleted (GrinIOOpenResult result)))

marshalForeignArgument :: Text -> GrinForeignType -> RuntimeValue -> EvalM Arg
marshalForeignArgument symbol foreignType argument =
  case foreignType of
    GrinForeignInt -> integerArgument (argInt64 . fromInteger)
    GrinForeignInt8 -> integerArgument (argInt8 . fromInteger)
    GrinForeignInt16 -> integerArgument (argInt16 . fromInteger)
    GrinForeignInt32 -> integerArgument (argInt32 . fromInteger)
    GrinForeignInt64 -> integerArgument (argInt64 . fromInteger)
    GrinForeignWord -> integerArgument (argWord64 . fromInteger)
    GrinForeignWord8 -> integerArgument (argWord8 . fromInteger)
    GrinForeignWord16 -> integerArgument (argWord16 . fromInteger)
    GrinForeignWord32 -> integerArgument (argWord32 . fromInteger)
    GrinForeignWord64 -> integerArgument (argWord64 . fromInteger)
    GrinForeignFloat -> integerArgument (argCFloat . realToFrac . castWord32ToFloat . fromInteger)
    GrinForeignDouble -> integerArgument (argCDouble . realToFrac . castWord64ToDouble . fromInteger)
    GrinForeignAddr ->
      case argument of
        RuntimeLit (GrinLitAddr value) -> do
          pointer <- liftEvalIO (newArray0 0 (BS.unpack value))
          pure (argPtr pointer)
        RuntimeAddress pointer -> pure (argPtr pointer)
        other -> throwInterpret (InterpretForeignTypeError symbol other)
    GrinForeignVoid -> throwInterpret (InterpretForeignTypeError symbol argument)
  where
    integerArgument make = make <$> expectForeignLiteral symbol (foreignTypeRuntimeRep foreignType) argument

-- | An integer literal of the runtime representation that the C ABI expects.
expectForeignLiteral :: Text -> GrinRep -> RuntimeValue -> EvalM Integer
expectForeignLiteral symbol expectedRep value =
  case value of
    RuntimeLit (GrinLitInt actualRep intValue) | actualRep == expectedRep -> pure intValue
    other -> throwInterpret (InterpretForeignTypeError symbol other)

lookupForeignFunction :: GrinForeignCall -> EvalM (FunPtr ())
lookupForeignFunction foreignCall = do
  lookupResult <- liftEvalIO (tryForeign (dlsym Default (T.unpack (grinForeignCallSymbol foreignCall))))
  case lookupResult of
    Left err ->
      throwInterpret
        ( InterpretForeignLookupError
            (grinForeignCallSymbol foreignCall)
            (T.pack (displayException err))
        )
    Right pointer -> pure pointer

tryForeign :: IO value -> IO (Either SomeException value)
tryForeign = try

expectForeignInt :: Text -> RuntimeValue -> EvalM Integer
expectForeignInt symbol value =
  case value of
    RuntimeLit (GrinLitInt IntRep intValue) -> pure intValue
    other -> throwInterpret (InterpretForeignTypeError symbol other)

runIOValue :: RuntimeValue -> EvalM RuntimeValue
runIOValue action = do
  results <- applyValue action []
  case results of
    [ioResult] -> pure ioResult
    _ -> throwInterpret (InterpretResultArity 1 (length results))

inspectCaseValue :: RuntimeValue -> EvalM RuntimeValue
inspectCaseValue value =
  case value of
    RuntimeLocation location -> do
      cell <- readCell location
      case cell of
        HeapValue node@RuntimeNode {} -> pure node
        _ -> throwInterpret (InterpretNoMatchingAlternative value)
    _ -> pure value

matchAlt :: RuntimeValue -> RuntimeValue -> GrinAlt -> Maybe Env
matchAlt original inspected alt =
  case (grinAltCon alt, inspected) of
    (GrinDefaultAlt, _) ->
      Just (Map.fromList [(var, original) | var <- grinAltBinders alt])
    (GrinLitAlt expected, RuntimeLit actual)
      | expected == actual -> Just Map.empty
    (GrinDataAlt expected, RuntimeNode (GrinConstructor actual 0) fields)
      | expected == actual,
        length fields == length (grinAltBinders alt) ->
          Just (Map.fromList (zip (grinAltBinders alt) fields))
    _ -> Nothing

expectSingle :: [RuntimeValue] -> EvalM RuntimeValue
expectSingle values =
  case values of
    [value] -> pure value
    _ -> throwInterpret (InterpretResultArity 1 (length values))

renderRawValueM :: RuntimeValue -> EvalM Text
renderRawValueM value = do
  exposed <- exposeWhnfValue value
  case exposed of
    RuntimeLit literal -> pure (renderLiteral literal)
    RuntimeAddress address -> pure (T.pack (show address))
    RuntimeArray {} -> pure "<array>"
    RuntimeIOHandle {} -> pure "<io-handle>"
    RuntimeByteArray {} -> pure "<byte-array>"
    RuntimeIOError {} -> pure "<io-error>"
    RuntimeIORequest {} -> pure "<io-request>"
    RuntimeMVar {} -> pure "<mvar>"
    RuntimeNode (GrinConstructor name 0) [char]
      | constructorDisplayName name == "C#" -> renderBoxedChar char
    RuntimeNode (GrinConstructor name 0) [] -> pure (constructorDisplayName name)
    RuntimeNode (GrinConstructor name 0) arguments
      | isTupleConstructor (constructorDisplayName name) (length arguments) -> do
          renderedArguments <- mapM renderRawArgument arguments
          pure ("(" <> T.intercalate "," renderedArguments <> ")")
    RuntimeNode (GrinConstructor name 0) arguments -> do
      renderedArguments <- mapM renderRawArgument arguments
      pure (T.unwords (constructorDisplayName name : renderedArguments))
    RuntimeNode GrinConstructor {} _ -> pure "<function>"
    RuntimeNode GrinClosure {} _ -> pure "<function>"
    RuntimeNode GrinThunk {} _ -> pure "<thunk>"
    RuntimeLocation location -> throwInterpret (InterpretInvalidLocation location)
    RuntimeMutVar {} -> pure "<mutvar>"
    RuntimeStableName {} -> pure "<stable-name>"
    RuntimeStateToken -> pure "<state>"

renderRawArgument :: RuntimeValue -> EvalM Text
renderRawArgument value = do
  exposed <- exposeWhnfValue value
  rendered <- renderRawValueM exposed
  pure $
    case exposed of
      RuntimeNode (GrinConstructor name 0) arguments
        | isTupleConstructor (constructorDisplayName name) (length arguments) -> rendered
      RuntimeNode (GrinConstructor name 0) [_]
        | constructorDisplayName name == "C#" -> rendered
      RuntimeNode (GrinConstructor _ 0) (_ : _) -> "(" <> rendered <> ")"
      _ -> rendered

exposeWhnfValue :: RuntimeValue -> EvalM RuntimeValue
exposeWhnfValue value = do
  forced <- forceValue value
  case forced of
    RuntimeLocation _ -> fetchValue forced
    _ -> pure forced

renderLiteral :: GrinLiteral -> Text
renderLiteral literal =
  case literal of
    GrinLitInt _ value -> T.pack (show value)
    GrinLitChar _ value -> T.pack (show value) <> "#"
    GrinLitString value -> T.pack (show (T.unpack value))
    GrinLitAddr value -> T.pack (show (map (Char.chr . fromIntegral) (BS.unpack value))) <> "#"

renderBoxedChar :: RuntimeValue -> EvalM Text
renderBoxedChar value = do
  forced <- forceValue value
  case forced of
    RuntimeLit (GrinLitChar _ charValue) -> pure (T.pack (show charValue))
    other -> throwInterpret (InterpretPrimitiveTypeError "C#" other)

isTupleConstructor :: Text -> Int -> Bool
isTupleConstructor name arity =
  arity >= 2
    && ( name == "(" <> T.replicate (arity - 1) "," <> ")"
           || name == "(#" <> T.replicate (arity - 1) "," <> "#)"
       )

-- | The name to show for one constructor. A tag names its package and its
-- module, and neither belongs in a printed value.
constructorDisplayName :: Text -> Text
constructorDisplayName name = maybe name snd (grinNameScope name)

throwInterpret :: InterpretError -> EvalM value
throwInterpret = throwE . EvalInterpret

liftEvalIO :: IO value -> EvalM value
liftEvalIO = lift . lift

getsMachine :: (Machine -> value) -> EvalM value
getsMachine = lift . gets

modifyMachine :: (Machine -> Machine) -> EvalM ()
modifyMachine = lift . modify'

buildHeapSnapshot :: IntMap HeapCell -> [RuntimeValue] -> HeapSnapshot
buildHeapSnapshot source values =
  let indirectionTargets =
        [ target
        | HeapValue (RuntimeLocation target) <- IntMap.elems source
        ]
      initial =
        SnapshotBuild
          { snapshotBuildSource = source,
            snapshotBuildValueSources =
              [ (value, location)
              | (location, HeapValue value@RuntimeNode {}) <- IntMap.toAscList source,
                location `elem` indirectionTargets
              ],
            snapshotBuildLocations = IntMap.empty,
            snapshotBuildSources = IntMap.empty,
            snapshotBuildNextLocation = 0,
            snapshotBuildCells = IntMap.empty
          }
      (returnValues, afterRoots) = runState (mapM snapshotRuntimeValue values) initial
      final = execState (snapshotPendingCells 0) afterRoots
   in HeapSnapshot
        { snapshotReturnValues = returnValues,
          snapshotHeap = snapshotBuildCells final
        }

snapshotPendingCells :: Int -> State SnapshotBuild ()
snapshotPendingCells location = do
  state <- get
  if location >= snapshotBuildNextLocation state
    then pure ()
    else do
      case IntMap.lookup location (snapshotBuildSources state) >>= (`IntMap.lookup` snapshotBuildSource state) of
        Nothing -> pure ()
        Just cell -> do
          snapshotCell <- snapshotHeapCell cell
          modify' $ \current -> current {snapshotBuildCells = IntMap.insert location snapshotCell (snapshotBuildCells current)}
      snapshotPendingCells (location + 1)

snapshotHeapCell :: HeapCell -> State SnapshotBuild SnapshotCell
snapshotHeapCell cell =
  case cell of
    HeapSuspended functionName fields -> SnapshotSuspended functionName <$> mapM snapshotRuntimeValue fields
    HeapValue (RuntimeLocation sourceLocation) -> SnapshotIndirection <$> snapshotLocation sourceLocation
    HeapValue value -> SnapshotValue <$> snapshotStoredValue value
    HeapRaised exception -> SnapshotRaised <$> snapshotRuntimeValue exception
    HeapBlackhole -> pure SnapshotBlackhole
    HeapThread -> pure SnapshotThreadId

snapshotRuntimeValue :: RuntimeValue -> State SnapshotBuild SnapshotValue
snapshotRuntimeValue value =
  case value of
    RuntimeLit literal -> pure (SnapshotLiteral literal)
    RuntimeAddress {} -> pure SnapshotAddress
    RuntimeArray {} -> pure SnapshotAddress
    RuntimeIOHandle {} -> pure SnapshotAddress
    RuntimeByteArray {} -> pure SnapshotAddress
    RuntimeIOError {} -> pure SnapshotAddress
    RuntimeIORequest {} -> pure SnapshotAddress
    RuntimeMVar {} -> pure SnapshotMutVar
    RuntimeNode {} -> do
      valueSources <- gets snapshotBuildValueSources
      case lookup value valueSources of
        Just sourceLocation -> SnapshotLocation <$> snapshotLocation sourceLocation
        Nothing -> snapshotStoredValue value
    RuntimeLocation sourceLocation -> SnapshotLocation <$> snapshotLocation sourceLocation
    RuntimeMutVar {} -> pure SnapshotMutVar
    RuntimeStableName {} -> pure SnapshotAddress
    RuntimeStateToken -> pure SnapshotStateToken

-- Heap-cell payloads define locations and therefore render their node inline.
-- The same node encountered elsewhere is rendered as a pointer back to this
-- owning value cell.
snapshotStoredValue :: RuntimeValue -> State SnapshotBuild SnapshotValue
snapshotStoredValue value =
  case value of
    RuntimeNode tag fields -> SnapshotNode tag <$> mapM snapshotRuntimeValue fields
    _ -> snapshotRuntimeValue value

snapshotLocation :: Int -> State SnapshotBuild Int
snapshotLocation sourceLocation = do
  state <- get
  case IntMap.lookup sourceLocation (snapshotBuildLocations state) of
    Just location -> pure location
    Nothing -> do
      let location = snapshotBuildNextLocation state
      modify' $ \current ->
        current
          { snapshotBuildLocations = IntMap.insert sourceLocation location (snapshotBuildLocations current),
            snapshotBuildSources = IntMap.insert location sourceLocation (snapshotBuildSources current),
            snapshotBuildNextLocation = location + 1
          }
      pure location
