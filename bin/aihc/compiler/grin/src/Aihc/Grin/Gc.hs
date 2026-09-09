{-# LANGUAGE OverloadedStrings #-}

-- | Make post-CPS allocation safepoints and relocated roots explicit.
module Aihc.Grin.Gc
  ( GcGrinProgram,
    entryGcProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
    lowerGc,
  )
where

import Aihc.Grin.Analysis (freeExprVars, maximumProgramVarUnique)
import Aihc.Grin.Cps (ContinuationFrameKind, CpsGrinError, CpsGrinProgram (..), toCpsGrin)
import Aihc.Grin.Heap (normalizeHeapReservations)
import Aihc.Grin.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | A CPS-GRIN program whose managed allocations have explicit safepoints.
-- Keeping this phase distinct prevents native backends from accidentally
-- consuming CPS-GRIN before roots have been made relocatable.
data GcGrinProgram = GcGrinProgram
  { gcGrinProgram :: !GrinProgram,
    gcContinuationFunctions :: !(Set FunctionName),
    gcContinuationFrames :: !(Map FunctionName ContinuationFrameKind),
    gcFunctionContinuations :: !(Map FunctionName GrinVar),
    gcUpdateFunction :: !FunctionName
  }
  deriving (Eq, Show, Read)

-- | Make the fixed GC-GRIN unit for the executable entry archive.
entryGcProgram :: Either CpsGrinError GcGrinProgram
entryGcProgram =
  lowerGc
    <$> toCpsGrin
      GrinProgram
        { grinConstructors = [],
          grinPrimitives = [],
          grinForeignCalls = [],
          grinGlobals = [],
          grinFunctions = []
        }

-- | Insert and normalize reservations. Then, give each reservation its live
-- roots and fresh SSA names for roots that collection can relocate.
lowerGc :: CpsGrinProgram -> GcGrinProgram
lowerGc cps =
  GcGrinProgram
    { gcGrinProgram =
        normalizedProgram
          { grinFunctions = evalState (mapM relocateFunction (grinFunctions normalizedProgram)) nextUnique
          },
      gcContinuationFunctions = cpsContinuationFunctions cps,
      gcContinuationFrames = cpsContinuationFrames cps,
      gcFunctionContinuations = cpsFunctionContinuations cps,
      gcUpdateFunction = cpsUpdateFunction cps
    }
  where
    program = cpsGrinProgram cps
    normalizedProgram = normalizeHeapReservations (insertHeapReservations program)
    nextUnique = 1 + maximumProgramVarUnique normalizedProgram

insertHeapReservations :: GrinProgram -> GrinProgram
insertHeapReservations program =
  program {grinFunctions = map insertFunctionReservations (grinFunctions program)}

insertFunctionReservations :: GrinFunction -> GrinFunction
insertFunctionReservations function =
  function {grinFunctionBody = insertExprReservations (grinFunctionBody function)}

insertExprReservations :: GrinExpr -> GrinExpr
insertExprReservations expression =
  case expression of
    GrinBind resultVars (GrinStore node) body ->
      GrinBind
        []
        (GrinEnsureHeap (staticHeapWords (nodeWords node)) [])
        (GrinBind resultVars (GrinStoreUnchecked node) (insertExprReservations body))
    GrinBind resultVars valueExpression body ->
      GrinBind resultVars (insertExprReservations valueExpression) (insertExprReservations body)
    GrinStore node ->
      GrinBind
        []
        (GrinEnsureHeap (staticHeapWords (nodeWords node)) [])
        (GrinStoreUnchecked node)
    GrinStoreRec bindings body ->
      GrinBind
        []
        (GrinEnsureHeap (staticHeapWords (sum (map (nodeWords . snd) bindings))) [])
        (GrinStoreRecUnchecked bindings (insertExprReservations body))
    GrinStoreRecUnchecked bindings body ->
      GrinStoreRecUnchecked bindings (insertExprReservations body)
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder (map insertAlternativeReservations alternatives)
    _ -> expression

insertAlternativeReservations :: GrinAlt -> GrinAlt
insertAlternativeReservations alternative =
  alternative {grinAltRhs = insertExprReservations (grinAltRhs alternative)}

relocateFunction :: GrinFunction -> State Int GrinFunction
relocateFunction function = do
  body <- relocateExpr (Set.fromList (grinFunctionParameters function)) (grinFunctionBody function)
  pure function {grinFunctionBody = body}

relocateExpr :: Set GrinVar -> GrinExpr -> State Int GrinExpr
relocateExpr bound expression =
  case expression of
    GrinBind [] (GrinEnsureHeap requiredWords []) body ->
      relocateReservation bound requiredWords body
    GrinBind resultVars valueExpression body -> do
      valueExpression' <- relocateExpr bound valueExpression
      body' <- relocateExpr (bound <> Set.fromList resultVars) body
      pure (GrinBind resultVars valueExpression' body')
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM (relocateAlternative (Set.insert binder bound)) alternatives
    GrinConstant {} -> pure expression
    GrinEnsureHeap {} -> pure expression
    GrinStore {} -> pure expression
    GrinStoreUnchecked {} -> pure expression
    GrinStoreRec {} -> pure expression
    GrinStoreRecUnchecked bindings body -> do
      let recursiveVars = Set.fromList (map fst bindings)
      body' <- relocateExpr (bound <> recursiveVars) body
      pure (GrinStoreRecUnchecked bindings body')
    GrinUpdate {} -> pure expression
    GrinUpdateBlackhole {} -> pure expression
    GrinEval {} -> pure expression
    GrinCpsEval {} -> pure expression
    GrinCall {} -> pure expression
    GrinPrimitiveCall {} -> pure expression
    GrinCpsPrimitiveCall {} -> pure expression
    GrinApply {} -> pure expression
    GrinCpsApply {} -> pure expression
    GrinContinue {} -> pure expression
    GrinCpsRaise {} -> pure expression
    GrinHalt {} -> pure expression
    GrinExit {} -> pure expression
    GrinThrow {} -> pure expression
    GrinCatch {} -> pure expression
    GrinForeignCallExpr {} -> pure expression

-- Reservations inserted before CPS carry only their dynamic size. Once
-- control flow is explicit, populate the reservation with every live pointer
-- root and rewrite the following expression to use the relocated SSA names.
relocateReservation :: Set GrinVar -> GrinValue -> GrinExpr -> State Int GrinExpr
relocateReservation bound requiredWords body = do
  let roots = livePointerRoots bound (freeExprVars body)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
      bodyWithRelocatedRoots = substituteExpr substitutions body
  body' <- relocateExpr (bound <> Set.fromList relocated) bodyWithRelocatedRoots
  pure
    ( GrinBind
        relocated
        (GrinEnsureHeap requiredWords (map GrinVarValue roots))
        body'
    )

relocateAlternative :: Set GrinVar -> GrinAlt -> State Int GrinAlt
relocateAlternative bound alternative = do
  rhs <- relocateExpr (bound <> Set.fromList (grinAltBinders alternative)) (grinAltRhs alternative)
  pure alternative {grinAltRhs = rhs}

livePointerRoots :: Set GrinVar -> Set GrinVar -> [GrinVar]
livePointerRoots bound uses =
  Set.toAscList (Set.filter (isPointerRuntimeRep . grinVarRuntimeRep) (bound `Set.intersection` uses))

freshRelocated :: GrinVar -> State Int GrinVar
freshRelocated var = do
  unique <- get
  put (unique + 1)
  pure var {grinVarName = grinVarName var <> "$gc", grinVarUnique = unique}

-- One info-table pointer plus the statically known payload. A
-- zero-field thunk reserves one payload word so it can become an indirection
-- in place.
nodeWords :: GrinNode -> Int
nodeWords node =
  1
    + case grinNodeTag node of
      GrinThunk {} -> max 1 fieldCount
      -- A constructor that still wants arguments spends one payload word on
      -- the count of the fields it holds, because every stage of one
      -- constructor shares a single info table.
      GrinConstructor _ remaining | remaining > 0 -> 1 + fieldCount
      _ -> fieldCount
  where
    fieldCount = length (grinNodeFields node)

staticHeapWords :: Int -> GrinValue
staticHeapWords = GrinLitValue . GrinLitInt WordRep . toInteger

substituteExpr :: Map GrinVar GrinVar -> GrinExpr -> GrinExpr
substituteExpr substitutions expression =
  case expression of
    GrinConstant values -> GrinConstant (map (substituteValue substitutions) values)
    GrinBind vars valueExpression body ->
      GrinBind vars (substituteExpr substitutions valueExpression) (substituteExpr (without vars substitutions) body)
    GrinStore node -> GrinStore (substituteNode substitutions node)
    GrinEnsureHeap requiredWords roots -> GrinEnsureHeap (substituteValue substitutions requiredWords) (map (substituteValue substitutions) roots)
    GrinStoreUnchecked node -> GrinStoreUnchecked (substituteNode substitutions node)
    GrinStoreRec bindings body -> substituteStoreRec GrinStoreRec substitutions bindings body
    GrinStoreRecUnchecked bindings body -> substituteStoreRec GrinStoreRecUnchecked substitutions bindings body
    GrinUpdate pointer value -> GrinUpdate (substituteValue substitutions pointer) (substituteValue substitutions value)
    GrinUpdateBlackhole pointer value -> GrinUpdateBlackhole (substituteValue substitutions pointer) (substituteValue substitutions value)
    GrinEval runtimeRep value -> GrinEval runtimeRep (substituteValue substitutions value)
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      GrinCpsEval runtimeRep (substituteValue substitutions value) (substituteValue substitutions continuation) (substituteValue substitutions updateContinuation)
    GrinCall runtimeRep name arguments -> GrinCall runtimeRep name (map (substituteValue substitutions) arguments)
    GrinPrimitiveCall runtimeRep name arguments -> GrinPrimitiveCall runtimeRep name (map (substituteValue substitutions) arguments)
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      GrinCpsPrimitiveCall runtimeRep name (map (substituteValue substitutions) arguments) (substituteValue substitutions continuation)
    GrinApply runtimeRep function arguments -> GrinApply runtimeRep (substituteValue substitutions function) (map (substituteValue substitutions) arguments)
    GrinCpsApply runtimeRep function arguments continuation ->
      GrinCpsApply runtimeRep (substituteValue substitutions function) (map (substituteValue substitutions) arguments) (substituteValue substitutions continuation)
    GrinContinue continuation values -> GrinContinue (substituteValue substitutions continuation) (map (substituteValue substitutions) values)
    GrinCpsRaise exception continuation -> GrinCpsRaise (substituteValue substitutions exception) (substituteValue substitutions continuation)
    GrinHalt values -> GrinHalt (map (substituteValue substitutions) values)
    GrinExit status -> GrinExit (substituteValue substitutions status)
    GrinCase scrutinee binder alternatives ->
      GrinCase
        (substituteValue substitutions scrutinee)
        binder
        [ alternative
            { grinAltRhs = substituteExpr (without (binder : grinAltBinders alternative) substitutions) (grinAltRhs alternative)
            }
        | alternative <- alternatives
        ]
    GrinThrow exception -> GrinThrow (substituteValue substitutions exception)
    GrinCatch runtimeRep action handler state ->
      GrinCatch runtimeRep (substituteValue substitutions action) (substituteValue substitutions handler) (map (substituteValue substitutions) state)
    GrinForeignCallExpr foreignCall arguments -> GrinForeignCallExpr foreignCall (map (substituteValue substitutions) arguments)

substituteStoreRec :: ([(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr) -> Map GrinVar GrinVar -> [(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr
substituteStoreRec constructor substitutions bindings body =
  constructor
    [(var, substituteNode substitutions' node) | (var, node) <- bindings]
    (substituteExpr substitutions' body)
  where
    substitutions' = without (map fst bindings) substitutions

substituteNode :: Map GrinVar GrinVar -> GrinNode -> GrinNode
substituteNode substitutions node =
  node {grinNodeFields = map (substituteValue substitutions) (grinNodeFields node)}

substituteValue :: Map GrinVar GrinVar -> GrinValue -> GrinValue
substituteValue substitutions value =
  case value of
    GrinVarValue var -> GrinVarValue (Map.findWithDefault var var substitutions)
    GrinGlobalValue {} -> value
    GrinLitValue {} -> value

without :: [GrinVar] -> Map GrinVar GrinVar -> Map GrinVar GrinVar
without vars substitutions = foldr Map.delete substitutions vars
