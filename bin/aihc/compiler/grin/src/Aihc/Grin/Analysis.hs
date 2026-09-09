-- | Shared structural analyses over strict GRIN.
module Aihc.Grin.Analysis
  ( freeExprVars,
    maximumProgramVarUnique,
  )
where

import Aihc.Grin.Syntax
import Data.Set (Set)
import Data.Set qualified as Set

freeExprVars :: GrinExpr -> Set GrinVar
freeExprVars expression =
  case expression of
    GrinConstant values -> foldMap freeValueVars values
    GrinBind vars valueExpression body ->
      freeExprVars valueExpression <> (freeExprVars body `Set.difference` Set.fromList vars)
    GrinStore node -> freeNodeVars node
    GrinEnsureHeap requiredWords roots -> freeValueVars requiredWords <> foldMap freeValueVars roots
    GrinStoreUnchecked node -> freeNodeVars node
    GrinStoreRec bindings body -> freeStoreRecVars bindings body
    GrinStoreRecUnchecked bindings body -> freeStoreRecVars bindings body
    GrinUpdate pointer value -> freeValueVars pointer <> freeValueVars value
    GrinUpdateBlackhole pointer value -> freeValueVars pointer <> freeValueVars value
    GrinEval _ value -> freeValueVars value
    GrinCpsEval _ value continuation updateContinuation ->
      freeValueVars value <> freeValueVars continuation <> freeValueVars updateContinuation
    GrinCall _ _ arguments -> foldMap freeValueVars arguments
    GrinPrimitiveCall _ _ arguments -> foldMap freeValueVars arguments
    GrinCpsPrimitiveCall _ _ arguments continuation ->
      foldMap freeValueVars arguments <> freeValueVars continuation
    GrinApply _ function arguments -> freeValueVars function <> foldMap freeValueVars arguments
    GrinCpsApply _ function arguments continuation ->
      freeValueVars function <> foldMap freeValueVars arguments <> freeValueVars continuation
    GrinContinue continuation values -> freeValueVars continuation <> foldMap freeValueVars values
    GrinCpsRaise exception continuation -> freeValueVars exception <> freeValueVars continuation
    GrinHalt values -> foldMap freeValueVars values
    GrinExit status -> freeValueVars status
    GrinCase scrutinee binder alternatives ->
      freeValueVars scrutinee <> foldMap (freeAlternativeVars binder) alternatives
    GrinThrow exception -> freeValueVars exception
    GrinCatch _ action handler state ->
      freeValueVars action <> freeValueVars handler <> foldMap freeValueVars state
    GrinForeignCallExpr _ arguments -> foldMap freeValueVars arguments

freeStoreRecVars :: [(GrinVar, GrinNode)] -> GrinExpr -> Set GrinVar
freeStoreRecVars bindings body =
  (foldMap (freeNodeVars . snd) bindings <> freeExprVars body)
    `Set.difference` Set.fromList (map fst bindings)

freeAlternativeVars :: GrinVar -> GrinAlt -> Set GrinVar
freeAlternativeVars binder alternative =
  freeExprVars (grinAltRhs alternative)
    `Set.difference` Set.fromList (binder : grinAltBinders alternative)

freeValueVars :: GrinValue -> Set GrinVar
freeValueVars value =
  case value of
    GrinVarValue var -> Set.singleton var
    GrinGlobalValue {} -> Set.empty
    GrinLitValue {} -> Set.empty

freeNodeVars :: GrinNode -> Set GrinVar
freeNodeVars = foldMap freeValueVars . grinNodeFields

maximumProgramVarUnique :: GrinProgram -> Int
maximumProgramVarUnique program =
  maximum
    ( 0
        : map (grinVarUnique . fst) (grinPrimitives program)
          <> concatMap staticUniques (grinGlobals program)
          <> concatMap functionUniques (grinFunctions program)
    )
  where
    staticUniques (_, node) = concatMap valueUnique (grinNodeFields node)
    functionUniques function = map grinVarUnique (grinFunctionParameters function) <> exprUniques (grinFunctionBody function)
    valueUnique value =
      case value of
        GrinVarValue var -> [grinVarUnique var]
        GrinGlobalValue {} -> []
        GrinLitValue {} -> []
    nodeUniques = concatMap valueUnique . grinNodeFields
    altUniques alternative = map grinVarUnique (grinAltBinders alternative) <> exprUniques (grinAltRhs alternative)
    exprUniques expr =
      case expr of
        GrinConstant values -> concatMap valueUnique values
        GrinBind vars valueExpression body -> map grinVarUnique vars <> exprUniques valueExpression <> exprUniques body
        GrinStore node -> nodeUniques node
        GrinEnsureHeap requiredWords roots -> valueUnique requiredWords <> concatMap valueUnique roots
        GrinStoreUnchecked node -> nodeUniques node
        GrinStoreRec bindings body -> storeRecUniques bindings body
        GrinStoreRecUnchecked bindings body -> storeRecUniques bindings body
        GrinUpdate pointer value -> concatMap valueUnique [pointer, value]
        GrinUpdateBlackhole pointer value -> concatMap valueUnique [pointer, value]
        GrinEval _ value -> valueUnique value
        GrinCpsEval _ value continuation updateContinuation -> concatMap valueUnique [value, continuation, updateContinuation]
        GrinCall _ _ arguments -> concatMap valueUnique arguments
        GrinPrimitiveCall _ _ arguments -> concatMap valueUnique arguments
        GrinCpsPrimitiveCall _ _ arguments continuation -> concatMap valueUnique (continuation : arguments)
        GrinApply _ function arguments -> concatMap valueUnique (function : arguments)
        GrinCpsApply _ function arguments continuation -> concatMap valueUnique (function : continuation : arguments)
        GrinContinue continuation values -> concatMap valueUnique (continuation : values)
        GrinCpsRaise exception continuation -> concatMap valueUnique [exception, continuation]
        GrinHalt values -> concatMap valueUnique values
        GrinExit status -> valueUnique status
        GrinCase scrutinee binder alternatives ->
          valueUnique scrutinee
            <> (grinVarUnique binder : concatMap altUniques alternatives)
        GrinThrow exception -> valueUnique exception
        GrinCatch _ action handler state -> concatMap valueUnique (action : handler : state)
        GrinForeignCallExpr _ arguments -> concatMap valueUnique arguments
    storeRecUniques bindings body = concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings <> exprUniques body
