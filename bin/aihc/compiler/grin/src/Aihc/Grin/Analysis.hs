-- | Shared structural analyses over strict GRIN.
module Aihc.Grin.Analysis
  ( freeExprVars,
    freeNodeVars,
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
          <> concatMap bindingUniques (grinGlobals program)
          <> concatMap functionUniques (grinFunctions program)
    )
  where
    bindingUniques (_, node) = nodeUniques node
    functionUniques function =
      map grinVarUnique (grinFunctionParameters function)
        <> exprUniques (grinFunctionBody function)

exprUniques :: GrinExpr -> [Int]
exprUniques expression =
  case expression of
    GrinConstant values -> concatMap valueUniques values
    GrinBind vars valueExpression body ->
      map grinVarUnique vars <> exprUniques valueExpression <> exprUniques body
    GrinStore node -> nodeUniques node
    GrinEnsureHeap requiredWords roots -> valueUniques requiredWords <> concatMap valueUniques roots
    GrinStoreUnchecked node -> nodeUniques node
    GrinStoreRec bindings body ->
      concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings
        <> exprUniques body
    GrinStoreRecUnchecked bindings body ->
      concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings
        <> exprUniques body
    GrinUpdate pointer value -> valueUniques pointer <> valueUniques value
    GrinUpdateBlackhole pointer value -> valueUniques pointer <> valueUniques value
    GrinEval _ value -> valueUniques value
    GrinCpsEval _ value continuation updateContinuation ->
      valueUniques value <> valueUniques continuation <> valueUniques updateContinuation
    GrinCall _ _ arguments -> concatMap valueUniques arguments
    GrinPrimitiveCall _ _ arguments -> concatMap valueUniques arguments
    GrinCpsPrimitiveCall _ _ arguments continuation ->
      concatMap valueUniques arguments <> valueUniques continuation
    GrinApply _ function arguments -> valueUniques function <> concatMap valueUniques arguments
    GrinCpsApply _ function arguments continuation ->
      valueUniques function <> concatMap valueUniques arguments <> valueUniques continuation
    GrinContinue continuation values -> valueUniques continuation <> concatMap valueUniques values
    GrinCpsRaise exception continuation -> valueUniques exception <> valueUniques continuation
    GrinHalt values -> concatMap valueUniques values
    GrinExit status -> valueUniques status
    GrinCase scrutinee binder alternatives ->
      valueUniques scrutinee
        <> (grinVarUnique binder : concatMap alternativeUniques alternatives)
    GrinThrow exception -> valueUniques exception
    GrinCatch _ action handler state ->
      valueUniques action <> valueUniques handler <> concatMap valueUniques state
    GrinForeignCallExpr _ arguments -> concatMap valueUniques arguments

alternativeUniques :: GrinAlt -> [Int]
alternativeUniques alternative =
  map grinVarUnique (grinAltBinders alternative)
    <> exprUniques (grinAltRhs alternative)

valueUniques :: GrinValue -> [Int]
valueUniques value =
  case value of
    GrinVarValue var -> [grinVarUnique var]
    GrinGlobalValue {} -> []
    GrinLitValue {} -> []

nodeUniques :: GrinNode -> [Int]
nodeUniques = concatMap valueUniques . grinNodeFields
