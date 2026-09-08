{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for expressions.
--
-- This module implements bidirectional type inference/checking for the
-- surface expression language. It walks the surface AST and returns the same
-- expression with pending type-checker annotations attached at the exact sites
-- that produced them.
module Aihc.Tc.Generate.Expr
  ( inferExpr,
    inferExprAt,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    CaseAlt (..),
    CompStmt (..),
    DoFlavor (..),
    DoStmt (..),
    Expr (..),
    FloatType (..),
    GuardedRhs (..),
    LambdaCaseAlt (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    RecordField (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    Type,
    UnqualifiedName (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName, displayIdentifier)
import Aihc.Tc.Annotations (PendingTcAnnotation (..), pendingAnnotation, pendingTypeLambdaAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), PatSynDirection (..), PatSynInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..), EvVar)
import Aihc.Tc.Generate.Bind (boolTyCon, inferGuardedRhss, inferLocalDecls, inferRhsWithLocals)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch)
import Aihc.Tc.Generate.Record (constructorNameSyntax, lookupRecordConstructor, orderRecordFields, recordFieldLabel, recordUpdateConstructors, synthesizedRecordLocal)
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (checkSurfaceType, tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Dict (DictResult (..), solveDictWithGivens)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unify)
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (when)
import Data.Either (fromRight)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the pending-annotated expression, the inferred type, and wanted
-- constraints.
inferExpr :: Expr -> TcM (Expr, TcType, [Ct])
inferExpr = inferExprAt NoSourceSpan

inferExprAt :: SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferExprAt ambient expr = case expr of
  EAnn integerAnn (EAnn ann inner)
    | Just integerResolution <- fromAnnotation @ResolutionAnnotation integerAnn,
      resolutionNamespace integerResolution == ResolutionNamespaceType,
      Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isFromIntegerResolution resolution,
      EInt _ TInteger _ <- inner -> do
        (literal, ty, cts) <- inferOverloadedIntegerLiteral ambient (Just integerResolution) ann resolution inner
        pure (EAnn integerAnn literal, ty, cts)
  EAnn ann inner
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isFromIntegerResolution resolution,
      EInt _ TInteger _ <- inner ->
        inferOverloadedIntegerLiteral ambient Nothing ann resolution inner
  EAnn ann inner
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isSyntaxTermResolution "fromRational" resolution,
      EFloat _ TFractional _ <- inner ->
        inferOverloadedLiteral ambient "fromRational" [] ann resolution inner
  EAnn ann inner
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isSyntaxTermResolution "fromString" resolution,
      EString _ _ <- inner ->
        inferOverloadedLiteral ambient "fromString" [] ann resolution inner
  EAnn ann inner
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      resolutionNamespace resolution == ResolutionNamespaceType,
      isPrimitiveLiteral inner ->
        inferPrimitiveLiteral ann resolution inner
  EAnn ann (EIf cond thenE elseE)
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isIfThenElseResolution resolution ->
        inferRebindableIf (resolutionSpan resolution `orSourceSpan` ambient) ann resolution cond thenE elseE
  EAnn ann (ENegate inner)
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isSyntaxTermResolution "negate" resolution ->
        inferNegate (resolutionSpan resolution `orSourceSpan` ambient) ann resolution inner
  EVar name ->
    inferVar (exprSpan expr `orSourceSpan` ambient) name
  EImplicitParam name ->
    inferImplicitParam (exprSpan expr `orSourceSpan` ambient) name
  EInt {} ->
    abortTc "integer literal is missing its resolver type annotation"
  EFloat {} ->
    abortTc "fractional literal is missing its resolver type annotation"
  EChar _ _ ->
    literalResult expr (resolvedType "Char")
  ECharHash {} ->
    abortTc "primitive character literal is missing its resolver type annotation"
  EString _ _ ->
    literalResult expr stringTyCon
  EStringHash {} ->
    abortTc "primitive string literal is missing its resolver type annotation"
  ELambdaPats pats body ->
    inferLambda (exprSpan expr `orSourceSpan` ambient) pats body
  ELambdaCase alts ->
    inferLambdaCase (exprSpan expr `orSourceSpan` ambient) alts
  ELambdaCases alts ->
    inferLambdaCases (exprSpan expr `orSourceSpan` ambient) alts
  EApp fun arg ->
    inferApp (exprSpan expr `orSourceSpan` ambient) fun arg
  ETypeApp fun tyArg ->
    inferTypeApp (exprSpan expr `orSourceSpan` ambient) fun tyArg
  EInfix lhs op rhs ->
    inferInfix (exprSpan expr `orSourceSpan` ambient) lhs op rhs
  ESectionL inner op ->
    inferSectionL (exprSpan expr `orSourceSpan` ambient) inner op
  ESectionR op inner ->
    inferSectionR (exprSpan expr `orSourceSpan` ambient) op inner
  EIf cond thenE elseE ->
    inferIf (exprSpan expr `orSourceSpan` ambient) cond thenE elseE
  EMultiWayIf alternatives ->
    inferMultiWayIf (exprSpan expr `orSourceSpan` ambient) alternatives
  ECase scrutinee alts ->
    inferCase (exprSpan expr `orSourceSpan` ambient) scrutinee alts
  ERecordCon name fields wildcard ->
    inferRecordCon (sourceSpanFromAnns (nameAnns name) `orSourceSpan` ambient) name fields wildcard
  ERecordUpd record fields ->
    inferRecordUpdate (exprSpan expr `orSourceSpan` ambient) record fields
  ELetDecls decls body -> do
    (decls', body', bodyTy, cts) <- inferLocalDecls inferExpr decls (inferExpr body)
    pure (ELetDecls decls' body', bodyTy, cts)
  EParen inner -> do
    (inner', ty, cts) <- inferExprAt (exprSpan expr `orSourceSpan` ambient) inner
    pure (EParen inner', ty, cts)
  -- An expression pragma such as SCC does not change the type. The
  -- compiler ignores pragmas and keeps the wrapped expression.
  EPragma pragma inner -> do
    (inner', ty, cts) <- inferExprAt ambient inner
    pure (EPragma pragma inner', ty, cts)
  ETypeSig inner tyAnn -> do
    inferTypeSig (exprSpan expr `orSourceSpan` ambient) inner tyAnn
  ENegate inner -> do
    (inner', innerTy, cs) <- inferExpr inner
    pure (ENegate inner', innerTy, cs)
  EAnn ann inner -> do
    (inner', ty, cts) <- inferExprAt (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
    pure (EAnn ann inner', ty, cts)
  ETuple flavor elems ->
    inferTuple (exprSpan expr `orSourceSpan` ambient) flavor elems
  EList elems ->
    inferList (exprSpan expr `orSourceSpan` ambient) elems
  EListComp body quals ->
    inferListComp (exprSpan expr `orSourceSpan` ambient) body quals
  EArithSeq arithSeq ->
    inferArithSeq (exprSpan expr `orSourceSpan` ambient) arithSeq
  EDo stmts flavor ->
    inferDo (exprSpan expr `orSourceSpan` ambient) flavor stmts
  -- A Template Haskell quote compiles to a runtime error, so it has any
  -- type the context wants.
  _ | isTemplateHaskellQuote expr -> literalResult expr freshMetaTv
  other -> do
    emitError (exprSpan expr `orSourceSpan` ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (expr, ty, [])

-- | A Template Haskell quote such as @[| e |]@, @[t| ty |]@, or @'name@.
isTemplateHaskellQuote :: Expr -> Bool
isTemplateHaskellQuote expr =
  case expr of
    ETHExpQuote {} -> True
    ETHTypedQuote {} -> True
    ETHDeclQuote {} -> True
    ETHTypeQuote {} -> True
    ETHPatQuote {} -> True
    ETHNameQuote {} -> True
    ETHTypeNameQuote {} -> True
    _ -> False

literalResult :: Expr -> TcM TcType -> TcM (Expr, TcType, [Ct])
literalResult expr typeAction = do
  ty <- typeAction
  pure (annotatePendingExpr (pendingAnnotation ty [] [] []) expr, ty, [])

-- | Infer the type of a variable reference.
inferVar :: SourceSpan -> Name -> TcM (Expr, TcType, [Ct])
inferVar ambient nameSyntax = do
  (mPending, ty, cts) <- inferNameOccurrence ambient nameSyntax
  let expr =
        case mPending of
          Just pending -> annotatePendingExprAt (sourceSpanFromAnns (nameAnns nameSyntax)) pending (EVar nameSyntax)
          Nothing -> EVar nameSyntax
  pure (expr, ty, cts)

-- | Infer the type of an implicit-parameter use such as @?x@.
--
-- The use has a fresh type and wants @?x@ at that type. The solver connects
-- the wanted constraint to a binding, and the evidence is the bound value.
inferImplicitParam :: SourceSpan -> Text -> TcM (Expr, TcType, [Ct])
inferImplicitParam sp name = do
  ty <- freshMetaTv
  ev <- freshEvVar
  let ct = mkWantedCt (IParamPred name ty) ev (ImplicitParamOrigin name) sp
      expr = annotatePendingExprAt sp (pendingAnnotation ty [] [ev] []) (EImplicitParam name)
  pure (expr, ty, [ct])

inferOperator :: SourceSpan -> Name -> TcM (Name, TcType, [Ct])
inferOperator ambient nameSyntax = do
  (mPending, ty, cts) <- inferNameOccurrence ambient nameSyntax
  let name' =
        case mPending of
          Just pending -> annotatePendingName pending nameSyntax
          Nothing -> nameSyntax
  pure (name', ty, cts)

inferNameOccurrence :: SourceSpan -> Name -> TcM (Maybe PendingTcAnnotation, TcType, [Ct])
inferNameOccurrence ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) `orSourceSpan` ambient
      name = nameToText nameSyntax
  target <- resolvedTermTarget nameSyntax
  mBinder <- lookupResolvedTerm name target
  rejectUnidirectionalPatSyn sp name target
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      let typeArgs = instTypeArgs inst
          evidenceVars = map ctEvVar cts
          pending = occurrenceAnnotation (instType inst) typeArgs evidenceVars
      pure (pending, instType inst, cts)
    Just (TcMonoIdBinder ty) -> do
      (instantiatedTy, typeArgs, predicates) <- instantiateSigmaType ty
      cts <- mapM (predToCt sp name) predicates
      let evidenceVars = map ctEvVar cts
      pure (occurrenceAnnotation instantiatedTy typeArgs evidenceVars, instantiatedTy, cts)
    Nothing ->
      abortTc ("resolved term missing from type environment: " <> show name <> " resolved as " <> show target)

-- | A unidirectional pattern synonym has no builder. An expression cannot
-- use it.
rejectUnidirectionalPatSyn :: SourceSpan -> Text -> ResolvedName -> TcM ()
rejectUnidirectionalPatSyn sp name target = do
  mPatSyn <- lookupPatSynTarget target
  case mPatSyn of
    Just info
      | psiDirection info == PatSynUnidirectionalInfo ->
          emitError sp (OtherError ("unidirectional pattern synonym " <> T.unpack name <> " cannot be used as an expression"))
    _ -> pure ()

occurrenceAnnotation :: TcType -> [TcType] -> [EvVar] -> Maybe PendingTcAnnotation
occurrenceAnnotation ty typeArgs evidenceVars
  | null typeArgs && null evidenceVars = Nothing
  | otherwise = Just (pendingAnnotation ty typeArgs evidenceVars [])

inferTypeSig :: SourceSpan -> Expr -> Type -> TcM (Expr, TcType, [Ct])
inferTypeSig sp inner tyAnn = do
  (inner', innerTy, cts) <- inferExprAt sp inner
  scoped <- getScopedTyVars
  sigTy <- checkSurfaceType scoped tyAnn KType
  ev <- freshEvVar
  let sigCt =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = innerTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin sp
            }
          TypeTrace
            { typeTraceType = sigTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = TypeSignatureOrigin "<expression>" sp
            }
          ev
          (SigOrigin sp)
          sp
  pure (ETypeSig inner' tyAnn, sigTy, cts <> [sigCt])

-- | An overloaded integer literal is @fromInteger (n :: Integer)@.
--
-- The resolver gives the Integer type when the built-in scope has it.
-- The argument type of the method then equals Integer.
inferOverloadedIntegerLiteral :: SourceSpan -> Maybe ResolutionAnnotation -> Annotation -> ResolutionAnnotation -> Expr -> TcM (Expr, TcType, [Ct])
inferOverloadedIntegerLiteral ambient integerResolution =
  inferOverloadedLiteral ambient "fromInteger" (maybe [] pure integerResolution)

-- | Infer an overloaded literal that a class method converts.
--
-- @literalResolutions@ holds the resolution of the type of the argument of
-- the method, when the resolver gives it. The argument type of the method
-- must then equal that type.
inferOverloadedLiteral :: SourceSpan -> Text -> [ResolutionAnnotation] -> Annotation -> ResolutionAnnotation -> Expr -> TcM (Expr, TcType, [Ct])
inferOverloadedLiteral ambient methodName literalResolutions resolutionAnn resolution literalExpr = do
  let sp = resolutionSpan resolution `orSourceSpan` ambient
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp methodName resolution
  resultTy <- freshMetaTv
  ev <- freshEvVar
  literalArgTy <-
    case methodTy of
      TcFunTy argumentTy _ -> pure argumentTy
      _ -> abortTc (T.unpack methodName <> " does not have a function type")
  literalCts <- concat <$> mapM (\literalResolution -> resolvedLiteralCts sp literalResolution literalArgTy) literalResolutions
  let expectedMethodTy = TcFunTy literalArgTy resultTy
      methodEq =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = methodTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ConstraintTypeOrigin (OccurrenceOf methodName)
            }
          TypeTrace
            { typeTraceType = expectedMethodTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ConstraintTypeOrigin (LitOrigin sp)
            }
          ev
          (LitOrigin sp)
          sp
      pending =
        pendingAnnotation
          resultTy
          typeArgs
          (map ctEvVar methodCts)
          []
  pure (annotatePendingExprAt sp pending (EAnn resolutionAnn literalExpr), resultTy, methodCts <> [methodEq] <> literalCts)

inferPrimitiveLiteral :: Annotation -> ResolutionAnnotation -> Expr -> TcM (Expr, TcType, [Ct])
inferPrimitiveLiteral resolutionAnn resolution literalExpr = do
  maybeInfo <- lookupResolvedTypeSyntax resolution
  case maybeInfo of
    Just info ->
      literalResult (EAnn resolutionAnn literalExpr) (pure (TcTyCon (tciTyCon info) []))
    Nothing ->
      abortTc ("resolved primitive literal type missing from type environment: " <> show (resolutionTarget resolution))

isPrimitiveLiteral :: Expr -> Bool
isPrimitiveLiteral expr =
  case expr of
    EInt _ numericType _ -> numericType /= TInteger
    EFloat _ floatType _ -> floatType /= TFractional
    ECharHash {} -> True
    EStringHash {} -> True
    _ -> False

-- | Whether a resolver annotation names the given syntax term.
isSyntaxTermResolution :: Text -> ResolutionAnnotation -> Bool
isSyntaxTermResolution name resolution =
  resolutionNamespace resolution == ResolutionNamespaceTerm
    && resolutionIdentifier resolution == IdentifierNamed name

isFromIntegerResolution :: ResolutionAnnotation -> Bool
isFromIntegerResolution = isSyntaxTermResolution "fromInteger"

-- | Whether a resolver annotation names the method that sequences a do statement.
isDoMethodResolution :: ResolutionAnnotation -> Bool
isDoMethodResolution resolution =
  isSyntaxTermResolution ">>=" resolution || isSyntaxTermResolution ">>" resolution

isIfThenElseResolution :: ResolutionAnnotation -> Bool
isIfThenElseResolution = isSyntaxTermResolution "ifThenElse"

isArithSeqResolution :: ResolutionAnnotation -> Bool
isArithSeqResolution resolution =
  resolutionNamespace resolution == ResolutionNamespaceTerm
    && resolutionIdentifier resolution
      `elem` map IdentifierNamed ["enumFrom", "enumFromThen", "enumFromTo", "enumFromThenTo"]

annotatePendingExpr :: PendingTcAnnotation -> Expr -> Expr
annotatePendingExpr ann =
  EAnn (mkAnnotation ann)

annotatePendingExprAt :: SourceSpan -> PendingTcAnnotation -> Expr -> Expr
annotatePendingExprAt NoSourceSpan ann =
  annotatePendingExpr ann
annotatePendingExprAt sp ann =
  EAnn (mkAnnotation sp) . annotatePendingExpr ann

annotatePendingName :: PendingTcAnnotation -> Name -> Name
annotatePendingName ann name =
  name {nameAnns = nameAnns name <> [mkAnnotation ann]}

-- | Convert a predicate to a wanted constraint.
predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  pure $
    mkWantedCt p ev (OccurrenceOf name) sp

-- | Infer the type of a lambda expression.
inferLambda :: SourceSpan -> [Pattern] -> Expr -> TcM (Expr, TcType, [Ct])
inferLambda sp pats body = do
  argTys <- mapM (const freshMetaTv) pats
  patCheck <- checkFunctionPatterns sp (zip pats argTys)
  (body', bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferExpr body)
  remainingCts <- solvePatternBranch sp patCheck bodyTy bodyCts
  let funTy = foldr TcFunTy bodyTy argTys
      pats' = zipWith (annotateLambdaPattern (pcBindings patCheck)) argTys (pcPatterns patCheck)
      lambda = annotatePendingExprAt sp (pendingAnnotation funTy [] [] []) (ELambdaPats pats' body')
  pure (lambda, funTy, remainingCts)

annotateLambdaPattern :: [(UnqualifiedName, TcType)] -> TcType -> Pattern -> Pattern
annotateLambdaPattern bindings argTy pat =
  let annotated = annotatePatternBindings bindings pat
   in if lambdaPatternCarriesBinderType annotated
        then annotated
        else PAnn (mkAnnotation (pendingAnnotation argTy [] [] [])) annotated

lambdaPatternCarriesBinderType :: Pattern -> Bool
lambdaPatternCarriesBinderType (PAnn _ inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType PVar {} = True
lambdaPatternCarriesBinderType (PParen inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType PAs {} = True
lambdaPatternCarriesBinderType (PStrict inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PIrrefutable inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PTypeSig inner _) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType _ = False

inferLambdaCase :: SourceSpan -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
inferLambdaCase sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  (alts', cts) <- inferCaseAlts sp argTy resTy alts
  pure (ELambdaCase alts', TcFunTy argTy resTy, cts)

inferCase :: SourceSpan -> Expr -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
inferCase sp scrutinee alts = do
  (scrutinee', scrutTy, scrutCts) <- inferExpr scrutinee
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAlts sp scrutTy resTy alts
  let pending = pendingAnnotation resTy [] [] []
  pure (annotatePendingExprAt sp pending (ECase scrutinee' alts'), resTy, scrutCts ++ altCts)

-- | Record construction is constructor application with the arguments in
-- field declaration order.
inferRecordCon :: SourceSpan -> Name -> [RecordField Expr] -> Bool -> TcM (Expr, TcType, [Ct])
inferRecordCon sp name fields wildcard = do
  when wildcard $
    abortTc ("record wildcard construction is not supported at " <> show sp)
  con <- lookupRecordConstructor name
  args <- orderRecordFields sp con fields missingField
  inferExprAt sp (foldl EApp (EVar name) args)
  where
    missingField field =
      abortTc ("record construction of " <> T.unpack (nameText name) <> " does not give the field " <> show (fromMaybe "<positional>" (dcfiLabel field)) <> " at " <> show sp)

-- | A record update is a case expression. Each alternative matches one
-- constructor that has every updated field and rebuilds it with the new
-- field values.
inferRecordUpdate :: SourceSpan -> Expr -> [RecordField Expr] -> TcM (Expr, TcType, [Ct])
inferRecordUpdate sp record fields = do
  (record', recordTy, recordCts) <- inferExprAt sp record
  zonked <- zonkType recordTy
  constructors <- recordUpdateConstructors sp (Just zonked) (map recordFieldLabel fields)
  alts <- mapM updateAlternative constructors
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAlts sp recordTy resTy alts
  let pending = pendingAnnotation resTy [] [] []
  pure (annotatePendingExprAt sp pending (ECase record' alts'), resTy, recordCts ++ altCts)
  where
    updateAlternative con = do
      binders <- mapM (\index -> synthesizedRecordLocal ("$field" <> T.pack (show index))) [1 .. length (dciFields con)]
      let conSyntax = constructorNameSyntax con
          argument field binder =
            case [recordFieldValue occurrence | occurrence <- fields, Just (recordFieldLabel occurrence) == dcfiLabel field] of
              value : _ -> value
              [] -> EVar (Name Nothing (unqualifiedNameType binder) (unqualifiedNameText binder) (unqualifiedNameAnns binder))
          body = foldl EApp (EVar conSyntax) (zipWith argument (dciFields con) binders)
      pure (CaseAlt [] (PCon conSyntax [] (map PVar binders)) (UnguardedRhs [] body Nothing))

inferLambdaCases :: SourceSpan -> [LambdaCaseAlt] -> TcM (Expr, TcType, [Ct])
inferLambdaCases sp alts = do
  let arity = maximum (0 : map (length . lambdaCaseAltPats) alts)
  argTys <- mapM (const freshMetaTv) [1 .. arity]
  resTy <- freshMetaTv
  results <- mapM (inferLambdaCaseAlt sp argTys resTy) alts
  let alts' = map fst results
      cts = concatMap snd results
  pure (ELambdaCases alts', foldr TcFunTy resTy argTys, cts)

inferCaseAlts :: SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM ([CaseAlt Expr], [Ct])
inferCaseAlts _sp _scrutTy _resTy [] = pure ([], [])
inferCaseAlts sp scrutTy resTy alternatives = do
  results <- mapM inferAlt alternatives
  pure (map fst results, concatMap snd results)
  where
    inferAlt (CaseAlt altAnns pat rhs) = do
      let altSp = sourceSpanFromAnns altAnns
          branchSp = combineSourceSpan altSp sp
      patCheck <- checkPattern branchSp pat scrutTy
      (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
      resultEv <- freshEvVar
      let rhsSp = rhsExprSpan rhs `orSourceSpan` branchSp
          resultCt =
            mkWantedEqCt
              TypeTrace
                { typeTraceType = rhsTy,
                  typeTraceRole = ActualType,
                  typeTraceOrigin = ExpressionTypeOrigin rhsSp
                }
              TypeTrace
                { typeTraceType = resTy,
                  typeTraceRole = ExpectedType,
                  typeTraceOrigin = ConstraintTypeOrigin (CaseBranchOrigin branchSp)
                }
              resultEv
              (CaseBranchOrigin rhsSp)
              rhsSp
          pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      remainingCts <- solvePatternBranch branchSp patCheck resTy (rhsCts <> [resultCt])
      pure (CaseAlt altAnns pat' rhs', remainingCts)

inferLambdaCaseAlt :: SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM (LambdaCaseAlt, [Ct])
inferLambdaCaseAlt sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkFunctionPatterns sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
  ev <- freshEvVar
  let pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck)
      rhsCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  remainingCts <- solvePatternBranch sp patCheck resTy (rhsCts <> [rhsCt])
  pure (alt {lambdaCaseAltPats = pats', lambdaCaseAltRhs = rhs'}, remainingCts)

sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    sp : _ -> sp

combineSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
combineSourceSpan NoSourceSpan fallback = fallback
combineSourceSpan span' _ = span'

inferRhs :: Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhs = inferRhsWithLocals inferExpr

inferApp :: SourceSpan -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferApp sp = inferApplication sp EApp

-- | Infer an application. The rebuild function makes the checked node from
-- the checked function and argument, so an application operator keeps its
-- infix node.
inferApplication :: SourceSpan -> (Expr -> Expr -> Expr) -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferApplication sp rebuild fun arg = do
  (rawFun, rawFunTy, rawFunCts) <- inferExpr fun
  -- A function with a polymorphic type, for example a record field with a
  -- higher-rank type, is instantiated at its application.
  (fun', funTy, instantiationCts) <- instantiateFunctionType sp rawFun rawFunTy
  let funCts = rawFunCts <> instantiationCts
  zonkedFunTy <- zonkType funTy
  case zonkedFunTy of
    TcFunTy expectedArgTy resultTy
      | hasLeadingForAll expectedArgTy -> do
          (arg', argCts) <- checkHigherRankArgument sp expectedArgTy arg
          pure (rebuild fun' arg', resultTy, funCts <> argCts)
      | otherwise -> do
          -- The function type is known, so the result type is known too.
          -- An enclosing application then sees a function type, which lets
          -- it check a higher-rank argument.
          (arg', argTy, argCts) <- inferExpr arg
          ev <- freshEvVar
          let eqCt = mkWantedCt (EqPred expectedArgTy argTy) ev (AppOrigin sp) sp
          pure (rebuild fun' arg', resultTy, funCts <> argCts <> [eqCt])
    _ -> do
      (arg', argTy, argCts) <- inferExpr arg
      resTy <- freshMetaTv
      ev <- freshEvVar
      let eqCt = mkWantedCt (EqPred funTy (TcFunTy argTy resTy)) ev (AppOrigin sp) sp
      pure (rebuild fun' arg', resTy, funCts <> argCts <> [eqCt])

-- | Instantiate the leading quantifiers and context of a function type.
-- The context becomes wanted constraints. The function expression gets an
-- annotation with the type arguments and the evidence, so the desugarer
-- applies them.
instantiateFunctionType :: SourceSpan -> Expr -> TcType -> TcM (Expr, TcType, [Ct])
instantiateFunctionType sp fun funTy = do
  zonked <- zonkType funTy
  if hasLeadingForAll zonked
    then do
      (instantiated, typeArgs, predicates) <- instantiateSigmaType zonked
      cts <- mapM (predToCt sp "<application>") predicates
      let pending = pendingAnnotation instantiated typeArgs (map ctEvVar cts) []
      pure (annotatePendingExprAt sp pending fun, instantiated, cts)
    else pure (fun, funTy, [])

-- | Whether an operator is the application operator of GHC.Base. GHC types
-- @f $ x@ like the application @f x@, so a higher-rank or representation
-- polymorphic argument works without impredicative instantiation.
isApplicationOperator :: Name -> TcM Bool
isApplicationOperator op
  | nameText op /= "$" = pure False
  | otherwise = do
      key <- resolvedTermKey op
      pure $ case key of
        TcTermGlobal _ moduleName' "$" -> moduleName' == "GHC.Base"
        _ -> False

checkHigherRankArgument :: SourceSpan -> TcType -> Expr -> TcM (Expr, [Ct])
checkHigherRankArgument sp expectedTy arg = do
  boundary <- getUniqueBoundary
  (arg', actualTy, argCts) <- inferExpr arg
  (skolems, predicates, expectedBody) <- skolemizeSigmaType expectedTy
  unify sp (AppOrigin sp) actualTy expectedBody
  rejectEscapingHigherRankMetas sp boundary skolems actualTy
  givenCts <- mapM makeGiven predicates
  let (equalityCts, dictionaryCts) = partition isEqualityConstraint argCts
  residualEqualities <- concat <$> mapM (solveEqualityConstraint predicates) equalityCts
  residualDictionaries <- concat <$> mapM (solveDictionary predicates) dictionaryCts
  let annotatedArg = annotatePendingExprAt sp (pendingTypeLambdaAnnotation expectedTy skolems (map ctEvVar givenCts)) arg'
  pure (annotatedArg, residualEqualities <> residualDictionaries)
  where
    makeGiven predicate = do
      evidence <- freshEvVar
      bindEvidence evidence (EvGiven predicate)
      pure ((mkWantedCt predicate evidence (AppOrigin sp) sp) {ctFlavor = Given})

    isEqualityConstraint ct =
      case ctPred ct of
        EqPred {} -> True
        _ -> False

    solveEqualityConstraint predicates ct = do
      result <- withGivenPredicates predicates (solveEquality ct)
      pure $ case result of
        EqSolved -> []
        EqStuck stuck -> [stuck]
        EqError err -> [err]

    solveDictionary predicates ct = do
      result <- solveDictWithGivens predicates ct
      pure $ case result of
        DictSolved -> []
        DictStuck stuck -> [stuck]

hasLeadingForAll :: TcType -> Bool
hasLeadingForAll TcForAllTy {} = True
hasLeadingForAll TcQualTy {} = True
hasLeadingForAll _ = False

instantiateSigmaType :: TcType -> TcM (TcType, [TcType], [Pred])
instantiateSigmaType = go []
  where
    go arguments (TcForAllTy binder body) = do
      argument <- freshMetaTv
      go (arguments <> [argument]) (applySubst (Map.singleton (tvUnique binder) argument) body)
    go arguments (TcQualTy predicates body) = pure (body, arguments, predicates)
    go arguments ty = pure (ty, arguments, [])

skolemizeSigmaType :: TcType -> TcM ([TyVarId], [Pred], TcType)
skolemizeSigmaType = go [] []
  where
    go skolems predicates (TcForAllTy binder body) = do
      skolem <- setTyVarKind (tvKind binder) <$> freshSkolemTv (tvName binder)
      go (skolems <> [skolem]) predicates (applySubst (Map.singleton (tvUnique binder) (TcTyVar skolem)) body)
    go skolems predicates (TcQualTy morePredicates body) =
      go skolems (predicates <> morePredicates) body
    go skolems predicates ty = pure (skolems, predicates, ty)

rejectEscapingHigherRankMetas :: SourceSpan -> Unique -> [TyVarId] -> TcType -> TcM ()
rejectEscapingHigherRankMetas sp (Unique boundaryInt) skolems actualTy = do
  let olderMetas = filter (isOlderThan boundaryInt) (typeMetaVariables actualTy)
  escaped <- anyM (metaMentionsAnySkolem skolems) olderMetas
  when escaped $
    emitError sp (OtherError "higher-rank type variable escapes its argument")
  where
    isOlderThan threshold (Unique metaInt) = metaInt < threshold

metaMentionsAnySkolem :: [TyVarId] -> Unique -> TcM Bool
metaMentionsAnySkolem skolems meta = do
  ty <- zonkType (TcMetaTv meta)
  pure (any (`typeMentionsTyVar` ty) skolems)

anyM :: (a -> TcM Bool) -> [a] -> TcM Bool
anyM _ [] = pure False
anyM predicate (value : values) = do
  matches <- predicate value
  if matches then pure True else anyM predicate values

typeMetaVariables :: TcType -> [Unique]
typeMetaVariables ty =
  case ty of
    TcTyVar {} -> []
    TcMetaTv meta -> [meta]
    TcTyCon _ arguments -> concatMap typeMetaVariables arguments
    TcFunTy argument result -> typeMetaVariables argument <> typeMetaVariables result
    TcForAllTy _ body -> typeMetaVariables body
    TcQualTy predicates body -> concatMap predicateMetaVariables predicates <> typeMetaVariables body
    TcAppTy function argument -> typeMetaVariables function <> typeMetaVariables argument

predicateMetaVariables :: Pred -> [Unique]
predicateMetaVariables predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeMetaVariables arguments
    EqPred left right -> typeMetaVariables left <> typeMetaVariables right
    IParamPred _ payload -> typeMetaVariables payload
    QuantifiedPred variables antecedents consequent ->
      concatMap (typeMetaVariables . tvKind) variables
        <> concatMap predicateMetaVariables antecedents
        <> predicateMetaVariables consequent

typeMentionsTyVar :: TyVarId -> TcType -> Bool
typeMentionsTyVar target ty =
  case ty of
    TcTyVar tyVar -> tyVar == target
    TcMetaTv {} -> False
    TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments
    TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result
    TcForAllTy binder body -> binder /= target && typeMentionsTyVar target body
    TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body
    TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument

predicateMentionsTyVar :: TyVarId -> Pred -> Bool
predicateMentionsTyVar target predicate =
  case predicate of
    ClassPred _ arguments -> any (typeMentionsTyVar target) arguments
    EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right
    IParamPred _ payload -> typeMentionsTyVar target payload
    QuantifiedPred variables antecedents consequent ->
      target `notElem` variables
        && (any (predicateMentionsTyVar target) antecedents || predicateMentionsTyVar target consequent)

inferTypeApp :: SourceSpan -> Expr -> Type -> TcM (Expr, TcType, [Ct])
inferTypeApp sp fun tyArg = do
  (fun', funTy, funCts) <- inferExpr fun
  scoped <- getScopedTyVars
  explicitTy <- checkSurfaceType scoped tyArg KType
  case drop (visibleTypeApplicationCount fun) (pendingTypeArgs fun') of
    inferredTy : _ -> do
      ev <- freshEvVar
      let origin = InstOrigin "visible type application"
          eqCt =
            mkWantedEqCt
              TypeTrace
                { typeTraceType = inferredTy,
                  typeTraceRole = InferredType,
                  typeTraceOrigin = ConstraintTypeOrigin origin
                }
              TypeTrace
                { typeTraceType = explicitTy,
                  typeTraceRole = RequiredType,
                  typeTraceOrigin = ConstraintTypeOrigin origin
                }
              ev
              origin
              sp
      pure (ETypeApp fun' tyArg, funTy, funCts <> [eqCt])
    [] -> do
      emitError sp (OtherError "visible type application requires a polymorphic expression")
      pure (ETypeApp fun' tyArg, funTy, funCts)

visibleTypeApplicationCount :: Expr -> Int
visibleTypeApplicationCount expr =
  case expr of
    ETypeApp fun _ -> 1 + visibleTypeApplicationCount fun
    EParen inner -> visibleTypeApplicationCount inner
    EPragma _ inner -> visibleTypeApplicationCount inner
    EAnn _ inner -> visibleTypeApplicationCount inner
    _ -> 0

pendingTypeArgs :: Expr -> [TcType]
pendingTypeArgs expr =
  case expr of
    EAnn ann inner ->
      case fromAnnotation @PendingTcAnnotation ann of
        Just pending -> pendingTcAnnTypeArgs pending
        Nothing -> pendingTypeArgs inner
    ETypeApp fun _ -> pendingTypeArgs fun
    EParen inner -> pendingTypeArgs inner
    EPragma _ inner -> pendingTypeArgs inner
    _ -> []

inferInfix :: SourceSpan -> Expr -> Name -> Expr -> TcM (Expr, TcType, [Ct])
inferInfix sp lhs op rhs = do
  isApplication <- isApplicationOperator op
  if isApplication
    then -- The operator node keeps no type arguments. The desugarer reads the
    -- result type from the left operand.
      inferApplication sp (`EInfix` op) lhs rhs
    else inferInfixOperator sp lhs op rhs

inferInfixOperator :: SourceSpan -> Expr -> Name -> Expr -> TcM (Expr, TcType, [Ct])
inferInfixOperator sp lhs op rhs = do
  -- Generate the same constraints as desugared binary application while
  -- keeping the operator occurrence on the surface operator node.
  (op', opTy, opCts) <- inferOperator sp op
  (lhs', lhsTy, lhsCts) <- inferExpr lhs
  midTy <- freshMetaTv
  lhsEv <- freshEvVar
  let lhsCt = mkWantedCt (EqPred opTy (TcFunTy lhsTy midTy)) lhsEv (AppOrigin sp) sp
  (rhs', rhsTy, rhsCts) <- inferExpr rhs
  resTy <- freshMetaTv
  rhsEv <- freshEvVar
  let rhsCt = mkWantedCt (EqPred midTy (TcFunTy rhsTy resTy)) rhsEv (AppOrigin sp) sp
  pure (EInfix lhs' op' rhs', resTy, opCts ++ lhsCts ++ [lhsCt] ++ rhsCts ++ [rhsCt])

inferSectionL :: SourceSpan -> Expr -> Name -> TcM (Expr, TcType, [Ct])
inferSectionL sp inner op = do
  (op', opTy, opCts) <- inferOperator sp op
  (inner', innerTy, innerCts) <- inferExpr inner
  argumentTy <- freshMetaTv
  resultTy <- freshMetaTv
  evidence <- freshEvVar
  let sectionTy = TcFunTy argumentTy resultTy
      pending = pendingAnnotation sectionTy [] [] [argumentTy]
      wanted = mkWantedCt (EqPred opTy (TcFunTy innerTy sectionTy)) evidence (AppOrigin sp) sp
  pure (annotatePendingExprAt sp pending (ESectionL inner' op'), sectionTy, opCts <> innerCts <> [wanted])

inferSectionR :: SourceSpan -> Name -> Expr -> TcM (Expr, TcType, [Ct])
inferSectionR sp op inner = do
  (op', opTy, opCts) <- inferOperator sp op
  (inner', innerTy, innerCts) <- inferExpr inner
  argumentTy <- freshMetaTv
  resultTy <- freshMetaTv
  evidence <- freshEvVar
  let sectionTy = TcFunTy argumentTy resultTy
      pending = pendingAnnotation sectionTy [] [] [argumentTy]
      wanted = mkWantedCt (EqPred opTy (TcFunTy argumentTy (TcFunTy innerTy resultTy))) evidence (AppOrigin sp) sp
  pure (annotatePendingExprAt sp pending (ESectionR op' inner'), sectionTy, opCts <> innerCts <> [wanted])

inferIf :: SourceSpan -> Expr -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferIf sp cond thenE elseE = do
  (cond', condTy, condCts) <- inferExpr cond
  (thenE', thenTy, thenCts) <- inferExpr thenE
  (elseE', elseTy, elseCts) <- inferExpr elseE
  resultTy <- freshMetaTv
  condEv <- freshEvVar
  expectedBoolTy <- boolTyCon
  let condCt = mkWantedCt (EqPred condTy expectedBoolTy) condEv (AppOrigin sp) sp
  thenEv <- freshEvVar
  elseEv <- freshEvVar
  let thenCt = mkWantedCt (EqPred thenTy resultTy) thenEv (AppOrigin sp) sp
      elseCt = mkWantedCt (EqPred elseTy resultTy) elseEv (AppOrigin sp) sp
      pending = pendingAnnotation resultTy [] [] []
  pure (annotatePendingExprAt sp pending (EIf cond' thenE' elseE'), resultTy, condCts ++ thenCts ++ elseCts ++ [condCt, thenCt, elseCt])

-- | A multi-way if is a guarded right-hand side without a binding. Each
-- alternative has the result type.
inferMultiWayIf :: SourceSpan -> [GuardedRhs Expr] -> TcM (Expr, TcType, [Ct])
inferMultiWayIf sp alternatives = do
  (alternatives', resultTy, cts) <- inferGuardedRhss inferExpr alternatives
  let pending = pendingAnnotation resultTy [] [] []
  pure (annotatePendingExprAt sp pending (EMultiWayIf alternatives'), resultTy, cts)

-- | RebindableSyntax gives an if expression the in-scope ifThenElse.
--
-- The method annotation sits inside the result annotation. The desugarer
-- applies the method to the condition and the two branches.
inferRebindableIf :: SourceSpan -> Annotation -> ResolutionAnnotation -> Expr -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferRebindableIf sp resolutionAnn resolution cond thenE elseE = do
  (cond', condTy, condCts) <- inferExpr cond
  (thenE', thenTy, thenCts) <- inferExpr thenE
  (elseE', elseTy, elseCts) <- inferExpr elseE
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp "ifThenElse" resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = TcFunTy condTy (TcFunTy thenTy (TcFunTy elseTy resultTy))
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf "ifThenElse") sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      resultPending = pendingAnnotation resultTy [] [] []
      annotated = annotatePendingExpr methodPending (EAnn resolutionAnn (EIf cond' thenE' elseE'))
  pure
    ( annotatePendingExprAt sp resultPending annotated,
      resultTy,
      condCts <> thenCts <> elseCts <> methodCts <> [methodEquality]
    )

-- | Negation applies the resolved negate method to its operand.
--
-- The method annotation sits inside the result annotation. The desugarer
-- applies the method to the operand.
inferNegate :: SourceSpan -> Annotation -> ResolutionAnnotation -> Expr -> TcM (Expr, TcType, [Ct])
inferNegate sp resolutionAnn resolution inner = do
  (inner', innerTy, innerCts) <- inferExpr inner
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp "negate" resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = TcFunTy innerTy resultTy
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf "negate") sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      resultPending = pendingAnnotation resultTy [] [] []
      annotated = annotatePendingExpr methodPending (EAnn resolutionAnn (ENegate inner'))
  pure
    ( annotatePendingExprAt sp resultPending annotated,
      resultTy,
      innerCts <> methodCts <> [methodEquality]
    )

inferTuple :: SourceSpan -> TupleFlavor -> [Maybe Expr] -> TcM (Expr, TcType, [Ct])
inferTuple sp flavor elems = do
  results <- mapM inferElem elems
  let elems' = map (\(expr, _, _) -> expr) results
      tys = map (\(_, ty, _) -> ty) results
      cts = concatMap (\(_, _, elemCts) -> elemCts) results
      n = length tys
  wired <- wiredTupleTyCon flavor n
  maybeTyCon <- lookupTyCon (tyConName wired)
  elementKinds <- mapM tcTypeKind tys
  let fallbackKind =
        case flavor of
          Boxed -> foldr KFun KType elementKinds
          Unboxed -> foldr KFun (KTYPE (TupleRep (map runtimeRepOrLifted elementKinds))) elementKinds
  tc <-
    case maybeTyCon of
      Just info -> pure (tciTyCon info)
      Nothing -> mkWiredTyCon wired fallbackKind
  -- A tuple section such as @(0,)@ is a function of its missing fields.
  let tupleTy = TcTyCon tc tys
      missingTys = [ty | (Nothing, ty, _) <- results]
      sectionTy = foldr TcFunTy tupleTy missingTys
      pending = pendingAnnotation sectionTy tys [] []
  pure (annotatePendingExprAt sp pending (ETuple flavor elems'), sectionTy, cts)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (Nothing, ty, [])
    inferElem (Just e) = do
      (e', ty, cts) <- inferExpr e
      pure (Just e', ty, cts)

    runtimeRepOrLifted kind = fromRight liftedRep (runtimeRepFromKind kind)

inferList :: SourceSpan -> [Expr] -> TcM (Expr, TcType, [Ct])
inferList sp elems = do
  nilInstantiation <- instantiateListConstructor sp "[]"
  nilCts <- mapM (predToCt sp "[]") (instPreds nilInstantiation)
  case elems of
    [] -> do
      let listTy = instType nilInstantiation
          pending = pendingAnnotation listTy (instTypeArgs nilInstantiation) (map ctEvVar nilCts) []
      pure (annotatePendingExprAt sp pending (EList []), listTy, nilCts)
    _ -> do
      results <- mapM inferElem elems
      consInstantiation <- instantiateListConstructor sp ":"
      consPredicateCts <- mapM (predToCt sp ":") (instPreds consInstantiation)
      case instType consInstantiation of
        TcFunTy sourceElemTy (TcFunTy sourceTailTy sourceResultTy) -> do
          let elems' = map (\(element, _, _, _) -> element) results
              elemCts = concatMap (\(_, _, cts, _) -> cts) results
              (firstElemTy, firstElemSp) = case results of
                (_, ty, _, elemSp) : _ -> (ty, elemSp)
                [] -> (sourceElemTy, sp)
              pending = pendingAnnotation sourceResultTy [sourceElemTy] [] []
          firstConstructorCt <- constructorEqualityCt firstElemSp firstElemTy sourceElemTy
          nilConstructorCt <- constructorEqualityCt sp (instType nilInstantiation) sourceTailTy
          resultConstructorCt <- constructorEqualityCt sp sourceResultTy sourceTailTy
          elementEqualityCts <- mapM (elementEqualityCt firstElemSp firstElemTy) (drop 1 results)
          let constructorCts = nilCts <> consPredicateCts <> [firstConstructorCt, nilConstructorCt, resultConstructorCt]
          pure (annotatePendingExprAt sp pending (EList elems'), sourceResultTy, elemCts <> elementEqualityCts <> constructorCts)
        _ -> abortTc "GHC.Types list cons constructor has an invalid type"
  where
    inferElem elemExpr = do
      (elemExpr', elemTy, elemCts) <- inferExpr elemExpr
      pure (elemExpr', elemTy, elemCts, exprSpan elemExpr `orSourceSpan` sp)
    constructorEqualityCt loc left right = do
      ev <- freshEvVar
      pure (mkWantedCt (EqPred left right) ev (AppOrigin loc) loc)
    elementEqualityCt firstElemSp firstElemTy (_, elemTy, _, elemSp) = do
      ev <- freshEvVar
      pure $
        mkWantedEqCt
          TypeTrace
            { typeTraceType = elemTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin elemSp
            }
          TypeTrace
            { typeTraceType = firstElemTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ListElementTypeOrigin firstElemSp
            }
          ev
          (AppOrigin elemSp)
          elemSp

inferArithSeq :: SourceSpan -> ArithSeq -> TcM (Expr, TcType, [Ct])
inferArithSeq sp arithSeq = do
  (arithSeq', resultTy, cts) <- inferArithSeqNode sp arithSeq
  let pending = pendingAnnotation resultTy [] [] []
  pure (annotatePendingExprAt sp pending (EArithSeq arithSeq'), resultTy, cts)

inferArithSeqNode :: SourceSpan -> ArithSeq -> TcM (ArithSeq, TcType, [Ct])
inferArithSeqNode sp arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner
      | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
        isArithSeqResolution resolution ->
          inferResolvedArithSeq sp ann resolution inner
      | otherwise -> do
          (inner', resultTy, cts) <- inferArithSeqNode sp inner
          pure (ArithSeqAnn ann inner', resultTy, cts)
    _ -> abortTc "arithmetic sequence is missing its resolved method"

inferResolvedArithSeq :: SourceSpan -> Annotation -> ResolutionAnnotation -> ArithSeq -> TcM (ArithSeq, TcType, [Ct])
inferResolvedArithSeq sp resolutionAnn resolution arithSeq = do
  (arithSeq', argumentTypes, argumentCts) <- inferArithSeqForm arithSeq
  let methodName = displayIdentifier (resolutionIdentifier resolution)
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp methodName resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = foldr TcFunTy resultTy argumentTypes
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf methodName) sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      annotated = ArithSeqAnn (mkAnnotation methodPending) (ArithSeqAnn resolutionAnn arithSeq')
  pure (annotated, resultTy, argumentCts <> methodCts <> [methodEquality])

-- | Instantiate a resolved syntax method such as enumFrom or ifThenElse.
inferResolvedSyntaxMethod :: SourceSpan -> Text -> ResolutionAnnotation -> TcM (TcType, [TcType], [Ct])
inferResolvedSyntaxMethod sp methodName resolution = do
  mBinder <- lookupResolvedTerm methodName (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp methodName) (instPreds inst)
      pure (instType inst, instTypeArgs inst, cts)
    Just (TcMonoIdBinder ty) -> pure (ty, [], [])
    Nothing ->
      abortTc ("resolved " <> T.unpack methodName <> " is missing from the type environment: " <> show (resolutionTarget resolution))

inferArithSeqForm :: ArithSeq -> TcM (ArithSeq, [TcType], [Ct])
inferArithSeqForm arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner -> do
      (inner', types, cts) <- inferArithSeqForm inner
      pure (ArithSeqAnn ann inner', types, cts)
    ArithSeqFrom from -> inferForm ArithSeqFrom [from]
    ArithSeqFromThen from thenExpr -> inferForm2 ArithSeqFromThen from thenExpr
    ArithSeqFromTo from to -> inferForm2 ArithSeqFromTo from to
    ArithSeqFromThenTo from thenExpr to -> do
      results <- mapM inferExpr [from, thenExpr, to]
      case results of
        [(from', fromTy, fromCts), (thenExpr', thenTy, thenCts), (to', toTy, toCts)] ->
          pure (ArithSeqFromThenTo from' thenExpr' to', [fromTy, thenTy, toTy], fromCts <> thenCts <> toCts)
        _ -> abortTc "arithmetic sequence has an invalid argument count"
  where
    inferForm constructor expressions = do
      results <- mapM inferExpr expressions
      case results of
        [(expression', ty, cts)] -> pure (constructor expression', [ty], cts)
        _ -> abortTc "arithmetic sequence has an invalid argument count"
    inferForm2 constructor first second = do
      (first', firstTy, firstCts) <- inferExpr first
      (second', secondTy, secondCts) <- inferExpr second
      pure (constructor first' second', [firstTy, secondTy], firstCts <> secondCts)

instantiateListConstructor :: SourceSpan -> Text -> TcM Instantiation
instantiateListConstructor sp name = do
  sourceBinder <- lookupTerm name
  maybeBinder <- maybe (lookupKnownTerm "GHC.Types" name) (pure . Just) sourceBinder
  case maybeBinder of
    Just (TcIdBinder scheme _) -> instantiateWithArgs scheme
    Just TcMonoIdBinder {} ->
      abortTc ("GHC.Types list constructor is monomorphic at " <> show sp <> ": " <> show name)
    Nothing ->
      abortTc ("GHC.Types list constructor is missing at " <> show sp <> ": " <> show name)

inferListComp :: SourceSpan -> Expr -> [CompStmt] -> TcM (Expr, TcType, [Ct])
inferListComp sp body quals = do
  listTyCon' <- resolvedListTyCon
  (quals', body', bodyTy, cts) <- inferCompQuals listTyCon' sp quals (inferExpr body)
  let resultTy = listType listTyCon' bodyTy
      pending = pendingAnnotation resultTy [bodyTy] [] []
  pure (annotatePendingExprAt sp pending (EListComp body' quals'), resultTy, cts)
  where
    listType tyCon elemTy = TcTyCon tyCon [elemTy]
    inferCompQuals _ _ [] action = do
      (body', bodyTy, bodyCts) <- action
      pure ([], body', bodyTy, bodyCts)
    inferCompQuals listTyCon' ambient (qual : rest) action =
      case qual of
        CompAnn ann inner -> do
          (stmts', body', bodyTy, cts) <- inferCompQuals listTyCon' (compStmtSpan qual `orSourceSpan` ambient) (inner : rest) action
          case stmts' of
            inner' : rest' -> pure (CompAnn ann inner' : rest', body', bodyTy, cts)
            [] -> pure ([], body', bodyTy, cts)
        CompGen pat src -> do
          elemTy <- freshMetaTv
          (src', srcTy, srcCts) <- inferExpr src
          patCheck <- checkPattern ambient pat elemTy
          ev <- freshEvVar
          let srcSp = exprSpan src `orSourceSpan` ambient
              srcListCt = mkWantedCt (EqPred srcTy (listType listTyCon' elemTy)) ev (AppOrigin srcSp) srcSp
          (rest', body', bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferCompQuals listTyCon' ambient rest action)
          remainingCts <- solvePatternBranch ambient patCheck bodyTy bodyCts
          pure (CompGen (annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)) src' : rest', body', bodyTy, srcCts ++ [srcListCt] ++ remainingCts)
        CompGuard guard -> do
          (guard', guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          expectedBoolTy <- boolTyCon
          let guardSp = exprSpan guard `orSourceSpan` ambient
              guardCt = mkWantedCt (EqPred guardTy expectedBoolTy) ev (AppOrigin guardSp) guardSp
          (rest', body', bodyTy, bodyCts) <- inferCompQuals listTyCon' ambient rest action
          pure (CompGuard guard' : rest', body', bodyTy, guardCts ++ [guardCt] ++ bodyCts)
        CompLetDecls decls -> do
          (decls', (rest', body'), bodyTy, bodyCts) <-
            inferLocalDecls inferExpr decls $ do
              (rest', body', bodyTy, bodyCts) <- inferCompQuals listTyCon' ambient rest action
              pure ((rest', body'), bodyTy, bodyCts)
          pure (CompLetDecls decls' : rest', body', bodyTy, bodyCts)
        CompThen {} -> unsupportedQual listTyCon' qual ambient rest action
        CompThenBy {} -> unsupportedQual listTyCon' qual ambient rest action
        CompGroupUsing {} -> unsupportedQual listTyCon' qual ambient rest action
        CompGroupByUsing {} -> unsupportedQual listTyCon' qual ambient rest action

    unsupportedQual listTyCon' qual ambient rest action = do
      let qualSp = compStmtSpan qual `orSourceSpan` ambient
      emitError qualSp (OtherError ("unsupported list comprehension qualifier in TC MVP: " ++ take 50 (show qual)))
      inferCompQuals listTyCon' ambient rest action

resolvedListTyCon :: TcM TyCon
resolvedListTyCon = do
  maybeInfo <- lookupTyCon "[]"
  maybe (mkKnownTyCon "GHC.Types" "[]" 1 (KFun KType KType)) (pure . tciTyCon) maybeInfo

inferDo :: SourceSpan -> DoFlavor -> [DoStmt Expr] -> TcM (Expr, TcType, [Ct])
inferDo sp flavor stmts =
  case flavor of
    DoPlain -> do
      (stmts', resultTy, cts) <- inferDoStmts sp stmts
      let pending = pendingAnnotation resultTy [] [] []
      pure (annotatePendingExprAt sp pending (EDo stmts' flavor), resultTy, cts)
    _ -> do
      emitError sp (OtherError ("unsupported do flavor in TC MVP: " ++ show flavor))
      resultTy <- freshMetaTv
      pure (EDo stmts flavor, resultTy, [])

inferDoStmts :: SourceSpan -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmts sp stmts =
  case stmts of
    [] -> do
      emitError sp (OtherError "empty do block in TC MVP")
      resultTy <- freshMetaTv
      pure ([], resultTy, [])
    [stmt] -> inferLastDoStmt sp stmt
    stmt : rest -> inferDoStmt sp stmt rest

inferLastDoStmt :: SourceSpan -> DoStmt Expr -> TcM ([DoStmt Expr], TcType, [Ct])
inferLastDoStmt ambient stmt =
  case stmt of
    DoAnn ann inner -> do
      (stmts', resultTy, cts) <- inferLastDoStmt (doStmtSpan stmt `orSourceSpan` ambient) inner
      case stmts' of
        [inner'] -> pure ([DoAnn ann inner'], resultTy, cts)
        _ -> pure (stmts', resultTy, cts)
    DoExpr body -> do
      (body', bodyTy, cts) <- inferExprAt ambient body
      pure ([DoExpr body'], bodyTy, cts)
    _ -> do
      emitError ambient (OtherError "the last statement in a do block must be an expression")
      resultTy <- freshMetaTv
      pure ([stmt], resultTy, [])

inferDoStmt :: SourceSpan -> DoStmt Expr -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmt ambient stmt rest =
  case stmt of
    DoAnn ann inner
      | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
        isDoMethodResolution resolution ->
          inferResolvedDoStmt ambient ann resolution inner rest
    DoAnn ann inner -> do
      (stmts', resultTy, cts) <- inferDoStmt (doStmtSpan stmt `orSourceSpan` ambient) inner rest
      case stmts' of
        inner' : rest' -> pure (DoAnn ann inner' : rest', resultTy, cts)
        [] -> pure ([], resultTy, cts)
    DoBind pat action -> do
      monadTy <- freshMetaTv
      itemTy <- freshMetaTv
      resultItemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      patCheck <- checkPattern ambient pat itemTy
      (rest', resultTy, restCts) <-
        withPatternBindings (pcBindings patCheck) (inferDoStmts ambient rest)
      actionEq <- wantedDoEq ambient actionTy (TcAppTy monadTy itemTy)
      resultEq <- wantedDoEq ambient resultTy (TcAppTy monadTy resultItemTy)
      monadCt <- wantedMonad ambient monadTy
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      remainingCts <- solvePatternBranch ambient patCheck resultTy restCts
      pure
        ( DoBind pat' action' : rest',
          resultTy,
          actionCts <> remainingCts <> [actionEq, resultEq, monadCt]
        )
    DoExpr action -> do
      monadTy <- freshMetaTv
      itemTy <- freshMetaTv
      resultItemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      (rest', resultTy, restCts) <- inferDoStmts ambient rest
      actionEq <- wantedDoEq ambient actionTy (TcAppTy monadTy itemTy)
      resultEq <- wantedDoEq ambient resultTy (TcAppTy monadTy resultItemTy)
      monadCt <- wantedMonad ambient monadTy
      pure
        ( DoExpr action' : rest',
          resultTy,
          actionCts <> restCts <> [actionEq, resultEq, monadCt]
        )
    DoLetDecls decls -> do
      (decls', rest', resultTy, cts) <-
        inferLocalDecls inferExpr decls $ do
          (rest', resultTy, restCts) <- inferDoStmts ambient rest
          pure (rest', resultTy, restCts)
      pure (DoLetDecls decls' : rest', resultTy, cts)
    DoRecStmt _ -> do
      emitError ambient (OtherError "recursive do statements are unsupported in TC MVP")
      (rest', resultTy, cts) <- inferDoStmts ambient rest
      pure (stmt : rest', resultTy, cts)

inferResolvedDoStmt :: SourceSpan -> Annotation -> ResolutionAnnotation -> DoStmt Expr -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferResolvedDoStmt ambient resolutionAnn resolution stmt rest =
  case stmt of
    DoBind pat action -> do
      itemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      patCheck <- checkPattern ambient pat itemTy
      (rest', restTy, restCts) <-
        withPatternBindings (pcBindings patCheck) (inferDoStmts ambient rest)
      blockTy <- freshMetaTv
      let bindTy = TcFunTy actionTy (TcFunTy (TcFunTy itemTy restTy) blockTy)
      (pending, methodCts) <- inferDoMethod ambient ">>=" resolution bindTy
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
          stmt' = DoAnn (mkAnnotation pending) (DoAnn resolutionAnn (DoBind pat' action'))
      remainingCts <- solvePatternBranch ambient patCheck restTy restCts
      pure (stmt' : rest', blockTy, actionCts <> remainingCts <> methodCts)
    DoExpr action -> do
      (action', actionTy, actionCts) <- inferExprAt ambient action
      (rest', restTy, restCts) <- inferDoStmts ambient rest
      blockTy <- freshMetaTv
      let thenTy = TcFunTy actionTy (TcFunTy restTy blockTy)
      (pending, methodCts) <- inferDoMethod ambient ">>" resolution thenTy
      let stmt' = DoAnn (mkAnnotation pending) (DoAnn resolutionAnn (DoExpr action'))
      pure (stmt' : rest', blockTy, actionCts <> restCts <> methodCts)
    _ -> do
      emitError ambient (OtherError "internal do-bind annotation on a non-action statement")
      inferDoStmt ambient stmt rest

-- | The constraint that equates the argument of a literal method with the
-- resolved type of the literal.
--
-- The list is empty when the built-in scope does not give that type.
resolvedLiteralCts :: SourceSpan -> ResolutionAnnotation -> TcType -> TcM [Ct]
resolvedLiteralCts sp literalResolution argumentTy = do
  maybeInfo <- lookupResolvedTypeSyntax literalResolution
  case maybeInfo of
    Nothing -> pure []
    Just info -> do
      ev <- freshEvVar
      pure [mkWantedCt (EqPred argumentTy (TcTyCon (tciTyCon info) [])) ev (LitOrigin sp) sp]

-- | Instantiate the method that sequences a do statement and equate it with the expected type.
inferDoMethod :: SourceSpan -> Text -> ResolutionAnnotation -> TcType -> TcM (PendingTcAnnotation, [Ct])
inferDoMethod sp methodName resolution expectedTy = do
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp methodName resolution
  ev <- freshEvVar
  let methodEq = mkWantedCt (EqPred methodTy expectedTy) ev (OccurrenceOf methodName) sp
      pending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
  pure (pending, methodCts <> [methodEq])

wantedDoEq :: SourceSpan -> TcType -> TcType -> TcM Ct
wantedDoEq sp actual expected = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred actual expected) ev (AppOrigin sp) sp)

wantedMonad :: SourceSpan -> TcType -> TcM Ct
wantedMonad sp monadTy = do
  ev <- freshEvVar
  maybeMonad <- lookupTyCon "Monad"
  case maybeMonad of
    Just monadInfo -> pure (mkWantedCt (ClassPred (tciTyCon monadInfo) [monadTy]) ev (AppOrigin sp) sp)
    Nothing -> abortTc "missing checked type constructor for Monad"

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sp _ = sp

compStmtSpan :: CompStmt -> SourceSpan
compStmtSpan compStmt =
  case compStmt of
    CompAnn ann _ -> fromMaybe NoSourceSpan (fromAnnotation @SourceSpan ann)
    _ -> NoSourceSpan

doStmtSpan :: DoStmt body -> SourceSpan
doStmtSpan stmt =
  case stmt of
    DoAnn ann _ -> fromMaybe NoSourceSpan (fromAnnotation @SourceSpan ann)
    _ -> NoSourceSpan

rhsExprSpan :: Rhs Expr -> SourceSpan
rhsExprSpan rhs =
  case rhs of
    UnguardedRhs anns expr _ -> exprSpan expr `orSourceSpan` sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

exprSpan :: Expr -> SourceSpan
exprSpan expr =
  case expr of
    EAnn ann inner ->
      fromMaybe (exprSpan inner) (fromAnnotation @SourceSpan ann)
    _ -> NoSourceSpan

nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

resolvedType :: Text -> TcM TcType
resolvedType name = do
  maybeInfo <- lookupTyCon name
  tyCon <- maybe (mkKnownTyCon "GHC.Types" name 0 typeKindType) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [])

stringTyCon :: TcM TcType
stringTyCon = do
  listTyCon <- resolvedListTyCon
  charType <- resolvedType "Char"
  pure (TcTyCon listTyCon [charType])
