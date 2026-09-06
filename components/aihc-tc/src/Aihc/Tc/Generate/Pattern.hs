{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared type-checking support for term patterns.
module Aihc.Tc.Generate.Pattern
  ( PatternCheck (..),
    annotatePatternBindings,
    checkPattern,
    checkPatterns,
    checkPatternsWithGivens,
    checkFunctionPatterns,
    checkFunctionPatternsWithGivens,
    checkedPattern,
    patternBinderNames,
    withPatternBindings,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BuiltinCon (..),
    Expr (..),
    FloatType (..),
    Literal (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    RecordField (..),
    SourceSpan (..),
    TupleFlavor (..),
    UnqualifiedName (..),
    fromAnnotation,
    mkAnnotation,
    nameText,
    peelLiteralAnn,
    peelPatternAnn,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Annotations (PendingTcAnnotation (..), TcAnnotation, pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (PatSynInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import {-# SOURCE #-} Aihc.Tc.Generate.Expr (inferExprAt)
import Aihc.Tc.Generate.Record (lookupRecordConstructor, orderRecordFields)
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | The variables that a pattern binds, in source order. Every pattern form
-- that can hold a sub-pattern is walked, so a tuple, list, record, view,
-- or signature pattern in a binding position lists the same binders as a
-- constructor pattern.
patternBinderNames :: Pattern -> [UnqualifiedName]
patternBinderNames pat =
  case pat of
    PAnn _ inner -> patternBinderNames inner
    PVar name -> [name]
    PTypeBinder _ -> []
    PTypeSyntax _ _ -> []
    PWildcard -> []
    PLit _ -> []
    PQuasiQuote _ _ -> []
    PTuple _ items -> concatMap patternBinderNames items
    PUnboxedSum _ _ inner -> patternBinderNames inner
    PList items -> concatMap patternBinderNames items
    PCon _ _ pats -> concatMap patternBinderNames pats
    PBuiltinCon _ _ pats -> concatMap patternBinderNames pats
    PInfix lhs _ rhs -> patternBinderNames lhs <> patternBinderNames rhs
    PView _ inner -> patternBinderNames inner
    PAs name inner -> name : patternBinderNames inner
    PStrict inner -> patternBinderNames inner
    PIrrefutable inner -> patternBinderNames inner
    PNegLit _ -> []
    PParen inner -> patternBinderNames inner
    PRecord _ fields _ -> concatMap (patternBinderNames . recordFieldValue) fields
    PTypeSig inner _ -> patternBinderNames inner
    PSplice _ -> []

data PatternCheck = PatternCheck
  { pcBindings :: ![(UnqualifiedName, TcType)],
    pcWantedCts :: ![Ct],
    pcGivenCts :: ![Ct],
    pcSkolems :: ![TyVarId],
    pcPatterns :: ![Pattern]
  }
  deriving (Show)

instance Semigroup PatternCheck where
  left <> right =
    PatternCheck
      { pcBindings = pcBindings left <> pcBindings right,
        pcWantedCts = pcWantedCts left <> pcWantedCts right,
        pcGivenCts = pcGivenCts left <> pcGivenCts right,
        pcSkolems = pcSkolems left <> pcSkolems right,
        pcPatterns = pcPatterns left <> pcPatterns right
      }

instance Monoid PatternCheck where
  mempty = PatternCheck [] [] [] [] []

data GadtHandling
  = GadtAsWanted
  | GadtAsGiven
  deriving (Eq)

checkPatterns :: SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatterns = checkPatternsWith GadtAsWanted

checkPatternsWithGivens :: SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatternsWithGivens = checkPatternsWith GadtAsGiven

checkFunctionPatterns :: SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkFunctionPatterns = checkFunctionPatternsWith GadtAsWanted

checkFunctionPatternsWithGivens :: SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkFunctionPatternsWithGivens = checkFunctionPatternsWith GadtAsGiven

checkFunctionPatternsWith :: GadtHandling -> SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkFunctionPatternsWith gadtHandling sp arguments = do
  mapM_ (checkFunctionArgument sp) arguments
  checkPatternsWith gadtHandling sp arguments

checkFunctionArgument :: SourceSpan -> (Pattern, TcType) -> TcM ()
checkFunctionArgument ambient (pat, ty) = do
  kind <- tcTypeKind ty
  case runtimeRepFromKind kind of
    Right representation
      | not (isFixedRuntimeRep representation) ->
          emitError
            (patternOwnSpan pat `orSourceSpan` ambient)
            (RepresentationPolymorphicFunctionArgument (functionArgumentName pat) ty)
    _ -> pure ()

functionArgumentName :: Pattern -> Text
functionArgumentName pat =
  case pat of
    PAnn _ inner -> functionArgumentName inner
    PVar name -> unqualifiedNameText name
    PParen inner -> functionArgumentName inner
    PAs name _ -> unqualifiedNameText name
    PStrict inner -> functionArgumentName inner
    PIrrefutable inner -> functionArgumentName inner
    PTypeSig inner _ -> functionArgumentName inner
    _ -> "<pattern>"

checkPatternsWith :: GadtHandling -> SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatternsWith gadtHandling sp = fmap mconcat . mapM (uncurry (checkPatternWith gadtHandling sp))

checkPattern :: SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPattern = checkPatternWith GadtAsWanted

checkPatternWith :: GadtHandling -> SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternWith gadtHandling sp pat scrutTy = do
  check <- case literalPatternCheck sp pat scrutTy of
    Just literalCheck -> literalCheck
    Nothing -> checkPatternCore gadtHandling sp pat scrutTy
  pure check {pcPatterns = map (checkedPatternType sp scrutTy) (pcPatterns check)}

checkPatternWithoutResultType :: GadtHandling -> SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternWithoutResultType gadtHandling sp pat scrutTy =
  case literalPatternCheck sp pat scrutTy of
    Just literalCheck -> literalCheck
    Nothing ->
      case pat of
        PAnn ann inner -> do
          innerCheck <- checkPatternWithoutResultType gadtHandling sp inner scrutTy
          pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
        PParen inner -> do
          innerCheck <- checkPatternWithoutResultType gadtHandling sp inner scrutTy
          pure innerCheck {pcPatterns = [PParen (checkedPattern innerCheck)]}
        PStrict inner -> do
          innerCheck <- checkPatternWithoutResultType gadtHandling sp inner scrutTy
          pure innerCheck {pcPatterns = [PStrict (checkedPattern innerCheck)]}
        PIrrefutable inner -> do
          innerCheck <- checkPatternWithoutResultType gadtHandling sp inner scrutTy
          pure innerCheck {pcPatterns = [PIrrefutable (checkedPattern innerCheck)]}
        _ -> checkPatternCore gadtHandling sp pat scrutTy

checkedPatternType :: SourceSpan -> TcType -> Pattern -> Pattern
checkedPatternType sp ty pat
  | patternUsesBinderAnnotation pat = pat
  | not (patternNeedsCheckedType pat) = pat
  | patternHasPendingType pat = pat
  | otherwise = annotatePendingPatternAt (patternOwnSpan pat `orSourceSpan` sp) (pendingAnnotation ty [] [] []) pat

annotatePendingPatternAt :: SourceSpan -> PendingTcAnnotation -> Pattern -> Pattern
annotatePendingPatternAt NoSourceSpan pending = PAnn (mkAnnotation pending)
annotatePendingPatternAt sp pending = PAnn (mkAnnotation sp) . PAnn (mkAnnotation pending)

patternUsesBinderAnnotation :: Pattern -> Bool
patternUsesBinderAnnotation pat =
  case pat of
    PAnn _ inner -> patternUsesBinderAnnotation inner
    PParen inner -> patternUsesBinderAnnotation inner
    PVar {} -> True
    PAs {} -> True
    PStrict inner -> patternUsesBinderAnnotation inner
    PIrrefutable inner -> patternUsesBinderAnnotation inner
    PTypeSig inner _ -> patternUsesBinderAnnotation inner
    _ -> False

patternNeedsCheckedType :: Pattern -> Bool
patternNeedsCheckedType pat =
  case pat of
    PAnn _ inner -> patternNeedsCheckedType inner
    PParen inner -> patternNeedsCheckedType inner
    PLit {} -> False
    PNegLit {} -> False
    PStrict inner -> patternNeedsCheckedType inner
    PIrrefutable inner -> patternNeedsCheckedType inner
    PTypeSig inner _ -> patternNeedsCheckedType inner
    _ -> True

patternHasPendingType :: Pattern -> Bool
patternHasPendingType pat =
  case pat of
    PAnn ann inner -> annotationHasType ann || patternHasPendingType inner
    PParen inner -> patternHasPendingType inner
    PLit literal -> literalHasPendingType literal
    PStrict inner -> patternHasPendingType inner
    PIrrefutable inner -> patternHasPendingType inner
    PTypeSig inner _ -> patternHasPendingType inner
    _ -> False

literalHasPendingType :: Literal -> Bool
literalHasPendingType literal =
  case literal of
    LitAnn ann inner -> annotationHasType ann || literalHasPendingType inner
    _ -> False

annotationIsPending :: Annotation -> Bool
annotationIsPending ann =
  case fromAnnotation ann :: Maybe PendingTcAnnotation of
    Just _ -> True
    Nothing -> False

annotationHasType :: Annotation -> Bool
annotationHasType ann =
  annotationIsPending ann
    || case fromAnnotation ann :: Maybe TcAnnotation of
      Just _ -> True
      Nothing -> False

sourceSpanFromAnnotations :: [Annotation] -> SourceSpan
sourceSpanFromAnnotations annotations =
  case mapMaybe fromAnnotation annotations of
    sourceSpan : _ -> sourceSpan
    [] -> NoSourceSpan

patternOwnSpan :: Pattern -> SourceSpan
patternOwnSpan pat =
  case pat of
    PAnn ann inner -> fromMaybe (patternOwnSpan inner) (fromAnnotation ann)
    PVar name -> sourceSpanFromAnnotations (unqualifiedNameAnns name)
    PParen inner -> patternOwnSpan inner
    PAs name _ -> sourceSpanFromAnnotations (unqualifiedNameAnns name)
    PStrict inner -> patternOwnSpan inner
    PIrrefutable inner -> patternOwnSpan inner
    PCon name _ _ -> sourceSpanFromAnnotations (nameAnns name)
    PInfix _ name _ -> sourceSpanFromAnnotations (nameAnns name)
    PRecord name _ _ -> sourceSpanFromAnnotations (nameAnns name)
    PTypeSig inner _ -> patternOwnSpan inner
    PView expr inner -> viewExprSpan expr `orSourceSpan` patternOwnSpan inner
    _ -> NoSourceSpan

-- | The span of a view pattern function. The parser gives spans to names
-- and to annotated expressions only.
viewExprSpan :: Expr -> SourceSpan
viewExprSpan expr =
  case expr of
    EAnn ann inner -> fromMaybe (viewExprSpan inner) (fromAnnotation ann)
    EVar name -> sourceSpanFromAnnotations (nameAnns name)
    EParen inner -> viewExprSpan inner
    EPragma _ inner -> viewExprSpan inner
    EApp function _ -> viewExprSpan function
    _ -> NoSourceSpan

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sourceSpan _ = sourceSpan

checkPatternCore :: GadtHandling -> SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternCore gadtHandling sp pat scrutTy =
  case pat of
    PVar name ->
      pure (checkedOnly pat) {pcBindings = [(name, scrutTy)]}
    PAnn ann inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
    PParen inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PParen (checkedPattern innerCheck)]}
    PWildcard {} -> pure (checkedOnly pat)
    PLit lit
      | isPrimitiveLiteral lit ->
          abortTc "primitive literal pattern is missing its resolver type annotation"
      | otherwise -> do
          maybeLiteralTy <- charLiteralPatternType lit
          case maybeLiteralTy of
            Just literalTy -> do
              eqCt <- wantedEq sp scrutTy literalTy
              pure (checkedOnly (PLit (checkedLiteral scrutTy lit))) {pcWantedCts = [eqCt]}
            Nothing -> pure (checkedOnly (PLit (checkedLiteral scrutTy lit)))
    PNegLit lit
      | isPrimitiveLiteral lit ->
          abortTc "primitive literal pattern is missing its resolver type annotation"
      | otherwise -> pure (checkedOnly pat)
    PAs name inner -> do
      let innerSpan = patternOwnSpan inner `orSourceSpan` sp
      innerCheck <- checkPatternWithoutResultType gadtHandling innerSpan inner scrutTy
      pure innerCheck {pcBindings = (name, scrutTy) : pcBindings innerCheck, pcPatterns = [PAs name (checkedPattern innerCheck)]}
    PStrict inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PStrict (checkedPattern innerCheck)]}
    PIrrefutable inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PIrrefutable (checkedPattern innerCheck)]}
    PCon name _typeArgs subPats ->
      checkConPattern gadtHandling sp pat name subPats scrutTy
    PInfix lhs op rhs ->
      checkConPattern gadtHandling sp pat op [lhs, rhs] scrutTy
    PRecord name fields wildcard -> do
      when wildcard $
        abortTc ("record wildcard patterns are not supported at " <> show (patternOwnSpan pat `orSourceSpan` sp))
      con <- lookupRecordConstructor name
      subPats <- orderRecordFields (patternOwnSpan pat `orSourceSpan` sp) con fields (\_ -> pure PWildcard)
      checkConPattern gadtHandling sp (PCon name [] subPats) name subPats scrutTy
    PList items -> checkListPattern gadtHandling sp items scrutTy
    PView viewExpr inner -> do
      let viewSpan = viewExprSpan viewExpr `orSourceSpan` sp
      (viewExpr', viewTy, viewCts) <- inferExprAt viewSpan viewExpr
      innerTy <- freshMetaTv
      eqCt <- wantedEq viewSpan viewTy (TcFunTy scrutTy innerTy)
      innerCheck <- checkPatternWith gadtHandling sp inner innerTy
      pure
        innerCheck
          { pcWantedCts = eqCt : viewCts <> pcWantedCts innerCheck,
            pcPatterns = [PView viewExpr' (checkedPattern innerCheck)]
          }
    PTuple flavor items -> checkTuplePattern gadtHandling sp flavor items scrutTy
    -- A prefix tuple constructor, @(,) a b@, checks like the @(a, b)@ form.
    -- The type arguments follow 'PCon', which ignores them.
    PBuiltinCon (BuiltinTuple flavor arity) _typeArgs items
      | length items == arity ->
          checkTuplePattern gadtHandling sp flavor items scrutTy
    _ -> pure (checkedOnly pat)

checkTuplePattern :: GadtHandling -> SourceSpan -> TupleFlavor -> [Pattern] -> TcType -> TcM PatternCheck
checkTuplePattern gadtHandling sp flavor items scrutTy = do
  elemTys <- mapM (const freshMetaTv) items
  let arity = length items
      typeName = tupleTyConText flavor arity
  maybeTyCon <- lookupTyCon typeName
  tupleTyCon <-
    case maybeTyCon of
      Just info -> pure (tciTyCon info)
      Nothing -> abortTc ("tuple pattern needs the type constructor " <> T.unpack typeName <> ", which is not in scope")
  let tupleTy = TcTyCon tupleTyCon elemTys
  eqCt <- wantedEq sp scrutTy tupleTy
  itemChecks <- checkPatternsWith gadtHandling sp (zip items elemTys)
  pure itemChecks {pcWantedCts = eqCt : pcWantedCts itemChecks, pcPatterns = [PTuple flavor (pcPatterns itemChecks)]}

checkedOnly :: Pattern -> PatternCheck
checkedOnly pat = mempty {pcPatterns = [pat]}

checkedLiteral :: TcType -> Literal -> Literal
checkedLiteral ty = LitAnn (mkAnnotation (pendingAnnotation ty [] [] []))

checkListPattern :: GadtHandling -> SourceSpan -> [Pattern] -> TcType -> TcM PatternCheck
checkListPattern gadtHandling sp items scrutTy =
  case items of
    [] -> do
      scheme <- listConstructorScheme "[]"
      (nilTy, _typeArgs, predicates, skolems) <- instantiateConstructorPattern scheme
      scrutCts <- constructorScrutineeCt gadtHandling sp "[]" scrutTy nilTy
      predicateGivens <- mapM (constructorGiven sp "[]") predicates
      pure
        mempty
          { pcWantedCts = fst scrutCts,
            pcGivenCts = predicateGivens <> snd scrutCts,
            pcSkolems = skolems,
            pcPatterns = [PList []]
          }
    item : rest -> do
      scheme <- listConstructorScheme ":"
      (consTy, _typeArgs, predicates, skolems) <- instantiateConstructorPattern scheme
      (argumentTypes, resultTy) <- splitConTy 2 consTy
      case argumentTypes of
        [itemTy, tailTy] -> do
          scrutCts <- constructorScrutineeCt gadtHandling sp ":" scrutTy resultTy
          itemCheck <- checkPatternWith gadtHandling sp item itemTy
          tailCheck <- checkListPattern gadtHandling sp rest tailTy
          predicateGivens <- mapM (constructorGiven sp ":") predicates
          let nestedCheck = itemCheck <> tailCheck
              checkedTailItems = case checkedPattern tailCheck of
                PAnn _ (PList patterns) -> patterns
                PList patterns -> patterns
                _ -> rest
              checkedItems = checkedPattern itemCheck : checkedTailItems
          pure
            nestedCheck
              { pcWantedCts = fst scrutCts <> pcWantedCts nestedCheck,
                pcGivenCts = predicateGivens <> snd scrutCts <> pcGivenCts nestedCheck,
                pcSkolems = skolems <> pcSkolems nestedCheck,
                pcPatterns = [PList checkedItems]
              }
        _ -> abortTc "GHC.Types list cons constructor has an invalid arity"

listConstructorScheme :: Text -> TcM TypeScheme
listConstructorScheme name = do
  sourceBinder <- lookupTerm name
  maybeBinder <- maybe (lookupKnownTerm "GHC.Types" name) (pure . Just) sourceBinder
  case maybeBinder of
    Just (TcIdBinder scheme _) -> pure scheme
    Just TcMonoIdBinder {} -> abortTc ("GHC.Types list constructor is monomorphic: " <> T.unpack name)
    Nothing -> abortTc ("GHC.Types list constructor is missing: " <> T.unpack name)

checkedPattern :: PatternCheck -> Pattern
checkedPattern check =
  case pcPatterns check of
    [pat] -> pat
    _ -> error "checkedPattern: expected exactly one checked pattern"

charLiteralPatternType :: Literal -> TcM (Maybe TcType)
charLiteralPatternType literal =
  case peelLiteralAnn literal of
    LitChar {} -> do
      maybeInfo <- lookupTyCon "Char"
      tyCon <- maybe (mkKnownTyCon "GHC.Types" "Char" 0 typeKindType) (pure . tciTyCon) maybeInfo
      pure (Just (TcTyCon tyCon []))
    _ -> pure Nothing

-- | The check of a literal pattern that needs its resolver annotations.
--
-- An overloaded integer pattern uses the resolved syntax terms.
-- A primitive literal pattern uses the resolved primitive type.
literalPatternCheck :: SourceSpan -> Pattern -> TcType -> Maybe (TcM PatternCheck)
literalPatternCheck sp pat scrutTy =
  case patternLiteral pat of
    Just (isNegative, lit)
      | isOverloadedIntegerLiteral lit -> Just (checkOverloadedIntegerPattern sp pat isNegative scrutTy)
      | isOverloadedFractionalLiteral lit -> Just (checkOverloadedLiteralPattern sp pat "fromRational" Nothing isNegative scrutTy)
      | isPrimitiveLiteral lit -> Just (checkPrimitiveLiteralPattern sp pat scrutTy)
    _ -> Nothing

-- | The literal of a literal pattern, with a flag for a negated literal.
patternLiteral :: Pattern -> Maybe (Bool, Literal)
patternLiteral pat =
  case peelPatternAnn pat of
    PLit lit -> Just (False, lit)
    PNegLit lit -> Just (True, lit)
    _ -> Nothing

isPrimitiveLiteral :: Literal -> Bool
isPrimitiveLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ numericType _ -> numericType /= TInteger
    LitFloat _ floatType _ -> floatType /= TFractional
    LitCharHash {} -> True
    LitStringHash {} -> True
    _ -> False

-- | Check a primitive literal pattern against the scrutinee type.
--
-- The resolver annotates the pattern with the primitive type of the literal.
-- The pattern type is that primitive type, so the scrutinee must equal it.
checkPrimitiveLiteralPattern :: SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPrimitiveLiteralPattern sp pat scrutTy = do
  resolution <- requiredPrimitiveLiteralResolution pat
  maybeInfo <- lookupResolvedTypeSyntax resolution
  info <-
    maybe
      (abortTc ("resolved primitive literal type missing from type environment: " <> show (resolutionTarget resolution)))
      pure
      maybeInfo
  let literalTy = TcTyCon (tciTyCon info) []
  eqCt <- wantedEq sp scrutTy literalTy
  pure (checkedOnly (checkedLiteralPattern scrutTy pat)) {pcWantedCts = [eqCt]}

-- | The Integer type that the resolver gives an overloaded integer pattern.
--
-- The result is 'Nothing' when the built-in scope does not give the type.
resolvedIntegerPatternType :: Pattern -> TcM (Maybe TcType)
resolvedIntegerPatternType pat =
  case [resolution | resolution <- patternResolutions pat, resolutionNamespace resolution == ResolutionNamespaceType] of
    resolution : _ -> fmap (\info -> TcTyCon (tciTyCon info) []) <$> lookupResolvedTypeSyntax resolution
    [] -> pure Nothing

requiredPrimitiveLiteralResolution :: Pattern -> TcM ResolutionAnnotation
requiredPrimitiveLiteralResolution pat =
  case [resolution | resolution <- patternResolutions pat, resolutionNamespace resolution == ResolutionNamespaceType] of
    resolution : _ -> pure resolution
    [] -> abortTc "primitive literal pattern is missing its resolver type annotation"

-- | Attach the checked type to the literal inside a literal pattern.
checkedLiteralPattern :: TcType -> Pattern -> Pattern
checkedLiteralPattern ty pat =
  case pat of
    PAnn ann inner -> PAnn ann (checkedLiteralPattern ty inner)
    PParen inner -> PParen (checkedLiteralPattern ty inner)
    PStrict inner -> PStrict (checkedLiteralPattern ty inner)
    PIrrefutable inner -> PIrrefutable (checkedLiteralPattern ty inner)
    PLit lit -> PLit (checkedLiteral ty lit)
    PNegLit lit -> PNegLit (checkedLiteral ty lit)
    _ -> pat

isOverloadedIntegerLiteral :: Literal -> Bool
isOverloadedIntegerLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ TInteger _ -> True
    _ -> False

isOverloadedFractionalLiteral :: Literal -> Bool
isOverloadedFractionalLiteral lit =
  case peelLiteralAnn lit of
    LitFloat _ TFractional _ -> True
    _ -> False

checkOverloadedIntegerPattern :: SourceSpan -> Pattern -> Bool -> TcType -> TcM PatternCheck
checkOverloadedIntegerPattern sp pat isNegative scrutTy = do
  integerTy <- resolvedIntegerPatternType pat
  checkOverloadedLiteralPattern sp pat "fromInteger" integerTy isNegative scrutTy

-- | Check a literal pattern that a class method converts and equality tests.
--
-- @literalTy@ is the resolved type of the literal, when the resolver gives
-- it. The argument type of the conversion method must then equal it.
checkOverloadedLiteralPattern :: SourceSpan -> Pattern -> Text -> Maybe TcType -> Bool -> TcType -> TcM PatternCheck
checkOverloadedLiteralPattern sp pat conversion literalTy isNegative scrutTy = do
  (conversionPending, conversionCts) <-
    checkPatternMethodWithExpected sp pat conversion $ \case
      TcFunTy argumentTy _ -> pure (scrutTy, TcFunTy (fromMaybe argumentTy literalTy) scrutTy)
      _ -> abortTc (T.unpack conversion <> " does not have a function type")
  negateCheck <-
    if isNegative
      then Just <$> checkPatternMethod sp pat "negate" scrutTy (TcFunTy scrutTy scrutTy)
      else pure Nothing
  (eqPending, eqCts) <-
    checkPatternMethodWithExpected sp pat "==" $ \case
      TcFunTy _ (TcFunTy _ boolTy) ->
        let expectedTy = TcFunTy scrutTy (TcFunTy scrutTy boolTy)
         in pure (expectedTy, expectedTy)
      _ -> abortTc "== does not have a binary function type"
  let methodAnnotations =
        [(conversion, conversionPending)]
          <> maybe [] (\(pending, _) -> [("negate", pending)]) negateCheck
          <> [("==", eqPending)]
      pat' = foldr (uncurry attachPendingPatternAnnotation) pat methodAnnotations
      negateCts = maybe [] snd negateCheck
  pure
    PatternCheck
      { pcBindings = [],
        pcWantedCts = conversionCts <> negateCts <> eqCts,
        pcGivenCts = [],
        pcSkolems = [],
        pcPatterns = [pat']
      }

checkPatternMethod :: SourceSpan -> Pattern -> Text -> TcType -> TcType -> TcM (PendingTcAnnotation, [Ct])
checkPatternMethod sp pat name annotationTy expectedTy =
  checkPatternMethodWithExpected sp pat name (const (pure (annotationTy, expectedTy)))

checkPatternMethodWithExpected :: SourceSpan -> Pattern -> Text -> (TcType -> TcM (TcType, TcType)) -> TcM (PendingTcAnnotation, [Ct])
checkPatternMethodWithExpected sp pat name expectedTypes = do
  resolution <- requiredPatternResolution name pat
  (actualTy, typeArgs, methodCts) <- inferResolvedPatternMethod sp name resolution
  (annotationTy, expectedTy) <- expectedTypes actualTy
  methodEq <- wantedMethodEq sp name actualTy expectedTy
  pure
    ( pendingAnnotation
        annotationTy
        typeArgs
        (map ctEvVar methodCts)
        [],
      methodCts <> [methodEq]
    )

wantedMethodEq :: SourceSpan -> Text -> TcType -> TcType -> TcM Ct
wantedMethodEq sp method actual expected = do
  ev <- freshEvVar
  pure $
    mkWantedEqCt
      TypeTrace
        { typeTraceType = actual,
          typeTraceRole = ActualType,
          typeTraceOrigin = ConstraintTypeOrigin (OccurrenceOf method)
        }
      TypeTrace
        { typeTraceType = expected,
          typeTraceRole = ExpectedType,
          typeTraceOrigin = ConstraintTypeOrigin (LitOrigin sp)
        }
      ev
      (LitOrigin sp)
      sp

inferResolvedPatternMethod :: SourceSpan -> Text -> ResolutionAnnotation -> TcM (TcType, [TcType], [Ct])
inferResolvedPatternMethod sp displayName resolution = do
  mBinder <- lookupResolvedTerm displayName (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp displayName) (instPreds inst)
      pure (instType inst, instTypeArgs inst, cts)
    Just (TcMonoIdBinder ty) ->
      pure (ty, [], [])
    Nothing ->
      abortTc ("resolved " <> T.unpack displayName <> " missing from type environment: " <> show (resolutionTarget resolution))

predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name pred' = do
  ev <- freshEvVar
  pure (mkWantedCt pred' ev (OccurrenceOf name) sp)

requiredPatternResolution :: Text -> Pattern -> TcM ResolutionAnnotation
requiredPatternResolution name pat =
  case [resolution | resolution <- patternResolutions pat, resolutionIdentifier resolution == IdentifierNamed name, resolutionNamespace resolution == ResolutionNamespaceTerm] of
    resolution : _ -> pure resolution
    [] -> do
      emitError NoSourceSpan (OtherError ("missing resolver annotation for overloaded pattern method " <> T.unpack name))
      abortTc ("missing resolver annotation for overloaded pattern method " <> T.unpack name)

patternResolutions :: Pattern -> [ResolutionAnnotation]
patternResolutions pat =
  case pat of
    PAnn ann inner -> mapMaybe fromAnnotation [ann] <> patternResolutions inner
    PParen inner -> patternResolutions inner
    PStrict inner -> patternResolutions inner
    PIrrefutable inner -> patternResolutions inner
    PAs _ inner -> patternResolutions inner
    PTypeSig inner _ -> patternResolutions inner
    _ -> []

attachPendingPatternAnnotation :: Text -> PendingTcAnnotation -> Pattern -> Pattern
attachPendingPatternAnnotation target pending pat =
  case pat of
    PAnn ann inner ->
      case fromAnnotation ann of
        Just resolution
          | resolutionIdentifier resolution == IdentifierNamed target,
            resolutionNamespace resolution == ResolutionNamespaceTerm ->
              PAnn (mkAnnotation pending) (PAnn ann inner)
        _ -> PAnn ann (attachPendingPatternAnnotation target pending inner)
    PParen inner -> PParen (attachPendingPatternAnnotation target pending inner)
    PStrict inner -> PStrict (attachPendingPatternAnnotation target pending inner)
    PIrrefutable inner -> PIrrefutable (attachPendingPatternAnnotation target pending inner)
    PAs name inner -> PAs name (attachPendingPatternAnnotation target pending inner)
    PTypeSig inner ty -> PTypeSig (attachPendingPatternAnnotation target pending inner) ty
    _ -> pat

annotatePatternBindings :: [(UnqualifiedName, TcType)] -> Pattern -> Pattern
annotatePatternBindings bindings =
  go
  where
    go pat =
      case pat of
        PAnn ann inner -> PAnn ann (go inner)
        PVar name -> PVar (annotateBinderName bindings name)
        PParen inner -> PParen (go inner)
        PAs name inner -> PAs (annotateBinderName bindings name) (go inner)
        PStrict inner -> PStrict (go inner)
        PIrrefutable inner -> PIrrefutable (go inner)
        PList items -> PList (map go items)
        PTuple flavor items -> PTuple flavor (map go items)
        PUnboxedSum alt arity inner -> PUnboxedSum alt arity (go inner)
        PInfix lhs op rhs -> PInfix (go lhs) op (go rhs)
        PView expr inner -> PView expr (go inner)
        PCon name typeArgs subPats -> PCon name typeArgs (map go subPats)
        PRecord name fields wildcard -> PRecord name (map annotateRecordField fields) wildcard
        PTypeSig inner type' -> PTypeSig (go inner) type'
        PSplice expr -> PSplice expr
        _ -> pat

    annotateRecordField :: RecordField Pattern -> RecordField Pattern
    annotateRecordField field =
      field {recordFieldValue = go (recordFieldValue field)}

annotateBinderName :: [(UnqualifiedName, TcType)] -> UnqualifiedName -> UnqualifiedName
annotateBinderName bindings name =
  case lookup name bindings of
    Nothing -> name
    Just ty
      | any annotationIsPending (unqualifiedNameAnns name) -> name
      | otherwise -> name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation (pendingAnnotation ty [] [] [])]}

checkConPattern :: GadtHandling -> SourceSpan -> Pattern -> Name -> [Pattern] -> TcType -> TcM PatternCheck
checkConPattern gadtHandling sp originalPat conSyntax subPats scrutTy = do
  let conName = patternNameText conSyntax
  target <- resolvedTermTarget conSyntax
  mBinder <- lookupResolvedTerm conName target
  mPatSyn <- lookupPatSynTarget target
  case mBinder of
    Just (TcIdBinder scheme _)
      | Just info <- mPatSyn ->
          checkPatSynPattern gadtHandling sp originalPat conName info scheme subPats scrutTy
    Just (TcIdBinder scheme _) -> do
      (conTy, typeArgs, predicates, skolems) <- instantiateConstructorPattern scheme
      (argTys, conResTy) <- splitConTy (length subPats) conTy
      scrutCt <- constructorScrutineeCt gadtHandling sp conName scrutTy conResTy
      subCheck <- checkPatternsWith gadtHandling sp (zip subPats argTys)
      predicateGivens <- mapM (constructorGiven sp conName) predicates
      let rebuiltPattern = replaceConstructorSubpatterns originalPat (pcPatterns subCheck)
          annotatedPattern
            | null predicateGivens && null skolems = rebuiltPattern
            | otherwise =
                PAnn
                  ( mkAnnotation
                      ( (pendingAnnotation conTy typeArgs (map ctEvVar predicateGivens) [])
                          { pendingTcAnnTypeBinders = skolems
                          }
                      )
                  )
                  rebuiltPattern
      pure
        subCheck
          { pcWantedCts = fst scrutCt <> pcWantedCts subCheck,
            pcGivenCts = predicateGivens <> snd scrutCt <> pcGivenCts subCheck,
            pcSkolems = skolems <> pcSkolems subCheck,
            pcPatterns = [annotatedPattern]
          }
    Just other ->
      abortTc ("resolved constructor is not an identifier binder: " <> show conName <> " resolved as " <> show target <> " with binder " <> show other)
    Nothing ->
      abortTc ("resolved constructor missing from type environment: " <> show conName <> " resolved as " <> show target)

-- | Check a pattern synonym use. The required predicates are wanted at
-- the use. The provided predicates are given to the branch. The annotation
-- records the type arguments, the required evidence and then the provided
-- evidence, and the existential skolems. The desugarer calls the matcher
-- with them.
checkPatSynPattern :: GadtHandling -> SourceSpan -> Pattern -> Text -> PatSynInfo -> TypeScheme -> [Pattern] -> TcType -> TcM PatternCheck
checkPatSynPattern gadtHandling sp originalPat conName info scheme subPats scrutTy = do
  when (length subPats /= psiArity info) $
    emitError sp (OtherError ("pattern synonym " <> T.unpack conName <> " takes " <> show (psiArity info) <> " arguments, but the pattern gives " <> show (length subPats)))
  (conTy, typeArgs, predicates, skolems) <- instantiateConstructorPattern scheme
  let (requiredPreds, providedPreds) = splitAt (length (psiReqTheta info)) predicates
  (argTys, conResTy) <- splitConTy (length subPats) conTy
  scrutCt <- constructorScrutineeCt gadtHandling sp conName scrutTy conResTy
  subCheck <- checkPatternsWith gadtHandling sp (zip subPats argTys)
  requiredCts <- mapM (predToCt sp conName) requiredPreds
  providedGivens <- mapM (constructorGiven sp conName) providedPreds
  let rebuiltPattern = replaceConstructorSubpatterns originalPat (pcPatterns subCheck)
      annotatedPattern =
        PAnn
          ( mkAnnotation
              ( (pendingAnnotation conTy typeArgs (map ctEvVar requiredCts <> map ctEvVar providedGivens) [])
                  { pendingTcAnnTypeBinders = skolems
                  }
              )
          )
          rebuiltPattern
  pure
    subCheck
      { pcWantedCts = fst scrutCt <> requiredCts <> pcWantedCts subCheck,
        pcGivenCts = providedGivens <> snd scrutCt <> pcGivenCts subCheck,
        pcSkolems = skolems <> pcSkolems subCheck,
        pcPatterns = [annotatedPattern]
      }

constructorGiven :: SourceSpan -> Text -> Pred -> TcM Ct
constructorGiven sp constructorName predicate = do
  evidence <- freshEvVar
  bindEvidence evidence (EvGiven predicate)
  let origin = OccurrenceOf constructorName
  pure
    Ct
      { ctPred = predicate,
        ctFlavor = Given,
        ctEvVar = evidence,
        ctOrigin = origin,
        ctProvenance = FromCtOrigin origin,
        ctLoc = sp
      }

instantiateConstructorPattern :: TypeScheme -> TcM (TcType, [TcType], [Pred], [TyVarId])
instantiateConstructorPattern (ForAll tyVars predicates body) = do
  let resultTyVars = constructorResultTyVars body
      isUniversal tyVar = tvUnique tyVar `Set.member` resultTyVars
  substitutions <- mapM (instantiateTyVar isUniversal) tyVars
  let substitution = Map.fromList [(tvUnique tyVar, ty) | (tyVar, ty, _) <- substitutions]
      instantiateType = applySubst substitution
      typeArgs = map (instantiateType . TcTyVar) tyVars
      skolems = [skolem | (_, _, Just skolem) <- substitutions]
  pure
    ( instantiateType body,
      typeArgs,
      map (applySubstPred substitution) predicates,
      skolems
    )
  where
    instantiateTyVar isUniversal tyVar
      | isUniversal tyVar = do
          meta <- freshMetaTv
          pure (tyVar, meta, Nothing)
      | otherwise = do
          skolem <- setTyVarKind (tvKind tyVar) <$> freshSkolemTv (tvName tyVar)
          pure (tyVar, TcTyVar skolem, Just skolem)

constructorResultTyVars :: TcType -> Set.Set Unique
constructorResultTyVars = typeTyVars . dropConstructorArguments
  where
    dropConstructorArguments (TcFunTy _ result) = dropConstructorArguments result
    dropConstructorArguments result = result

typeTyVars :: TcType -> Set.Set Unique
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> Set.singleton (tvUnique tyVar)
    TcMetaTv {} -> Set.empty
    TcTyCon _ arguments -> Set.unions (map typeTyVars arguments)
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> Set.delete (tvUnique tyVar) (typeTyVars body)
    TcQualTy predicates body -> Set.unions (typeTyVars body : map predTyVars predicates)
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

predTyVars :: Pred -> Set.Set Unique
predTyVars predicate =
  case predicate of
    ClassPred _ arguments -> Set.unions (map typeTyVars arguments)
    EqPred left right -> typeTyVars left <> typeTyVars right
    IParamPred _ payload -> typeTyVars payload
    QuantifiedPred variables antecedents consequent ->
      foldr
        (Set.delete . tvUnique)
        (Set.unions (predTyVars consequent : map predTyVars antecedents))
        variables

replaceConstructorSubpatterns :: Pattern -> [Pattern] -> Pattern
replaceConstructorSubpatterns pat subPats =
  case pat of
    PCon name typeArgs _ -> PCon name typeArgs subPats
    PInfix _ op _ ->
      case subPats of
        [lhs, rhs] -> PInfix lhs op rhs
        _ -> pat
    _ -> pat

constructorScrutineeCt :: GadtHandling -> SourceSpan -> Text -> TcType -> TcType -> TcM ([Ct], [Ct])
constructorScrutineeCt gadtHandling sp conName scrutTy conResTy = do
  ev <- freshEvVar
  gadtCon <- isGadtCon conName
  if gadtHandling == GadtAsGiven && gadtCon
    then
      pure
        ( [],
          [ Ct
              { ctPred = EqPred scrutTy conResTy,
                ctFlavor = Given,
                ctEvVar = ev,
                ctOrigin = AppOrigin sp,
                ctProvenance = FromCtOrigin (AppOrigin sp),
                ctLoc = sp
              }
          ]
        )
    else do
      let wantedCt = mkWantedCt (EqPred scrutTy conResTy) ev (AppOrigin sp) sp
      pure ([wantedCt], [])

splitConTy :: Int -> TcType -> TcM ([TcType], TcType)
splitConTy 0 ty = pure ([], ty)
splitConTy n (TcFunTy arg rest) = do
  (args, result) <- splitConTy (n - 1) rest
  pure (arg : args, result)
splitConTy n result = do
  missingArgs <- mapM (const freshMetaTv) [1 .. n]
  pure (missingArgs, result)

wantedEq :: SourceSpan -> TcType -> TcType -> TcM Ct
wantedEq sp left right = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred left right) ev (AppOrigin sp) sp)

withPatternBindings :: [(UnqualifiedName, TcType)] -> TcM a -> TcM a
withPatternBindings [] action = action
withPatternBindings ((name, ty) : rest) action =
  extendResolvedTermEnv name (TcMonoIdBinder ty) (withPatternBindings rest action)

tupleTyConText :: TupleFlavor -> Int -> Text
tupleTyConText flavor arity =
  case flavor of
    Boxed -> boxedTupleTyConName arity
    Unboxed -> unboxedTupleTyConName arity

patternNameText :: Name -> Text
patternNameText name =
  case nameQualifier name of
    Nothing -> nameText name
    Just qualifier -> qualifier <> "." <> nameText name
