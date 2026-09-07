{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Human-readable inline rendering for type-checker annotations.
module TcAnnotatedRender
  ( renderAnnotatedTcResults,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    Module,
    fromAnnotation,
    moduleName,
  )
import Aihc.Tc (TypeFamilyInstanceInfo (..), renderTcSignature, renderTcType, renderTcTypeInModule)
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
  )
import Aihc.Tc.Constraint (CtOrigin (..), EqProvenance (..), TypeOrigin (..), TypeRole (..), TypeTrace (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Evidence (CallSite (..), Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Tc.Types
import Aihc.Testing.AnnotatedModule (renderAnnotatedModuleSources)
import Control.Applicative ((<|>))
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, pretty)

renderAnnotatedTcResults :: [Text] -> [Module] -> [String]
renderAnnotatedTcResults sources results =
  case compare (length sources) (length results) of
    LT -> error "renderAnnotatedTcResults: fewer source texts than modules"
    GT -> error "renderAnnotatedTcResults: more source texts than modules"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . snd) (zip sources results)
       in concatMap renderModule moduleSources
  where
    renderModule (source, modu) =
      renderAnnotatedModuleSources (renderTcAnnotation (moduleName modu)) [source] [modu]

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe "<unnamed>" (moduleName modu)

renderTcAnnotation :: Maybe Text -> Annotation -> Maybe (Doc ann)
renderTcAnnotation currentModule annotation =
  pretty
    <$> ( renderTypeAnnotation currentModule <$> fromAnnotation @TcAnnotation annotation
            <|> renderClassAnnotation <$> fromAnnotation @TcClassAnnotation annotation
            <|> renderDerivingAnnotation <$> fromAnnotation @TcDerivingAnnotation annotation
            <|> renderInstanceAnnotation <$> fromAnnotation @TcInstanceAnnotation annotation
            <|> renderInstanceMethodAnnotation <$> fromAnnotation @TcInstanceMethodAnnotation annotation
            <|> renderDiagnostic <$> fromAnnotation @TcDiagnostic annotation
        )

renderTypeAnnotation :: Maybe Text -> TcAnnotation -> String
renderTypeAnnotation currentModule ann =
  intercalate "; " ("type: " <> renderTcTypeInModule currentModule (tcAnnType ann) : renderElaboration ann)

renderClassAnnotation :: TcClassAnnotation -> String
renderClassAnnotation classAnnotation =
  "class methods:" <> case tcClassMethods classAnnotation of
    [] -> ""
    methods -> " " <> intercalate ", " (map renderClassMethod methods)

renderClassMethod :: TcClassMethodAnnotation -> String
renderClassMethod method =
  renderTcSignature (tcClassMethodName method) (tcClassMethodType method)

renderDerivingAnnotation :: TcDerivingAnnotation -> String
renderDerivingAnnotation annotation =
  "deriving plans: " <> intercalate ", " (map renderDerivingPlan (tcDerivingPlans annotation))

renderDerivingPlan :: TcDerivingPlan -> String
renderDerivingPlan plan =
  renderDerivingStrategy (tcDerivingStrategy plan)
    <> " "
    <> renderTcType (TcTyCon (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan))
    <> renderDerivingContext (tcDerivingContext plan)
    <> renderDerivingMethods (tcDerivingClassMethods plan)

renderDerivingStrategy :: TcDerivingStrategy -> String
renderDerivingStrategy strategy =
  case strategy of
    TcDerivingStock -> "stock"
    TcDerivingNewtype -> "newtype"
    TcDerivingAnyclass -> "anyclass"
    TcDerivingVia viaType -> "via " <> renderTcType viaType

renderDerivingContext :: TcDerivingContext -> String
renderDerivingContext context =
  case context of
    TcDerivingInferContext -> " [context: infer]"
    TcDerivingExplicitContext [] -> " [context: ()]"
    TcDerivingExplicitContext predicates -> " [context: " <> intercalate ", " (map renderPred predicates) <> "]"

renderDerivingMethods :: [TcClassMethodAnnotation] -> String
renderDerivingMethods [] = ""
renderDerivingMethods methods = " [methods: " <> intercalate ", " (map (T.unpack . tcClassMethodName) methods) <> "]"

renderInstanceAnnotation :: TcInstanceAnnotation -> String
renderInstanceAnnotation ann =
  renderTcSignature (tcInstanceDictName ann) (tcInstanceDictType ann)
    <> case tcInstanceSuperClasses ann of
      [] -> ""
      superClasses -> " [superclasses: " <> intercalate ", " (map (renderEvTerm . snd) superClasses) <> "]"
    <> case tcInstanceAssociatedTypes ann of
      [] -> ""
      equations -> " [associated types: " <> intercalate ", " (map renderTypeFamilyEquation equations) <> "]"

renderTypeFamilyEquation :: TypeFamilyInstanceInfo -> String
renderTypeFamilyEquation equation =
  renderTcType (tfiiLeft equation) <> " = " <> renderTcType (tfiiRight equation)

renderInstanceMethodAnnotation :: TcInstanceMethodAnnotation -> String
renderInstanceMethodAnnotation ann =
  renderTcSignature (tcInstanceMethodName ann) (tcInstanceMethodType ann)

renderElaboration :: TcAnnotation -> [String]
renderElaboration ann =
  typeArgs
    <> evidenceTerms
    <> termArgTypes
  where
    typeArgs =
      case tcAnnTypeArgs ann of
        [] -> []
        tys -> ["type-args: " <> intercalate ", " (map renderTcType tys)]
    evidenceTerms =
      case tcAnnEvidenceTerms ann of
        [] -> []
        evs -> ["evidence: " <> intercalate ", " (map renderEvTerm evs)]
    termArgTypes =
      case tcAnnTermArgTypes ann of
        [] -> []
        tys -> ["term-args: " <> intercalate ", " (map renderTcType tys)]

renderDiagnostic :: TcDiagnostic -> String
renderDiagnostic diagnostic =
  severityPrefix (diagSeverity diagnostic) <> ": " <> renderDiagnosticKind (diagKind diagnostic)

severityPrefix :: TcSeverity -> String
severityPrefix TcError = "error"
severityPrefix TcWarning = "warning"

renderDiagnosticKind :: TcErrorKind -> String
renderDiagnosticKind kind =
  case kind of
    UnificationError left right _ maybeProvenance ->
      case maybeProvenance of
        Just provenance ->
          renderTypeMismatch provenance
        Nothing ->
          "couldn't match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError variable ty ->
      "occurs check failed: " <> renderTcType variable <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted pred' _ ->
      "unsolved constraint " <> renderPred pred'
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      "function argument " <> T.unpack name <> " has type " <> renderTcType ty <> " without a fixed runtime representation"
    OtherError message ->
      message

renderTypeMismatch :: EqProvenance -> String
renderTypeMismatch provenance =
  typeRoleNoun (typeTraceRole actual)
    <> " has type "
    <> renderTcType (typeTraceType actual)
    <> ", but expected "
    <> renderTcType (typeTraceType expected)
    <> renderExpectedOrigin (typeTraceOrigin expected)
  where
    actual = eqActualTrace provenance
    expected = eqExpectedTrace provenance

renderExpectedOrigin :: TypeOrigin -> String
renderExpectedOrigin origin =
  case origin of
    ListElementTypeOrigin _ ->
      " from an earlier list element"
    TypeSignatureOrigin name _ ->
      " from the type signature for " <> T.unpack name
    ConstraintTypeOrigin ctOrigin ->
      " from " <> renderOrigin ctOrigin
    _ -> ""

typeRoleNoun :: TypeRole -> String
typeRoleNoun role =
  case role of
    ActualType -> "expression"
    ExpectedType -> "expected type"
    RequiredType -> "required type"
    InferredType -> "inferred type"

renderOrigin :: CtOrigin -> String
renderOrigin origin =
  case origin of
    OccurrenceOf name -> "the occurrence of " <> T.unpack name
    AppOrigin {} -> "an application"
    LambdaOrigin {} -> "a lambda expression"
    LetOrigin {} -> "a let binding"
    LitOrigin {} -> "a literal"
    SigOrigin {} -> "a type signature"
    CaseBranchOrigin {} -> "a case branch"
    InstOrigin name -> "the instance " <> T.unpack name
    UnifyOrigin {} -> "a unification constraint"
    ImplicitParamOrigin name -> "the implicit parameter " <> T.unpack name

renderEvTerm :: EvTerm -> String
renderEvTerm ev =
  case ev of
    EvVarTerm evVar -> renderEvVar evVar
    EvGiven pred' -> "given " <> renderPred pred'
    EvDict _ name typeArgs evidence ->
      T.unpack name
        <> renderTypeArgs typeArgs
        <> renderEvidenceArgs evidence
    EvCoercion coercion -> renderCoercion coercion
    EvSuperClass evidence _ _ _ index -> "super[" <> show index <> "](" <> renderEvTerm evidence <> ")"
    EvCast evidence coercion -> "cast(" <> renderEvTerm evidence <> ", " <> renderCoercion coercion <> ")"
    EvTypeable _ ty arguments ->
      "typeable @" <> renderTcType ty <> renderEvidenceArgs arguments
    EvTypeLam variable body ->
      "Λ" <> T.unpack (tvName variable) <> ". " <> renderEvTerm body
    EvDictLam predicate _ body ->
      "λ(_ ∷ " <> renderPred predicate <> "). " <> renderEvTerm body
    EvTypeApp function argument ->
      "(" <> renderEvTerm function <> ") @" <> renderTcType argument
    EvDictApp function argument ->
      "(" <> renderEvTerm function <> ") (" <> renderEvTerm argument <> ")"
    EvCallStackPush _ function site parent ->
      "push(" <> T.unpack function <> "@" <> show (callSiteStartLine site) <> ":" <> show (callSiteStartColumn site) <> ", " <> renderEvTerm parent <> ")"
    EvCallStackEmpty _ -> "emptyCallStack"

renderTypeArgs :: [TcType] -> String
renderTypeArgs [] = ""
renderTypeArgs tys = " @" <> intercalate " @" (map renderTcType tys)

renderEvidenceArgs :: [EvTerm] -> String
renderEvidenceArgs [] = ""
renderEvidenceArgs evs = " (" <> intercalate ", " (map renderEvTerm evs) <> ")"

renderCoercion :: Coercion -> String
renderCoercion coercion =
  case coercion of
    CoVar evVar -> renderEvVar evVar
    GivenCo predicate -> "given " <> renderPred predicate
    Refl ty -> "refl " <> renderTcType ty
    Sym co -> "sym (" <> renderCoercion co <> ")"
    Trans left right -> "trans (" <> renderCoercion left <> ") (" <> renderCoercion right <> ")"
    AppCo function argument -> "app (" <> renderCoercion function <> ") (" <> renderCoercion argument <> ")"
    FunCo domain range -> "fun (" <> renderCoercion domain <> ") (" <> renderCoercion range <> ")"
    NthCo index proof -> "nth[" <> show index <> "](" <> renderCoercion proof <> ")"
    EvidenceCo _ evidence -> renderEvTerm evidence
    TyConAppCo tyCon _ args -> T.unpack (tyConName tyCon) <> " " <> unwords (map renderCoercion args)
    AxiomInstCo key tys -> show key <> renderTypeArgs tys

renderEvVar :: EvVar -> String
renderEvVar (EvVar (Unique unique)) = "ev" <> show unique

renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred cls args -> T.unpack (tyConName cls) <> concatMap ((" " <>) . renderTcType) args
    EqPred left right -> renderTcType left <> " ~ " <> renderTcType right
    IParamPred name payload -> T.unpack name <> " ∷ " <> renderTcType payload
    QuantifiedPred variables antecedents consequent ->
      "forall "
        <> unwords (map (T.unpack . tvName) variables)
        <> ". "
        <> (if null antecedents then "" else "(" <> intercalate ", " (map renderPred antecedents) <> ") => ")
        <> renderPred consequent
