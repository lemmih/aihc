{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize type-checker annotations after constraint solving.
module Aihc.Tc.Finalize
  ( finalizeModuleTc,
  )
where

import Aihc.Parser.Syntax (Annotation, Module, fromAnnotation, mkAnnotation)
import Aihc.Resolve.Traverse (traverseAnnotations)
import Aihc.Tc.Annotations
  ( PendingTcAnnotation (..),
    TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    TcPatSynAnnotation (..),
    renderTcType,
  )
import Aihc.Tc.Env (AssociatedTypeInfo (..), DataConFieldInfo (..), DataConInfo (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), TypeFamilyInstanceInfo (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar)
import Aihc.Tc.Kind (defaultKindMetas)
import Aihc.Tc.Monad
import Aihc.Tc.Tidy (tidyType)
import Aihc.Tc.Types (Pred (..), TcType (..), TyVarId, Unique (..), tvKind, pattern KType)
import Aihc.Tc.Zonk (defaultPredKinds, defaultTyVarKinds, defaultTypeKinds, zonkPred, zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Maybe (mapMaybe)

-- | Convert every pending type-checker annotation in a module to a final
-- annotation. The walk covers every syntax constructor, so a pending
-- annotation cannot escape.
finalizeModuleTc :: Module -> TcM Module
finalizeModuleTc = traverseAnnotations finalizeAnnotationTc

finalizeAnnotationTc :: Annotation -> TcM Annotation
finalizeAnnotationTc ann =
  case fromAnnotation @PendingTcAnnotation ann of
    Just pending -> mkAnnotation <$> annotationForPendingTc pending
    Nothing ->
      case fromAnnotation @TcPatSynAnnotation ann of
        Just patSyn -> do
          -- The matcher, builder, and selector equations live inside the
          -- annotation. The walk does not enter an annotation payload.
          matcher <- traverseAnnotations finalizeAnnotationTc (tcPatSynMatcher patSyn)
          builder <- traverse (traverseAnnotations finalizeAnnotationTc) (tcPatSynBuilder patSyn)
          selectors <- traverse (traverse (traverseAnnotations finalizeAnnotationTc)) (tcPatSynSelectors patSyn)
          pure (mkAnnotation (TcPatSynAnnotation matcher builder selectors))
        Nothing -> do
          rejectMetaFinalAnnotation ann
          pure ann

annotationForPendingTc :: PendingTcAnnotation -> TcM TcAnnotation
annotationForPendingTc pending = do
  ty <- zonkType (pendingTcAnnType pending)
  typeBinders <- mapM zonkTypeBinder (pendingTcAnnTypeBinders pending)
  typeArgs <- mapM zonkType (pendingTcAnnTypeArgs pending)
  evidenceTerms <- mapM (evidenceForEvVar ty >=> zonkEvTerm) (pendingTcAnnEvidenceVars pending)
  evidenceBinders <- mapM (evidenceForEvVar ty >=> zonkEvTerm) (pendingTcAnnEvidenceBinders pending)
  termArgTypes <- mapM zonkType (pendingTcAnnTermArgTypes pending)
  let ann = TcAnnotation ty typeBinders typeArgs evidenceTerms evidenceBinders termArgTypes
  defaulted <- defaultUnsolvedAnnotationMetas ann
  rejectMetaTcAnnotation defaulted
  pure defaulted

defaultUnsolvedAnnotationMetas :: TcAnnotation -> TcM TcAnnotation
defaultUnsolvedAnnotationMetas annotation =
  case firstMetaTcAnnotation annotation of
    Nothing -> pure annotation
    Just meta -> do
      solution <- readMetaTv meta
      case solution of
        Just {} -> pure ()
        Nothing -> do
          kind <- defaultKindMetas =<< readMetaTvKind meta
          case kind of
            KType -> do
              unitTyCon <- mkKnownTyCon "GHC.Tuple" "Unit" 0 KType
              writeMetaTv meta (TcTyCon unitTyCon [])
            _ ->
              abortTc
                ( "internal type annotation error: cannot default a meta-variable with kind "
                    <> renderTcType (tidyType kind)
                )
      annotation' <- zonkTcAnnotation annotation
      defaultUnsolvedAnnotationMetas annotation'

zonkTcAnnotation :: TcAnnotation -> TcM TcAnnotation
zonkTcAnnotation annotation =
  TcAnnotation
    <$> zonkType (tcAnnType annotation)
    <*> mapM zonkTypeBinder (tcAnnTypeBinders annotation)
    <*> mapM zonkType (tcAnnTypeArgs annotation)
    <*> mapM zonkEvTerm (tcAnnEvidenceTerms annotation)
    <*> mapM zonkEvTerm (tcAnnEvidenceBinders annotation)
    <*> mapM zonkType (tcAnnTermArgTypes annotation)

zonkTypeBinder :: TyVarId -> TcM TyVarId
zonkTypeBinder binder = do
  binderType <- zonkType (TcTyVar binder)
  case binderType of
    TcTyVar binder' -> pure binder'
    _ -> abortTc "internal type annotation error: type binder zonked to a non-variable"

evidenceForEvVar :: TcType -> EvVar -> TcM EvTerm
evidenceForEvVar contextType ev = do
  maybeEvidence <- lookupEvidence ev
  case maybeEvidence of
    Just evidence -> pure evidence
    Nothing -> do
      -- A reported type error can leave a wanted constraint without
      -- evidence. Keep the placeholder so the error reaches the user.
      errorCount <- currentErrorCount
      if errorCount > 0
        then pure (EvVarTerm ev)
        else abortTc ("internal type annotation error: missing evidence for " <> show ev <> " while finalizing " <> renderTcType (tidyType contextType))

zonkEvTerm :: EvTerm -> TcM EvTerm
zonkEvTerm evTerm =
  case evTerm of
    EvVarTerm ev -> do
      -- Follow a solved evidence variable to its term.
      maybeEvidence <- lookupEvidence ev
      case maybeEvidence of
        Just evidence -> zonkEvTerm evidence
        Nothing -> pure (EvVarTerm ev)
    EvGiven pred' ->
      EvGiven <$> finalizePred pred'
    EvDict origin name typeArgs evidence ->
      EvDict origin name <$> mapM finalizeType typeArgs <*> mapM zonkEvTerm evidence
    EvCoercion coercion ->
      EvCoercion <$> zonkCoercion coercion
    EvSuperClass evidence sourceOrigin sourcePredicate fieldTypes index ->
      EvSuperClass <$> zonkEvTerm evidence <*> pure sourceOrigin <*> finalizePred sourcePredicate <*> mapM finalizeType fieldTypes <*> pure index
    EvCast evidence coercion ->
      EvCast <$> zonkEvTerm evidence <*> zonkCoercion coercion
    EvTypeable origin ty arguments ->
      EvTypeable origin <$> finalizeType ty <*> mapM zonkEvTerm arguments
    EvTypeLam variable body ->
      EvTypeLam <$> defaultTyVarKinds variable <*> zonkEvTerm body
    EvDictLam predicate binderType body ->
      EvDictLam <$> finalizePred predicate <*> finalizeType binderType <*> zonkEvTerm body
    EvTypeApp function argument ->
      EvTypeApp <$> zonkEvTerm function <*> finalizeType argument
    EvDictApp function argument ->
      EvDictApp <$> zonkEvTerm function <*> zonkEvTerm argument
    EvCallStackPush origin function site parent ->
      EvCallStackPush origin function site <$> zonkEvTerm parent
    EvCallStackEmpty origin ->
      pure (EvCallStackEmpty origin)

finalizeType :: TcType -> TcM TcType
finalizeType = zonkType >=> defaultTypeKinds

finalizePred :: Pred -> TcM Pred
finalizePred = zonkPred >=> defaultPredKinds

zonkCoercion :: Coercion -> TcM Coercion
zonkCoercion coercion =
  case coercion of
    CoVar ev ->
      pure (CoVar ev)
    Refl ty ->
      Refl <$> zonkType ty
    Sym inner ->
      Sym <$> zonkCoercion inner
    Trans left right ->
      Trans <$> zonkCoercion left <*> zonkCoercion right
    TyConAppCo tyCon coercions ->
      TyConAppCo tyCon <$> mapM zonkCoercion coercions
    AxiomInstCo name typeArgs ->
      AxiomInstCo name <$> mapM zonkType typeArgs

rejectMetaTcAnnotation :: TcAnnotation -> TcM ()
rejectMetaTcAnnotation ann =
  case firstMetaTcAnnotation ann of
    Nothing -> pure ()
    Just {} ->
      abortTc
        ( "internal type annotation error: unzonked meta-variable in finalized annotation with type "
            <> renderTcType (tidyType (tcAnnType ann))
        )

rejectMetaFinalAnnotation :: Annotation -> TcM ()
rejectMetaFinalAnnotation ann = do
  traverseReject "type annotation" (firstMetaTcAnnotation <$> fromAnnotation @TcAnnotation ann)
  traverseReject "class annotation" (firstMetaClassAnnotation <$> fromAnnotation @TcClassAnnotation ann)
  traverseReject "deriving annotation" (firstMetaDerivingAnnotation <$> fromAnnotation @TcDerivingAnnotation ann)
  traverseReject "instance annotation" (firstMetaInstanceAnnotation <$> fromAnnotation @TcInstanceAnnotation ann)
  traverseReject "instance method annotation" (firstMetaInstanceMethodAnnotation <$> fromAnnotation @TcInstanceMethodAnnotation ann)
  traverseReject "data-family instance annotation" (firstMetaDataFamilyInstance <$> fromAnnotation @DataFamilyInstanceInfo ann)
  traverseReject "type-family instance annotation" (firstMetaTypeFamilyInstance <$> fromAnnotation @TypeFamilyInstanceInfo ann)
  where
    traverseReject _ Nothing = pure ()
    traverseReject context (Just maybeMeta) = rejectMeta ("finalized " <> context) maybeMeta

rejectMeta :: String -> Maybe Unique -> TcM ()
rejectMeta context maybeMeta =
  case maybeMeta of
    Nothing ->
      pure ()
    Just {} ->
      abortTc ("internal type annotation error: unzonked meta-variable in " <> context)

firstMetaTcAnnotation :: TcAnnotation -> Maybe Unique
firstMetaTcAnnotation ann =
  firstJusts
    ( firstMetaType (tcAnnType ann)
        : map firstMetaType (tcAnnTypeArgs ann)
        ++ map firstMetaEvTerm (tcAnnEvidenceTerms ann)
        ++ map firstMetaEvTerm (tcAnnEvidenceBinders ann)
        ++ map firstMetaType (tcAnnTermArgTypes ann)
    )

firstMetaClassAnnotation :: TcClassAnnotation -> Maybe Unique
firstMetaClassAnnotation classAnnotation =
  firstJusts (map (firstMetaType . TcTyVar) (tcClassKindTyVars classAnnotation))
    <|> firstJusts (map (firstMetaType . tcDictBinderType) (tcClassSuperClasses classAnnotation))
    <|> firstJusts (map firstMetaClassMethodAnnotation (tcClassMethods classAnnotation))
    <|> firstJusts (map (firstMetaType . snd) (tcClassDefaultSignatures classAnnotation))
    <|> firstJusts (map firstMetaTypeFamilyInstance (mapMaybe atiDefault (tcClassAssociatedTypes classAnnotation)))

firstMetaClassMethodAnnotation :: TcClassMethodAnnotation -> Maybe Unique
firstMetaClassMethodAnnotation method =
  firstMetaType (tcClassMethodType method) <|> firstMetaType (tcClassMethodDictType method)

firstMetaDerivingAnnotation :: TcDerivingAnnotation -> Maybe Unique
firstMetaDerivingAnnotation annotation =
  firstJusts (map firstMetaDerivingPlan (tcDerivingPlans annotation))

firstMetaDerivingPlan :: TcDerivingPlan -> Maybe Unique
firstMetaDerivingPlan plan =
  firstJusts
    ( map firstMetaType (tcDerivingHeadTypes plan)
        ++ maybe [] (map firstMetaDataConInfo . dtiConstructors) (tcDerivingDataType plan)
        ++ map firstMetaClassMethodAnnotation (tcDerivingClassMethods plan)
        ++ map firstMetaDictBinderAnnotation (tcDerivingClassSuperClasses plan)
        ++ concatMap (map firstMetaPred . snd) (tcDerivingDefaultSignatures plan)
        ++ [firstMetaDerivingStrategy (tcDerivingStrategy plan), firstMetaDerivingContext (tcDerivingContext plan)]
    )

firstMetaDerivingStrategy :: TcDerivingStrategy -> Maybe Unique
firstMetaDerivingStrategy strategy =
  case strategy of
    TcDerivingVia viaType -> firstMetaType viaType
    _ -> Nothing

firstMetaDerivingContext :: TcDerivingContext -> Maybe Unique
firstMetaDerivingContext context =
  case context of
    TcDerivingInferContext -> Nothing
    TcDerivingExplicitContext predicates -> firstJusts (map firstMetaPred predicates)

firstMetaInstanceAnnotation :: TcInstanceAnnotation -> Maybe Unique
firstMetaInstanceAnnotation ann =
  firstJusts
    ( firstMetaType (tcInstanceDictType ann)
        : map firstMetaType (tcInstanceHeadTypes ann)
        ++ map firstMetaDictBinderAnnotation (tcInstanceClassSuperClasses ann)
        ++ map (firstMetaType . tcClassMethodType) (tcInstanceClassMethods ann)
        ++ map firstMetaDictBinderAnnotation (tcInstanceContextDicts ann)
        ++ [ firstMetaDictBinderAnnotation superClass <|> firstMetaEvTerm evidence
           | (superClass, evidence) <- tcInstanceSuperClasses ann
           ]
        ++ concatMap (map firstMetaEvTerm . snd) (tcInstanceDefaultMethodEvidence ann)
        ++ map firstMetaTypeFamilyInstance (tcInstanceAssociatedTypes ann)
    )

firstMetaDictBinderAnnotation :: TcDictBinderAnnotation -> Maybe Unique
firstMetaDictBinderAnnotation ann =
  firstJusts (map firstMetaType (tcDictBinderArgs ann)) <|> firstMetaType (tcDictBinderType ann)

firstMetaInstanceMethodAnnotation :: TcInstanceMethodAnnotation -> Maybe Unique
firstMetaInstanceMethodAnnotation ann =
  firstMetaType (tcInstanceMethodType ann)

firstMetaDataFamilyInstance :: DataFamilyInstanceInfo -> Maybe Unique
firstMetaDataFamilyInstance info =
  firstMetaType (dfiiFamilyType info)

firstMetaTypeFamilyInstance :: TypeFamilyInstanceInfo -> Maybe Unique
firstMetaTypeFamilyInstance info =
  firstMetaType (tfiiLeft info) <|> firstMetaType (tfiiRight info)

firstMetaDataConInfo :: DataConInfo -> Maybe Unique
firstMetaDataConInfo info =
  firstJusts
    ( map firstMetaPred (dciTheta info)
        ++ map (firstMetaType . dcfiType) (dciFields info)
        ++ [firstMetaType (dciResTy info)]
    )

firstMetaEvTerm :: EvTerm -> Maybe Unique
firstMetaEvTerm evTerm =
  case evTerm of
    EvVarTerm {} ->
      Nothing
    EvGiven pred' ->
      firstMetaPred pred'
    EvDict _ _ typeArgs evidence ->
      firstJusts (map firstMetaType typeArgs ++ map firstMetaEvTerm evidence)
    EvCoercion coercion ->
      firstMetaCoercion coercion
    EvSuperClass evidence _ sourcePredicate fieldTypes _ ->
      firstMetaEvTerm evidence <|> firstMetaPred sourcePredicate <|> firstJusts (map firstMetaType fieldTypes)
    EvCast evidence coercion ->
      firstMetaEvTerm evidence <|> firstMetaCoercion coercion
    EvTypeable _ ty arguments ->
      firstMetaType ty <|> firstJusts (map firstMetaEvTerm arguments)
    EvTypeLam variable body ->
      firstMetaType (tvKind variable) <|> firstMetaEvTerm body
    EvDictLam predicate binderType body ->
      firstMetaPred predicate <|> firstMetaType binderType <|> firstMetaEvTerm body
    EvTypeApp function argument ->
      firstMetaEvTerm function <|> firstMetaType argument
    EvDictApp function argument ->
      firstMetaEvTerm function <|> firstMetaEvTerm argument
    EvCallStackPush _ _ _ parent ->
      firstMetaEvTerm parent
    EvCallStackEmpty {} ->
      Nothing

firstMetaCoercion :: Coercion -> Maybe Unique
firstMetaCoercion coercion =
  case coercion of
    CoVar {} ->
      Nothing
    Refl ty ->
      firstMetaType ty
    Sym inner ->
      firstMetaCoercion inner
    Trans left right ->
      firstMetaCoercion left <|> firstMetaCoercion right
    TyConAppCo _ coercions ->
      firstJusts (map firstMetaCoercion coercions)
    AxiomInstCo _ typeArgs ->
      firstJusts (map firstMetaType typeArgs)

firstMetaPred :: Pred -> Maybe Unique
firstMetaPred pred' =
  case pred' of
    ClassPred _ args ->
      firstJusts (map firstMetaType args)
    EqPred left right ->
      firstMetaType left <|> firstMetaType right
    IParamPred _ payload ->
      firstMetaType payload
    QuantifiedPred variables antecedents consequent ->
      firstJusts (map (firstMetaType . tvKind) variables)
        <|> firstJusts (map firstMetaPred antecedents)
        <|> firstMetaPred consequent

firstMetaType :: TcType -> Maybe Unique
firstMetaType ty =
  case ty of
    TcMetaTv meta ->
      Just meta
    TcTyVar {} ->
      Nothing
    TcTyCon _ args ->
      firstJusts (map firstMetaType args)
    TcFunTy left right ->
      firstMetaType left <|> firstMetaType right
    TcForAllTy _ body ->
      firstMetaType body
    TcQualTy preds body ->
      firstJusts (map firstMetaPred preds) <|> firstMetaType body
    TcAppTy fun arg ->
      firstMetaType fun <|> firstMetaType arg

firstJusts :: [Maybe a] -> Maybe a
firstJusts [] = Nothing
firstJusts (Nothing : rest) = firstJusts rest
firstJusts (Just value : _) = Just value
