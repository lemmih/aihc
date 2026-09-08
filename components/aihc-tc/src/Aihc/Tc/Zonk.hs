-- | Zonking: replace meta-variables with their solutions.
--
-- After solving, zonking replaces all meta-variables throughout the
-- type annotations. Any remaining unsolved meta-variables become
-- ambiguity errors or are defaulted.
module Aihc.Tc.Zonk
  ( zonkType,
    zonkPred,
    defaultTypeKinds,
    defaultTypeSchemeKinds,
    defaultTyConKindScheme,
    defaultPredKinds,
    defaultTyVarKinds,
    zonkErrorKind,
    finalizeDiagnostics,
  )
where

import Aihc.Tc.Constraint (EqProvenance (..), TypeTrace (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..))
import Aihc.Tc.Kind (defaultKindMetas, zonkKind)
import Aihc.Tc.Monad (TcM, TcState (..), getKinds, readMetaTv, writeMetaTv)
import Aihc.Tc.Tidy (tidyDiagnostic)
import Aihc.Tc.Types
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets, modify')

-- | Zonk a type: chase meta-variable solutions to their final values.
zonkType :: TcType -> TcM TcType
zonkType ty = case ty of
  TcArrowTy -> pure ty
  TcMetaTv u -> do
    mSol <- readMetaTv u
    case mSol of
      Nothing -> pure ty
      Just sol -> do
        zonked <- zonkType sol
        writeMetaTv u zonked
        pure zonked
  TcTyVar tv -> TcTyVar <$> zonkTyVar tv
  TcTyCon tc args -> TcTyCon tc <$> mapM zonkType args
  TcFunTy a b -> TcFunTy <$> zonkType a <*> zonkType b
  TcForAllTy tv body -> TcForAllTy <$> zonkTyVar tv <*> zonkType body
  TcQualTy preds body -> TcQualTy <$> mapM zonkPred preds <*> zonkType body
  TcAppTy f a -> mkAppTy <$> zonkType f <*> zonkType a

-- | Zonk a predicate.
zonkPred :: Pred -> TcM Pred
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b
zonkPred (IParamPred name payload) = IParamPred name <$> zonkType payload
zonkPred (QuantifiedPred variables antecedents consequent) =
  QuantifiedPred <$> mapM zonkTyVar variables <*> mapM zonkPred antecedents <*> zonkPred consequent

zonkTyVar :: TyVarId -> TcM TyVarId
zonkTyVar tv = do
  kind <- zonkKind (tvKind tv)
  pure (setTyVarKind kind tv)

-- | Finalize every kind embedded in a type. Unlike ordinary zonking, this
-- defaults unconstrained kind metavariables to 'Type', so it must only run at
-- a module/interface boundary after kind constraints have been solved.
defaultTypeKinds :: TcType -> TcM TcType
defaultTypeKinds ty =
  case ty of
    TcMetaTv {} -> pure ty
    TcArrowTy -> pure ty
    TcTyVar tv -> TcTyVar <$> defaultTyVarKinds tv
    TcTyCon tyCon args -> TcTyCon tyCon <$> mapM defaultTypeKinds args
    TcFunTy argument result -> TcFunTy <$> defaultTypeKinds argument <*> defaultTypeKinds result
    TcForAllTy tv body -> TcForAllTy <$> defaultTyVarKinds tv <*> defaultTypeKinds body
    TcQualTy predicates body -> TcQualTy <$> mapM defaultPredKinds predicates <*> defaultTypeKinds body
    TcAppTy function argument -> mkAppTy <$> defaultTypeKinds function <*> defaultTypeKinds argument

defaultTypeSchemeKinds :: TypeScheme -> TcM TypeScheme
defaultTypeSchemeKinds (ForAll tyVars predicates body) =
  ForAll
    <$> mapM defaultTyVarKinds tyVars
    <*> mapM defaultPredKinds predicates
    <*> defaultTypeKinds body

defaultTyConKindScheme :: TypeScheme -> TcM TypeScheme
defaultTyConKindScheme scheme@(ForAll tyVars predicates _) = do
  tyVars' <- mapM defaultTyVarKinds tyVars
  predicates' <- mapM defaultPredKinds predicates
  kind <- defaultKindMetas (typeSchemeBody scheme) >>= zonkKind
  pure (ForAll tyVars' predicates' kind)

defaultPredKinds :: Pred -> TcM Pred
defaultPredKinds predicate =
  case predicate of
    ClassPred className args -> ClassPred className <$> mapM defaultTypeKinds args
    EqPred left right -> EqPred <$> defaultTypeKinds left <*> defaultTypeKinds right
    IParamPred name payload -> IParamPred name <$> defaultTypeKinds payload
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred <$> mapM defaultTyVarKinds variables <*> mapM defaultPredKinds antecedents <*> defaultPredKinds consequent

defaultTyVarKinds :: TyVarId -> TcM TyVarId
defaultTyVarKinds tv = do
  kind <- defaultKindMetas (tvKind tv)
  pure (setTyVarKind kind tv)

-- | Zonk the types in one error kind.
zonkErrorKind :: TcErrorKind -> TcM TcErrorKind
zonkErrorKind kind =
  case kind of
    UnificationError left right origin provenance ->
      UnificationError <$> zonkType left <*> zonkType right <*> pure origin <*> traverse zonkProvenance provenance
    OccursCheckError variable ty ->
      OccursCheckError <$> zonkType variable <*> zonkType ty
    KindMismatch expected actual ->
      KindMismatch <$> zonkType expected <*> zonkType actual
    UnsolvedWanted predicate origin ->
      UnsolvedWanted <$> zonkPred predicate <*> pure origin
    TopLevelUnliftedBinding name ty ->
      TopLevelUnliftedBinding name <$> zonkType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      RepresentationPolymorphicFunctionArgument name <$> zonkType ty
    UnboundVariable {} -> pure kind
    OtherError {} -> pure kind

zonkProvenance :: EqProvenance -> TcM EqProvenance
zonkProvenance provenance = do
  actual <- zonkTrace (eqActualTrace provenance)
  expected <- zonkTrace (eqExpectedTrace provenance)
  pure provenance {eqActualTrace = actual, eqExpectedTrace = expected}
  where
    zonkTrace trace = do
      ty <- zonkType (typeTraceType trace)
      pure trace {typeTraceType = ty}

-- | Zonk and tidy the collected diagnostics.
--
-- Run this before the diagnostics leave the type checker.
-- Zonking shows the solutions that the solver found after the diagnostic.
-- Tidying replaces internal meta-variable numbers with stable display names.
finalizeDiagnostics :: TcM ()
finalizeDiagnostics = do
  kinds <- getKinds
  diagnostics <- lift (gets tcsDiagnostics)
  zonked <- mapM zonkDiagnostic diagnostics
  lift (modify' (\state -> state {tcsDiagnostics = map (tidyDiagnostic kinds) zonked}))
  where
    zonkDiagnostic diagnostic = do
      kind <- zonkErrorKind (diagKind diagnostic)
      pure diagnostic {diagKind = kind}
