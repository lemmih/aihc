-- | Unification of types.
--
-- Handles meta-variable solving with occurs check.
module Aihc.Tc.Unify
  ( unify,
    unifyTypes,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Tc.Constraint (CtOrigin (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (tcTypeKind, unifyKindsAt)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)

-- | Unify two types, recording the solution and emitting an error if
-- they are incompatible.
unify :: SourceSpan -> CtOrigin -> TcType -> TcType -> TcM ()
unify loc origin t1 t2 = do
  t1' <- zonkType t1 >>= reduceTypeFamilies
  t2' <- zonkType t2 >>= reduceTypeFamilies
  result <- unifyTypesAt loc t1' t2'
  case result of
    Right () -> pure ()
    Left (UnificationError left right _ provenance) ->
      emitError loc (UnificationError left right origin provenance)
    Left err -> emitError loc err

-- | Attempt to unify two types, returning an error kind on failure.
unifyTypes :: TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypes = unifyTypesAt NoSourceSpan

-- | Attempt to unify two types. A kind mismatch is reported at the span.
unifyTypesAt :: SourceSpan -> TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypesAt _ (TcMetaTv u1) (TcMetaTv u2)
  | u1 == u2 = pure (Right ())
unifyTypesAt loc (TcMetaTv u) ty = unifyMetaTv loc u ty
unifyTypesAt loc ty (TcMetaTv u) = unifyMetaTv loc u ty
unifyTypesAt _ (TcTyVar v1) (TcTyVar v2)
  | v1 == v2 = pure (Right ())
unifyTypesAt loc t1 t2
  | t1 == t2 = pure (Right ())
  | otherwise = do
      children <- decomposeNominalEquality t1 t2
      case children of
        Just pairs -> sequence_ <$> mapM (uncurry (unifyTypesAt loc)) pairs
        Nothing -> pure (Left (UnificationError t1 t2 (UnifyOrigin NoSourceSpan) Nothing))

-- | Unify a meta-variable with a type, performing the occurs check.
unifyMetaTv :: SourceSpan -> Unique -> TcType -> TcM (Either TcErrorKind ())
unifyMetaTv loc u ty = do
  ty' <- zonkType ty
  case ty' of
    TcMetaTv u' | u == u' -> pure (Right ())
    _
      | occursIn u ty' -> pure $ Left $ OccursCheckError (TcMetaTv u) ty'
      -- A meta-variable stands for a monotype. Binding it to a polytype
      -- would let inference guess an impredicative instantiation.
      | isPolyType ty' -> pure $ Left $ UnificationError (TcMetaTv u) ty' (UnifyOrigin NoSourceSpan) Nothing
      | otherwise -> do
          declaredKind <- readMetaTvKind u
          solvedKind <- tcTypeKind ty'
          unifyKindsAt loc declaredKind solvedKind
          writeMetaTv u ty'
          pure (Right ())

-- | Check whether a meta-variable occurs in a type (occurs check).
occursIn :: Unique -> TcType -> Bool
occursIn u = go
  where
    go (TcMetaTv u') = u == u'
    go (TcTyVar _) = False
    go (TcTyCon _ args) = any go args
    go (TcFunTy a b) = go a || go b
    go (TcForAllTy _ body) = go body
    go (TcQualTy preds body) = any goPred preds || go body
    go (TcAppTy f a) = go f || go a

    goPred (ClassPred _ args) = any go args
    goPred (EqPred a b) = go a || go b
    goPred (IParamPred _ payload) = go payload
    goPred (QuantifiedPred variables antecedents consequent) =
      any (go . tvKind) variables || any goPred antecedents || goPred consequent
