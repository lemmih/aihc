-- | Scheme instantiation.
--
-- Instantiation replaces the quantified type variables in a 'TypeScheme'
-- with fresh meta-variables, and emits wanted constraints for the scheme's
-- predicates.
module Aihc.Tc.Instantiate
  ( Instantiation (..),
    instantiate,
    instantiateWithArgs,
  )
where

import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (foldM)
import Data.Map.Strict qualified as Map

data Instantiation = Instantiation
  { instType :: !TcType,
    instTypeArgs :: ![TcType],
    instPreds :: ![Pred]
  }
  deriving (Eq, Show)

-- | Instantiate a type scheme.
--
-- Returns the instantiated monotype and the wanted predicates
-- (constraints that must be satisfied at the use site).
instantiate :: TypeScheme -> TcM (TcType, [Pred])
instantiate scheme = do
  inst <- instantiateWithArgs scheme
  pure (instType inst, instPreds inst)

instantiateWithArgs :: TypeScheme -> TcM Instantiation
instantiateWithArgs (ForAll tvs preds body) = do
  -- Allocate in binder order so later binder kinds can refer to earlier
  -- instantiations (for example @b :: TYPE r@).
  subst <- foldM extendSubst Map.empty tvs
  let substTy = applySubst subst
      body' = substTy body
      preds' = map (applySubstPred subst) preds
      typeArgs = map (substTy . TcTyVar) tvs
  pure
    Instantiation
      { instType = body',
        instTypeArgs = typeArgs,
        instPreds = preds'
      }
  where
    extendSubst subst tv = do
      meta <- freshMetaTvOfKind (applySubst subst (tvKind tv))
      pure (Map.insert (tvUnique tv) meta subst)
