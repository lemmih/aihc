-- | Classify constraints without a change to their evidence variables.
module Aihc.Tc.Solve.Canonicalize
  ( canonicalize,
    classifyCt,
    CanonResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Types

-- | Result of canonicalization.
data CanonResult
  = -- | Produce canonical equality constraints.
    CanonEqs ![Ct]
  | -- | Produce a canonical dictionary constraint.
    CanonDict !Ct
  | -- | Constraint is already solved (trivially true).
    CanonSolved
  deriving (Show)

-- | Keep equality evidence intact for the equality solver.
canonicalize :: Ct -> CanonResult
canonicalize ct = case ctPred ct of
  EqPred {} -> CanonEqs [ct]
  ClassPred {} -> CanonDict ct
  QuantifiedPred {} -> CanonDict ct
  IParamPred {} -> CanonDict ct

-- | Classify a constraint as equality or dictionary.
classifyCt :: Ct -> Either Ct Ct
classifyCt ct = case ctPred ct of
  EqPred {} -> Left ct
  ClassPred {} -> Right ct
  QuantifiedPred {} -> Right ct
  IParamPred {} -> Right ct
