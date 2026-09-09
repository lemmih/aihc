-- | Constraint representation for the OutsideIn(X) solver.
--
-- The internal constraint language is richer than just 'Pred' because
-- OutsideIn(X) needs implications.
module Aihc.Tc.Constraint
  ( -- * Constraint flavors
    CtFlavor (..),

    -- * Constraint origins
    CtOrigin (..),
    CtProvenance (..),
    EqProvenance (..),
    TypeOrigin (..),
    TypeRole (..),
    TypeTrace (..),

    -- * Constraints
    Ct (..),
    mkWantedCt,
    mkWantedEqCt,
    ctEqProvenance,

    -- * Implications
    Implication (..),
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Evidence (EvVar)
import Aihc.Tc.Types
import Data.Text (Text)

-- | The flavor of a constraint.
data CtFlavor
  = -- | Given: from GADT match, user annotation, or superclass.
    Given
  | -- | Wanted: must be solved to typecheck the program.
    Wanted
  | -- | Derived: inferred, no evidence needed.
    Derived
  deriving (Eq, Show)

-- | The origin of a constraint, for error reporting.
--
-- Every constraint carries origin data so that error messages can explain
-- where the constraint came from.
data CtOrigin
  = OccurrenceOf !Text
  | AppOrigin !SourceSpan
  | LambdaOrigin !SourceSpan
  | LetOrigin !SourceSpan
  | LitOrigin !SourceSpan
  | SigOrigin !SourceSpan
  | CaseBranchOrigin !SourceSpan
  | InstOrigin !Text
  | UnifyOrigin !SourceSpan
  | -- | A use of an implicit parameter such as @?x@.
    ImplicitParamOrigin !Text
  deriving (Eq, Show)

-- | The diagnostic role a type played in an equality.
data TypeRole
  = ActualType
  | ExpectedType
  | RequiredType
  | InferredType
  deriving (Eq, Show)

-- | Where one side of a type constraint came from.
data TypeOrigin
  = UnknownTypeOrigin
  | ExpressionTypeOrigin !SourceSpan
  | ListElementTypeOrigin !SourceSpan
  | TypeSignatureOrigin !Text !SourceSpan
  | ConstraintTypeOrigin !CtOrigin
  deriving (Eq, Show)

-- | A type plus the diagnostic role and source of that type information.
data TypeTrace = TypeTrace
  { typeTraceType :: !TcType,
    typeTraceRole :: !TypeRole,
    typeTraceOrigin :: !TypeOrigin
  }
  deriving (Eq, Show)

-- | Expected-vs-actual provenance for an equality constraint.
data EqProvenance = EqProvenance
  { eqActualTrace :: !TypeTrace,
    eqExpectedTrace :: !TypeTrace,
    eqContextOrigins :: ![TypeOrigin],
    eqPrimarySpan :: !SourceSpan
  }
  deriving (Eq, Show)

-- | Diagnostic provenance carried by a constraint.
data CtProvenance
  = FromCtOrigin !CtOrigin
  | FromEqProvenance !EqProvenance
  deriving (Eq, Show)

-- | A constraint to be solved.
data Ct = Ct
  { ctPred :: !Pred,
    ctFlavor :: !CtFlavor,
    ctEvVar :: !EvVar,
    ctOrigin :: !CtOrigin,
    ctProvenance :: !CtProvenance,
    ctLoc :: !SourceSpan
  }
  deriving (Show)

-- | Create a wanted constraint.
mkWantedCt :: Pred -> EvVar -> CtOrigin -> SourceSpan -> Ct
mkWantedCt p ev orig loc =
  Ct
    { ctPred = p,
      ctFlavor = Wanted,
      ctEvVar = ev,
      ctOrigin = orig,
      ctProvenance = FromCtOrigin orig,
      ctLoc = loc
    }

-- | Create a wanted equality constraint with expected-vs-actual diagnostic
-- provenance. The solver may reorient or decompose the predicate, but the
-- provenance keeps the source-facing roles stable for error reporting.
mkWantedEqCt :: TypeTrace -> TypeTrace -> EvVar -> CtOrigin -> SourceSpan -> Ct
mkWantedEqCt actual expected ev orig loc =
  Ct
    { ctPred = EqPred (typeTraceType actual) (typeTraceType expected),
      ctFlavor = Wanted,
      ctEvVar = ev,
      ctOrigin = orig,
      ctProvenance =
        FromEqProvenance
          EqProvenance
            { eqActualTrace = actual,
              eqExpectedTrace = expected,
              eqContextOrigins = [],
              eqPrimarySpan = loc
            },
      ctLoc = loc
    }

ctEqProvenance :: Ct -> Maybe EqProvenance
ctEqProvenance ct =
  case ctProvenance ct of
    FromEqProvenance provenance -> Just provenance
    FromCtOrigin {} -> Nothing

-- | An implication constraint.
--
-- This is one of the most important data types in the whole design.
-- It represents branch-local reasoning scoped inside implications,
-- which is the heart of OutsideIn(X).
data Implication = Implication
  { -- | Skolem type variables introduced by this implication.
    implSkols :: ![TyVarId],
    -- | Given constraints (from GADT match, etc.).
    implGivenCts :: ![Ct],
    -- | Wanted constraints to solve under this implication.
    implWantedCts :: ![Ct]
  }
  deriving (Show)
