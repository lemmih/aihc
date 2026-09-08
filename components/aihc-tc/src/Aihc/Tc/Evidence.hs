-- | Evidence terms and coercions.
--
-- Evidence should be a first-class IR concept from day one.
-- Class constraints elaborate to dictionaries, equality constraints
-- elaborate to coercions.
module Aihc.Tc.Evidence
  ( -- * Evidence variables
    EvVar (..),

    -- * Evidence terms
    EvTerm (..),
    CallSite (..),
    TypeableTyCon (..),
    TypeableKind (..),
    EvBinding (..),

    -- * Coercions
    Coercion (..),
  )
where

import Aihc.Tc.Types
import Data.Text (Text)

-- | An evidence variable, uniquely identified.
newtype EvVar = EvVar {evVarUnique :: Unique}
  deriving (Eq, Ord, Show, Read)

-- | Evidence terms.
--
-- Every wanted constraint gets an evidence variable; the solver fills it.
-- The evidence is then used during elaboration to produce dictionary-passing
-- and coercion-carrying core.
data EvTerm
  = -- | Reference to an evidence variable (given or previously solved).
    EvVarTerm !EvVar
  | -- | Given dictionary from a qualified type.
    EvGiven !Pred
  | -- | Dictionary construction: origin, dictionary name, type args, sub-evidence.
    EvDict !(Text, Text) !Text ![TcType] ![EvTerm]
  | -- | A checked representation constraint with an empty dictionary.
    EvCoercible !TyCon !TcType !TcType
  | -- | Coercion evidence (for equality constraints).
    EvCoercion !Coercion
  | -- | Superclass selection from a dictionary. The source class origin,
    -- predicate, and complete field layout make the projection explicit.
    EvSuperClass !EvTerm !(Maybe (Text, Text)) !Pred ![TcType] !Int
  | -- | Cast evidence through a coercion.
    EvCast !EvTerm !Coercion
  | -- | Compiler-synthesized structural runtime type representation.
    EvTypeable !(Maybe (Text, Text)) !TcType !TypeableTyCon ![(TcType, EvTerm)] ![EvTerm]
  | -- | Type abstraction for quantified evidence.
    EvTypeLam !TyVarId !EvTerm
  | -- | Dictionary abstraction with its checked binder type.
    EvDictLam !Pred !TcType !EvTerm
  | -- | Type application for quantified evidence.
    EvTypeApp !EvTerm !TcType
  | -- | Dictionary application for quantified evidence.
    EvDictApp !EvTerm !EvTerm
  | -- | A call-stack entry for one occurrence of a function with a
    -- @HasCallStack@ constraint: the origin of the @CallStack@ type, the
    -- function name, the call site, and the parent call stack.
    EvCallStackPush !(Text, Text) !Text !CallSite !EvTerm
  | -- | The empty call stack, with the origin of the @CallStack@ type.
    EvCallStackEmpty !(Text, Text)
  deriving (Eq, Ord, Show, Read)

-- | Checked constructor metadata for runtime reflection.
data TypeableTyCon = TypeableTyCon !TyCon !Int !TypeableKind
  deriving (Eq, Ord, Show, Read)

-- | Checked kind structure. Variables index the constructor kind arguments.
data TypeableKind
  = TypeableKindVar !Int
  | TypeableKindType !TcType
  | TypeableKindFun !TypeableKind !TypeableKind
  | TypeableKindCon !TypeableTyCon ![TypeableKind]
  | TypeableKindApp !TypeableKind !TypeableKind
  deriving (Eq, Ord, Show, Read)

-- | The source position of one call site.
data CallSite = CallSite
  { callSiteFile :: !Text,
    callSiteStartLine :: !Int,
    callSiteStartColumn :: !Int,
    callSiteEndLine :: !Int,
    callSiteEndColumn :: !Int
  }
  deriving (Eq, Ord, Show, Read)

-- | A binding of an evidence variable to its term.
data EvBinding = EvBinding
  { evBindVar :: !EvVar,
    evBindTerm :: !EvTerm
  }
  deriving (Eq, Ord, Show, Read)

-- | Coercions for type equality evidence.
--
-- The solver preserves nominal evidence through symmetry, transitivity,
-- and congruence. Argument projections retain their source proof.
data Coercion
  = -- | Coercion variable.
    CoVar !EvVar
  | -- | A nominal equality from the current evidence scope.
    GivenCo !Pred
  | -- | Reflexivity: @t ~ t@.
    Refl !TcType
  | -- | Symmetry: if @co : t1 ~ t2@ then @Sym co : t2 ~ t1@.
    Sym !Coercion
  | -- | Transitivity: @Trans co1 co2 : t1 ~ t3@.
    Trans !Coercion !Coercion
  | -- | Lift through a type constructor. The argument types determine implicit kind arguments.
    TyConAppCo !TyCon ![TcType] ![Coercion]
  | -- | Congruence for a type application.
    AppCo !Coercion !Coercion
  | -- | Congruence for a function domain and range.
    FunCo !Coercion !Coercion
  | -- | Project a nominal argument. Zero selects the last argument.
    NthCo !Int !Coercion
  | -- | Equality evidence from a dictionary field.
    EvidenceCo !Pred !EvTerm
  | -- | Type family / newtype axiom instantiation (future).
    AxiomInstCo !TcAxiomKey ![TcType]
  deriving (Eq, Ord, Show, Read)
