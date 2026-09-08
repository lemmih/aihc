-- | The identities of the type constructors and terms that the type checker
-- knows by name rather than from source.
--
-- Source syntax such as @(a, b)@ or @(# a, b #)@ names no module, so the
-- type checker cannot resolve it. Neither can it resolve the types it
-- reaches for on its own: the @Bool@ of a guard, the @Char@ of a character
-- literal, the list of a comprehension. The compiler that embeds the type
-- checker says where each of those lives through a 'TcWiring' table in its
-- configuration, the same way 'Aihc.Tc.Deriving.References' says where the
-- names of generated deriving code live.
--
-- The tuple tables are functions of the arity because a tuple family is
-- infinite and its members need not share one naming scheme: the aihc core
-- libraries call the boxed ones @Unit@, @Solo@, @Tuple2@, and so on.
module Aihc.Tc.Wiring
  ( TcWiring (..),
    tupleTyCon,
    tupleDataCon,
  )
where

import Aihc.Parser.Syntax (TupleFlavor (..))
import Aihc.Tc.Types (TyCon)
import Data.Text (Text)

-- | The type constructors of the built-in syntactic forms, and the names
-- the type checker mentions on its own.
data TcWiring = TcWiring
  { -- | The boxed tuple type of each arity, such as @Tuple2@ for @(a, b)@.
    tcWiringBoxedTupleTyCon :: Int -> TyCon,
    -- | The boxed tuple data constructor of each arity. It is the same
    -- name in the term namespace, and it is what a promoted tuple such as
    -- @'(a, b)@ denotes.
    tcWiringBoxedTupleDataCon :: Int -> TyCon,
    -- | The unboxed tuple type of each arity, for @(# a, b #)@.
    tcWiringUnboxedTupleTyCon :: Int -> TyCon,
    -- | The unboxed tuple data constructor of each arity.
    tcWiringUnboxedTupleDataCon :: Int -> TyCon,
    -- | The unboxed sum type of each arity, for @(# a | b #)@.
    tcWiringUnboxedSumTyCon :: Int -> TyCon,
    -- | The list type constructor, for @[a]@ and list comprehensions.
    tcWiringListTyCon :: TyCon,
    -- | The empty-list data constructor, which a promoted @'[]@ also
    -- denotes.
    tcWiringNilDataCon :: TyCon,
    -- | The list cons data constructor, which a promoted @'(:)@ also
    -- denotes.
    tcWiringConsDataCon :: TyCon,
    -- | The function arrow, for @(->)@ used as a constructor.
    tcWiringArrowTyCon :: TyCon,
    -- | The kind of ordinary types, which @*@ and a bare @Type@ denote.
    tcWiringTypeTyCon :: TyCon,
    -- | The kind of constraints, which a bare @Constraint@ denotes.
    tcWiringConstraintTyCon :: TyCon,
    -- | The type of a guard and of an @if@ condition.
    tcWiringBoolTyCon :: TyCon,
    -- | The type of a character literal.
    tcWiringCharTyCon :: TyCon,
    -- | The nominal equality constraint @~@.
    tcWiringEqualityTyCon :: TyCon,
    -- | The representational equality class @Coercible@.
    tcWiringCoercibleTyCon :: TyCon,
    -- | The constraint constructor of one implicit parameter, such as
    -- @?x :: Int@. Each parameter name gets its own constructor.
    tcWiringImplicitParamTyCon :: Text -> TyCon,
    -- | An unlifted primitive type of one name, such as @Int#@. A foreign
    -- declaration marshals through these.
    tcWiringPrimitiveTyCon :: Text -> TyCon,
    -- | The application operator, as a module name and a term name. It is
    -- typed like an application rather than like a function, so that a
    -- higher-rank argument needs no impredicative instantiation.
    tcWiringApplyOperator :: (Text, Text),
    -- | Whether a class declared in one package and module is the
    -- Template Haskell @Lift@ class, whose parameters take implicit kind
    -- parameters. The package identity carries a version, so the test is
    -- a predicate rather than an identity.
    tcWiringIsLiftClass :: (Text, Text) -> Text -> Bool
  }

-- | The tables are functions, so a wiring shows as its name alone. The
-- type checker environment derives 'Show' for diagnostics.
instance Show TcWiring where
  show _ = "TcWiring"

-- | The tuple type constructor of one flavor and arity.
tupleTyCon :: TcWiring -> TupleFlavor -> Int -> TyCon
tupleTyCon wiring flavor =
  case flavor of
    Boxed -> tcWiringBoxedTupleTyCon wiring
    Unboxed -> tcWiringUnboxedTupleTyCon wiring

-- | The tuple data constructor of one flavor and arity.
tupleDataCon :: TcWiring -> TupleFlavor -> Int -> TyCon
tupleDataCon wiring flavor =
  case flavor of
    Boxed -> tcWiringBoxedTupleDataCon wiring
    Unboxed -> tcWiringUnboxedTupleDataCon wiring
