-- | The identities of the type constructors that built-in syntax denotes.
--
-- Source syntax such as @(a, b)@ or @(# a, b #)@ names no module, so the
-- type checker cannot resolve it. The compiler that embeds the type checker
-- says which type constructor each form stands for through a 'TcWiring'
-- table in its configuration, the same way 'Aihc.Tc.Deriving.References'
-- says where the names of generated deriving code live.
--
-- The tables are functions of the arity because a tuple family is infinite
-- and its members need not share one naming scheme: the aihc core libraries
-- call the boxed ones @Unit@, @Solo@, @Tuple2@, and so on.
module Aihc.Tc.Wiring
  ( TcWiring (..),
    tupleTyCon,
    tupleDataCon,
  )
where

import Aihc.Parser.Syntax (TupleFlavor (..))
import Aihc.Tc.Types (TyCon)

-- | The type constructors of the built-in syntactic forms.
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
    tcWiringUnboxedTupleDataCon :: Int -> TyCon
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
