{-# LANGUAGE OverloadedStrings #-}

-- | The library names that generated deriving code refers to, and the
-- classes that stock deriving knows.
--
-- A derived instance is ordinary surface syntax, so its method bodies
-- mention library values and types such as @True@, @(:)@, or @Int#@. The
-- type checker does not know where those live: the compiler that embeds it
-- says so through a 'DerivingReferences' table in its configuration. Every
-- name comes from the primitive package; class methods such as @(==)@ or
-- @showsPrec@ need no entry, because the class being derived says where
-- they are.
module Aihc.Tc.Deriving.References
  ( DerivingReference (..),
    DerivingReferences (..),
    defaultDerivingReferences,
    derivingReferenceList,
  )
where

import Aihc.Parser.Syntax (NameType (..))
import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Data.Text (Text)

-- | The resolved identity of one library name.
data DerivingReference = DerivingReference
  { referencePackage :: !PackageId,
    referenceModule :: !Text,
    referenceName :: !Text,
    referenceNameType :: !NameType,
    referenceNamespace :: !ResolutionNamespace
  }
  deriving (Eq, Show)

-- | Every library name that a generated instance body may mention.
data DerivingReferences = DerivingReferences
  { -- | The @True@ constructor of @Bool@.
    derivingTrue :: !DerivingReference,
    -- | The @False@ constructor of @Bool@.
    derivingFalse :: !DerivingReference,
    -- | The @LT@ constructor of @Ordering@.
    derivingLT :: !DerivingReference,
    -- | The @EQ@ constructor of @Ordering@.
    derivingEQ :: !DerivingReference,
    -- | The @GT@ constructor of @Ordering@.
    derivingGT :: !DerivingReference,
    -- | The @I#@ constructor that boxes an @Int#@ into an @Int@.
    derivingIntCon :: !DerivingReference,
    -- | The primitive @Int#@ type, which types the precedence literals of
    -- derived @Show@ instances.
    derivingIntPrimType :: !DerivingReference,
    -- | The @(>=)@ method of @Ord@, compared on @Int@ precedences.
    derivingGreaterOrEqual :: !DerivingReference,
    -- | The list constructor @(:)@, which derived @Show@ renders through.
    derivingCons :: !DerivingReference,
    -- | The classes GHC's stock deriving mechanisms know about, as the
    -- module that defines each and its name. A class elsewhere with the
    -- same name is not stock.
    derivingStockClasses :: ![(Text, Text)]
  }
  deriving (Eq, Show)

-- | The table for the aihc core libraries, given the identity of the
-- @aihc-prim@ package.
defaultDerivingReferences :: PackageId -> DerivingReferences
defaultDerivingReferences prim =
  DerivingReferences
    { derivingTrue = term prim "GHC.Types" NameConId "True",
      derivingFalse = term prim "GHC.Types" NameConId "False",
      derivingLT = term prim "GHC.Types" NameConId "LT",
      derivingEQ = term prim "GHC.Types" NameConId "EQ",
      derivingGT = term prim "GHC.Types" NameConId "GT",
      derivingIntCon = term prim "GHC.Types" NameConId "I#",
      derivingIntPrimType = DerivingReference prim "GHC.Prim" "Int#" NameConId ResolutionNamespaceType,
      derivingGreaterOrEqual = term prim "GHC.Classes" NameVarSym ">=",
      derivingCons = term prim "GHC.Types" NameConSym ":",
      derivingStockClasses = coreStockClasses
    }
  where
    term package moduleName nameType name =
      DerivingReference package moduleName name nameType ResolutionNamespaceTerm

-- | The stock classes of the aihc core libraries, where they are defined.
-- GHC keeps the same list as known-key names.
coreStockClasses :: [(Text, Text)]
coreStockClasses =
  [ ("GHC.Classes", "Eq"),
    ("GHC.Classes", "Ord"),
    ("GHC.Prim.Enum", "Enum"),
    ("GHC.Enum", "Bounded"),
    ("GHC.Show", "Show"),
    ("Prelude", "Read"),
    ("GHC.Ix", "Ix"),
    ("GHC.Prim.Base", "Functor"),
    ("GHC.Internal.Foldable", "Foldable"),
    ("GHC.Internal.Traversable", "Traversable"),
    ("Data.Data", "Data"),
    ("Type.Reflection", "Typeable"),
    ("GHC.Generics", "Generic"),
    ("GHC.Generics", "Generic1"),
    ("GHC.Internal.TH.Lift", "Lift")
  ]

-- | Every reference in the table, for callers that make the names visible
-- to later compiler phases.
derivingReferenceList :: DerivingReferences -> [DerivingReference]
derivingReferenceList references =
  [ derivingTrue references,
    derivingFalse references,
    derivingLT references,
    derivingEQ references,
    derivingGT references,
    derivingIntCon references,
    derivingIntPrimType references,
    derivingGreaterOrEqual references,
    derivingCons references
  ]
