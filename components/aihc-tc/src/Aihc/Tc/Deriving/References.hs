{-# LANGUAGE OverloadedStrings #-}

-- | The library names that generated deriving code refers to.
--
-- A derived instance is ordinary surface syntax, so its method bodies
-- mention library values and types such as @True@, @showParen@, or @Int#@.
-- The type checker does not know where those live: the compiler that
-- embeds it says so through a 'DerivingReferences' table in its
-- configuration. Class methods such as @(==)@ or @showsPrec@ need no entry,
-- because the class being derived says where they are.
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
    -- | @showParen :: Bool -> ShowS -> ShowS@.
    derivingShowParen :: !DerivingReference,
    -- | @showString :: String -> ShowS@.
    derivingShowString :: !DerivingReference,
    -- | The packages whose classes count as standard for stock deriving.
    derivingStockPackages :: ![PackageId]
  }
  deriving (Eq, Show)

-- | The table for the aihc core libraries, given the identities of the
-- @aihc-prim@ and @aihc-base@ packages.
defaultDerivingReferences :: PackageId -> PackageId -> DerivingReferences
defaultDerivingReferences prim base =
  DerivingReferences
    { derivingTrue = term prim "GHC.Types" NameConId "True",
      derivingFalse = term prim "GHC.Types" NameConId "False",
      derivingLT = term prim "GHC.Types" NameConId "LT",
      derivingEQ = term prim "GHC.Types" NameConId "EQ",
      derivingGT = term prim "GHC.Types" NameConId "GT",
      derivingIntCon = term prim "GHC.Types" NameConId "I#",
      derivingIntPrimType = DerivingReference prim "GHC.Prim" "Int#" NameConId ResolutionNamespaceType,
      derivingGreaterOrEqual = term prim "GHC.Classes" NameVarSym ">=",
      derivingShowParen = term base "GHC.Show" NameVarId "showParen",
      derivingShowString = term base "GHC.Show" NameVarId "showString",
      derivingStockPackages = [prim, base]
    }
  where
    term package moduleName nameType name =
      DerivingReference package moduleName name nameType ResolutionNamespaceTerm

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
    derivingShowParen references,
    derivingShowString references
  ]
