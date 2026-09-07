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
    -- | The @(>>=)@ method of @Monad@, which sequences a derived @Read@
    -- parser that keeps its result.
    derivingBind :: !DerivingReference,
    -- | The @(>>)@ method of @Monad@, which sequences a derived @Read@
    -- parser that drops its result.
    derivingThen :: !DerivingReference,
    -- | The @return@ method of @Monad@, which delivers the parsed value.
    derivingReturn :: !DerivingReference,
    -- | @parens@, which accepts the optional parentheses around a value.
    derivingReadParens :: !DerivingReference,
    -- | @prec@, which sets the precedence context of one alternative.
    derivingReadPrecContext :: !DerivingReference,
    -- | @step@, which reads one field above the constructor precedence.
    derivingReadStep :: !DerivingReference,
    -- | @reset@, which reads a record field at the lowest precedence.
    derivingReadReset :: !DerivingReference,
    -- | @(+++)@, which offers the alternatives of a datatype.
    derivingReadAlternative :: !DerivingReference,
    -- | @pfail@, the parser of a datatype without constructors.
    derivingReadFail :: !DerivingReference,
    -- | @expectP@, which accepts one expected lexeme.
    derivingReadExpect :: !DerivingReference,
    -- | @readField@, which accepts @label =@ before a record field.
    derivingReadField :: !DerivingReference,
    -- | @readSymField@, which accepts @(op) =@ before a record field with
    -- a symbolic label.
    derivingReadSymField :: !DerivingReference,
    -- | The @Ident@ lexeme constructor, for a constructor name.
    derivingLexemeIdent :: !DerivingReference,
    -- | The @Symbol@ lexeme constructor, for an operator name.
    derivingLexemeSymbol :: !DerivingReference,
    -- | The @Punc@ lexeme constructor, for punctuation.
    derivingLexemePunc :: !DerivingReference,
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
      derivingBind = term prim "GHC.Prim.Base" NameVarSym ">>=",
      derivingThen = term prim "GHC.Prim.Base" NameVarSym ">>",
      derivingReturn = term prim "GHC.Prim.Base" NameVarId "return",
      derivingReadParens = term prim readModule NameVarId "parens",
      derivingReadPrecContext = term prim readModule NameVarId "prec",
      derivingReadStep = term prim readModule NameVarId "step",
      derivingReadReset = term prim readModule NameVarId "reset",
      derivingReadAlternative = term prim readModule NameVarSym "+++",
      derivingReadFail = term prim readModule NameVarId "pfail",
      derivingReadExpect = term prim readModule NameVarId "expectP",
      derivingReadField = term prim readModule NameVarId "readField",
      derivingReadSymField = term prim readModule NameVarId "readSymField",
      derivingLexemeIdent = term prim readModule NameConId "Ident",
      derivingLexemeSymbol = term prim readModule NameConId "Symbol",
      derivingLexemePunc = term prim readModule NameConId "Punc",
      derivingStockClasses = coreStockClasses
    }
  where
    readModule = "GHC.Prim.Read"
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
    ("GHC.Internal.Read", "Read"),
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
    derivingCons references,
    derivingBind references,
    derivingThen references,
    derivingReturn references,
    derivingReadParens references,
    derivingReadPrecContext references,
    derivingReadStep references,
    derivingReadReset references,
    derivingReadAlternative references,
    derivingReadFail references,
    derivingReadExpect references,
    derivingReadField references,
    derivingReadSymField references,
    derivingLexemeIdent references,
    derivingLexemeSymbol references,
    derivingLexemePunc references
  ]
