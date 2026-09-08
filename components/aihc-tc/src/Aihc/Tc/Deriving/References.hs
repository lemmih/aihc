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
    -- | The classes that stock deriving writes code for, as the package
    -- and module that define each and its name. All three must agree, so a
    -- user module that repeats a core-library module name does not make
    -- its own class stock. Every entry names the primitive package,
    -- because each of these classes is declared there.
    derivingStockClasses :: ![(PackageId, Text, Text)],
    -- | The remaining stock classes of GHC, as the module that defines each
    -- and its name. The generator writes no code for them: it reports that
    -- stock deriving of the class is not supported and produces no
    -- instance. A wrong match therefore cannot produce wrong code, so these
    -- entries need no package and the table stays free of base-library
    -- package identities.
    derivingRecognizedClasses :: ![(Text, Text)]
  }
  deriving (Eq, Show)

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
