module Test.ResolveFacts
  ( ResolveFacts (..),
    VarFact (..),
  )
where

import Data.Text (Text)
import Resolver.Types

data VarFact = VarFact
  { vfName :: Text,
    vfBinding :: Maybe Text,
    vfClass :: NameClass
  }
  deriving (Eq, Show)

data ResolveFacts = ResolveFacts
  { rfModuleName :: Maybe Text,
    rfDeclNames :: [Text],
    rfVars :: [VarFact],
    rfDiagnosticCodes :: [DiagnosticCode]
  }
  deriving (Eq, Show)
