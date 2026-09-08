-- | Parse and inspect type schemes.
--
-- This is the public boundary for clients that need to describe an expected
-- Haskell type. Parsing is delegated to @aihc-parser@ and semantic conversion
-- (including kind checking) is delegated to @aihc-tc@.
module Aihc.Tc.TypeScheme
  ( parseTypeScheme,
    typeSchemeFromType,
    typeSchemeArity,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseSignatureType)
import Aihc.Parser.Syntax (Extension (ExplicitForAll, KindSignatures, MagicHash, UnboxedTuples))
import Aihc.Tc.Error (TcDiagnostic (..), TcSeverity (TcError))
import Aihc.Tc.Kind (sigToScheme)
import Aihc.Tc.Monad (TcConfig, emptyTcEnv, initTcState, runTcM, tcAbortMessage, tcsDiagnostics)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (defaultTypeSchemeKinds)
import Data.Text (Text)

-- | Parse and kind-check a standalone type scheme.
parseTypeScheme :: TcConfig -> Text -> Either String TypeScheme
parseTypeScheme config source =
  case parseSignatureType parserConfig source of
    ParseErr errors -> Left (show errors)
    ParseOk surfaceType ->
      case runTcM (emptyTcEnv config) initTcState $ sigToScheme surfaceType >>= defaultTypeSchemeKinds of
        Left abort -> Left (tcAbortMessage abort)
        Right (scheme, state) ->
          case [diagnostic | diagnostic <- tcsDiagnostics state, diagSeverity diagnostic == TcError] of
            [] -> Right scheme
            errors -> Left (show errors)
  where
    parserConfig =
      defaultConfig
        { parserExtensions = [ExplicitForAll, KindSignatures, MagicHash, UnboxedTuples]
        }

-- | Recover a scheme from the explicit forall and context nodes carried by a
-- type-checker type.
typeSchemeFromType :: TcType -> TypeScheme
typeSchemeFromType = go [] []
  where
    go tyVars predicates ty =
      case ty of
        TcForAllTy tyVar body -> go (tyVars <> [tyVar]) predicates body
        TcQualTy morePredicates body -> go tyVars (predicates <> morePredicates) body
        _ -> ForAll tyVars predicates ty

typeSchemeArity :: TypeScheme -> Int
typeSchemeArity (ForAll _ _ body) = go body
  where
    go (TcFunTy _ result) = 1 + go result
    go _ = 0
