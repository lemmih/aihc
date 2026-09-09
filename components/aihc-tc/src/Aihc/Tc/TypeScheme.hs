-- | Parsing and rigid comparison of type schemes.
--
-- This is the public boundary for clients that need to describe an expected
-- Haskell type. Parsing is delegated to @aihc-parser@ and semantic conversion
-- (including kind checking) is delegated to @aihc-tc@.
module Aihc.Tc.TypeScheme
  ( parseTypeScheme,
    typeSchemeFromType,
    equivalentTypeSchemes,
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | Compare two schemes by rigid unification: quantified variables may be
-- alpha-renamed, but their order, dependency, kinds, predicates, and body must
-- agree exactly.
equivalentTypeSchemes :: TypeScheme -> TypeScheme -> Bool
equivalentTypeSchemes (ForAll leftVars leftPredicates leftBody) (ForAll rightVars rightPredicates rightBody)
  | length leftVars /= length rightVars = False
  | length leftPredicates /= length rightPredicates = False
  | otherwise =
      let renaming = Map.fromList (zip (map tvUnique leftVars) (map tvUnique rightVars))
       in and (zipWith (equivalentType renaming `onTyVar`) leftVars rightVars)
            && and (zipWith (equivalentPred renaming) leftPredicates rightPredicates)
            && equivalentType renaming leftBody rightBody
  where
    onTyVar compareKinds left right = compareKinds (tvKind left) (tvKind right)

typeSchemeArity :: TypeScheme -> Int
typeSchemeArity (ForAll _ _ body) = go body
  where
    go (TcFunTy _ result) = 1 + go result
    go _ = 0

equivalentType :: Map Unique Unique -> TcType -> TcType -> Bool
equivalentType renaming left right =
  case (left, right) of
    (TcTyVar leftVar, TcTyVar rightVar) ->
      Map.lookup (tvUnique leftVar) renaming == Just (tvUnique rightVar)
    (TcArrowTy, TcArrowTy) -> True
    (TcMetaTv leftMeta, TcMetaTv rightMeta) -> leftMeta == rightMeta
    (TcTyCon leftCon leftArgs, TcTyCon rightCon rightArgs) ->
      leftCon == rightCon
        && length leftArgs == length rightArgs
        && and (zipWith (equivalentType renaming) leftArgs rightArgs)
    (TcFunTy leftArg leftResult, TcFunTy rightArg rightResult) ->
      equivalentType renaming leftArg rightArg
        && equivalentType renaming leftResult rightResult
    (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody) ->
      equivalentType renaming (tvKind leftVar) (tvKind rightVar)
        && equivalentType
          (Map.insert (tvUnique leftVar) (tvUnique rightVar) renaming)
          leftBody
          rightBody
    (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody) ->
      length leftPredicates == length rightPredicates
        && and (zipWith (equivalentPred renaming) leftPredicates rightPredicates)
        && equivalentType renaming leftBody rightBody
    (TcAppTy leftFunction leftArg, TcAppTy rightFunction rightArg) ->
      equivalentType renaming leftFunction rightFunction
        && equivalentType renaming leftArg rightArg
    _ -> False

equivalentPred :: Map Unique Unique -> Pred -> Pred -> Bool
equivalentPred renaming left right =
  case (left, right) of
    (ClassPred leftClass leftArgs, ClassPred rightClass rightArgs) ->
      leftClass == rightClass
        && length leftArgs == length rightArgs
        && and (zipWith (equivalentType renaming) leftArgs rightArgs)
    (EqPred leftA leftB, EqPred rightA rightB) ->
      equivalentType renaming leftA rightA && equivalentType renaming leftB rightB
    (IParamPred leftName leftPayload, IParamPred rightName rightPayload) ->
      leftName == rightName && equivalentType renaming leftPayload rightPayload
    (QuantifiedPred leftVariables leftAntecedents leftConsequent, QuantifiedPred rightVariables rightAntecedents rightConsequent) ->
      length leftVariables == length rightVariables
        && and (zipWith (equivalentType renaming `onKind`) leftVariables rightVariables)
        && length leftAntecedents == length rightAntecedents
        && and (zipWith (equivalentPred quantifiedRenaming) leftAntecedents rightAntecedents)
        && equivalentPred quantifiedRenaming leftConsequent rightConsequent
      where
        quantifiedRenaming =
          foldr
            (\(leftVariable, rightVariable) -> Map.insert (tvUnique leftVariable) (tvUnique rightVariable))
            renaming
            (zip leftVariables rightVariables)
        onKind comparison leftVariable rightVariable = comparison (tvKind leftVariable) (tvKind rightVariable)
    _ -> False
