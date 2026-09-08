{-# LANGUAGE OverloadedStrings #-}

-- | Solve constraints that are valid only inside a constructor pattern branch.
module Aihc.Tc.Generate.PatternBranch
  ( solvePatternBranch,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Generate.Pattern (PatternCheck (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve (SolveResult (..), solveWithImpls)
import Aihc.Tc.Solve.InertSet (InertSet (..))
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (unless)
import Data.Text qualified as T

solvePatternBranch :: SourceSpan -> PatternCheck -> TcType -> [Ct] -> TcM [Ct]
solvePatternBranch sourceSpan patternCheck branchResultType bodyWanteds
  | null (pcGivenCts patternCheck) && null (pcSkolems patternCheck) =
      pure (pcWantedCts patternCheck <> bodyWanteds)
  | otherwise = do
      level <- getTcLevel
      let givens = pcGivenCts patternCheck
          implication =
            Implication
              { implSkols = pcSkolems patternCheck,
                implGivenEvs = map ctEvVar givens,
                implGivenCts = givens,
                implWantedCts = pcWantedCts patternCheck <> bodyWanteds,
                implTcLevel = level,
                implInfo = CaseBranchOrigin sourceSpan
              }
      result <- solveWithImpls [] [implication]
      rejectEscapingPatternType sourceSpan (pcSkolems patternCheck) branchResultType
      -- The wanteds that wait on the enclosing scope continue outward.
      pure (inertDicts (srInerts result))

rejectEscapingPatternType :: SourceSpan -> [TyVarId] -> TcType -> TcM ()
rejectEscapingPatternType sourceSpan skolems outerType = do
  zonkedOuterType <- zonkType outerType
  let escaping = filter (`typeMentionsTyVar` zonkedOuterType) skolems
  unless (null escaping) $
    emitError
      sourceSpan
      ( OtherError
          ( "existential type variable escapes its pattern-match branch: "
              <> T.unpack (T.intercalate ", " (map tvName escaping))
          )
      )

typeMentionsTyVar :: TyVarId -> TcType -> Bool
typeMentionsTyVar target ty =
  case ty of
    TcTyVar tyVar -> tyVar == target
    TcMetaTv {} -> False
    TcArrowTy -> False
    TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments
    TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result
    TcForAllTy tyVar body -> tyVar /= target && typeMentionsTyVar target body
    TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body
    TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument

predicateMentionsTyVar :: TyVarId -> Pred -> Bool
predicateMentionsTyVar target predicate =
  case predicate of
    ClassPred _ arguments -> any (typeMentionsTyVar target) arguments
    EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right
    IParamPred _ payload -> typeMentionsTyVar target payload
    QuantifiedPred variables antecedents consequent ->
      target `notElem` variables
        && (any (predicateMentionsTyVar target) antecedents || predicateMentionsTyVar target consequent)
