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
      let givens = pcGivenCts patternCheck
          implication =
            Implication
              { implSkols = pcSkolems patternCheck,
                implGivenCts = givens,
                implWantedCts = pcWantedCts patternCheck <> bodyWanteds
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
