-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    normalizeGrinProgram,
    normalizeGrinExpr,
    CpsGrinProgram,
    CpsGrinError (..),
    ContinuationFrameKind (..),
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
    GcGrinProgram,
    entryGcProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
    lowerGc,
    normalizeHeapReservations,
    lowerProgram,
    lintProgram,
    lintCpsProgram,
    lintGcProgram,
    GrinLintError (..),
    GrinParseError,
    parseProgram,
    parseExpr,
    renderParseError,
    prettyProgram,
    prettyExpr,
    ProgramStreams (..),
    interpretProgramBinding,
    interpretProgramIoBinding,
    interpretProgramFunctionSnapshot,
    InterpretError (..),
    RuntimeValue (..),
    HeapSnapshot (..),
    SnapshotValue (..),
    SnapshotCell (..),
    renderSnapshotReturn,
    renderSnapshotHeap,
    renderHeapSnapshot,
  )
where

import Aihc.Grin.Anf (normalizeGrinExpr, normalizeGrinProgram)
import Aihc.Grin.Cps
  ( ContinuationFrameKind (..),
    CpsGrinError (..),
    CpsGrinProgram,
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
  )
import Aihc.Grin.Gc (GcGrinProgram, entryGcProgram, gcContinuationFrames, gcContinuationFunctions, gcFunctionContinuations, gcGrinProgram, gcUpdateFunction, lowerGc)
import Aihc.Grin.Heap (normalizeHeapReservations)
import Aihc.Grin.Interpret (InterpretError (..), ProgramStreams (..), RuntimeValue (..), interpretProgramBinding, interpretProgramFunctionSnapshot, interpretProgramIoBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintCpsProgram, lintGcProgram, lintProgram)
import Aihc.Grin.Lower (lowerProgram)
import Aihc.Grin.Parser (GrinParseError, parseExpr, parseProgram, renderParseError)
import Aihc.Grin.Pretty (prettyExpr, prettyProgram)
import Aihc.Grin.Snapshot
import Aihc.Grin.Syntax
