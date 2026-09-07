-- | Lir, the low-level intermediate language between GC-GRIN and the machine
-- backends. See @docs/lir.md@.
module Aihc.Lir
  ( module Aihc.Lir.Syntax,
    LirParseError,
    parseModule,
    renderParseError,
    prettyModule,
    renderModule,
    LintError (..),
    lintModule,
    renderLintError,
    LoadError (..),
    renderLoadError,
    expandIncludes,
    loadModule,
    resolveConstants,
    Value (..),
    InterpretError (..),
    runFunction,
    renderValue,
    renderValues,
    renderInterpretError,
  )
where

import Aihc.Lir.Interpret
import Aihc.Lir.Lint
import Aihc.Lir.Parser
import Aihc.Lir.Pretty (prettyModule, renderModule)
import Aihc.Lir.Resolve
import Aihc.Lir.Syntax
