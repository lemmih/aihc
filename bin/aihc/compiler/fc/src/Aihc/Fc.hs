-- | System FC language.
module Aihc.Fc
  ( module Aihc.Fc.Syntax,
    module Aihc.Fc.Name,
    renderProgram,
    renderType,
    renderExpr,
    parseProgram,
    renderParseError,
    FcParseError,
    tidyProgram,
    desugarModuleFc,
    DesugarConfig (..),
    moduleDesugarConfig,
    allPublicDesugarConfig,
    FcDesugarResult (..),
    lintProgram,
    loadScopeClosure,
    ModuleLoader,
    storeModuleLoader,
    LintError (..),
  )
where

import Aihc.Fc.Desugar (DesugarConfig (..), FcDesugarResult (..), allPublicDesugarConfig, desugarModuleFc, moduleDesugarConfig)
import Aihc.Fc.Lint (LintError (..), ModuleLoader, lintProgram, loadScopeClosure, storeModuleLoader)
import Aihc.Fc.Name
import Aihc.Fc.Parser (FcParseError, parseProgram, renderParseError)
import Aihc.Fc.Pretty (renderExpr, renderProgram, renderType)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
