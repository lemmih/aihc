-- | Named constants and includes. See the "Constants" section of
-- @docs/lir.md@.
--
-- A module names its constants with @const@ items and takes the constants of
-- another file with @include@ items. 'expandIncludes' replaces every include
-- with the constants of the file it names, and 'resolveConstants' substitutes
-- every reference to a constant with its value and drops the definitions, so
-- a backend sees a module without constants.
module Aihc.Lir.Resolve
  ( LoadError (..),
    renderLoadError,
    expandIncludes,
    loadModule,
    resolveConstants,
    unresolvedConstant,
    resolvedSwitchCaseValue,
  )
where

import Aihc.Lir.Parser (LirParseError, parseModule, renderParseError)
import Aihc.Lir.Pretty (prettySymbol, renderDoc)
import Aihc.Lir.Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath (takeDirectory, (</>))

data LoadError
  = LoadParseError !FilePath !LirParseError
  | -- | The chain of includes that returns to a file already on it.
    LoadIncludeCycle ![FilePath]
  | -- | An included file holds an item that is not a constant or an include.
    LoadIncludeItem !FilePath !Symbol
  deriving (Eq, Show)

renderLoadError :: LoadError -> String
renderLoadError err =
  case err of
    LoadParseError path parseError -> path <> ": Lir parse failed: " <> renderParseError parseError
    LoadIncludeCycle chain -> "include cycle: " <> unwords (map show chain)
    LoadIncludeItem path symbol -> path <> ": included file defines " <> T.unpack (renderDoc (prettySymbol symbol)) <> ", which is not a constant"

-- | Read, parse, and expand the includes of the module at @path@.
loadModule :: FilePath -> IO (Either LoadError Module)
loadModule path = do
  text <- TIO.readFile path
  case parseModule text of
    Left err -> pure (Left (LoadParseError path err))
    Right lirModule -> expandIncludes TIO.readFile path lirModule

-- | Replace every @include@ item of a module with the constants of the file
-- it names. An include path is relative to the directory of the file that
-- holds it, and @path@ is that file. An included file may itself include
-- files; it may hold nothing but constants and includes. The reader supplies
-- the text of a file, so a test can run this without a file system.
expandIncludes :: (FilePath -> IO Text) -> FilePath -> Module -> IO (Either LoadError Module)
expandIncludes reader path = fmap (fmap Module) . expandItems path [] . moduleItems
  where
    -- @current@ is the file whose items these are, and @chain@ the files
    -- that include it, innermost first.
    expandItems current chain items = concatEither <$> mapM (expandItem current chain) items
    expandItem current chain item =
      case item of
        ItemInclude relative -> includeFile (current : chain) (takeDirectory current </> T.unpack relative)
        _ -> pure (Right [item])
    includeFile chain included
      | included `elem` chain = pure (Left (LoadIncludeCycle (reverse (included : chain))))
      | otherwise = do
          text <- reader included
          case parseModule text of
            Left err -> pure (Left (LoadParseError included err))
            Right (Module items) ->
              case [symbol | item <- items, Just symbol <- [definedSymbol item]] of
                symbol : _ -> pure (Left (LoadIncludeItem included symbol))
                [] -> expandItems included chain items
    definedSymbol item =
      case item of
        ItemFunction function -> Just (functionName function)
        ItemExternFunction external -> Just (externFunctionName external)
        ItemGlobal global -> Just (globalName global)
        ItemData dataItem -> Just (dataName dataItem)
        ItemExternData symbol -> Just symbol
        ItemConstant _ -> Nothing
        ItemInclude _ -> Nothing
    concatEither = fmap concat . sequence

-- | Substitute the value of every constant the module defines for each
-- reference to it, and drop the definitions. A reference to a symbol that is
-- not a constant stays as it is; the linter reports one that names nothing.
resolveConstants :: Module -> Module
resolveConstants (Module items) = Module [resolveItem item | item <- items, not (isConstant item)]
  where
    constants :: Map Symbol Integer
    constants = Map.fromList [(constantName constant, constantValue constant) | ItemConstant constant <- items]
    isConstant item =
      case item of
        ItemConstant _ -> True
        _ -> False
    resolveItem item =
      case item of
        ItemFunction function -> ItemFunction function {functionBlocks = map resolveBlock (functionBlocks function)}
        ItemData dataItem -> ItemData dataItem {dataFields = map resolveField (dataFields dataItem)}
        _ -> item
    resolveField field =
      case field of
        DataIntConstant ty symbol | Just value <- Map.lookup symbol constants -> DataInt ty value
        DataWordConstant symbol | Just value <- Map.lookup symbol constants -> DataWord value
        _ -> field
    resolveBlock block =
      block
        { blockInstructions = [instruction {instructionOperation = resolveOperation (instructionOperation instruction)} | instruction <- blockInstructions block],
          blockTerminator = resolveTerminator (blockTerminator block)
        }
    resolveOperation operation =
      case operation of
        Binary op ty left right -> Binary op ty (operand left) (operand right)
        Unary op ty value -> Unary op ty (operand value)
        Wide op ty left right -> Wide op ty (operand left) (operand right)
        Compare op ty left right -> Compare op ty (operand left) (operand right)
        FloatBinary op ty left right -> FloatBinary op ty (operand left) (operand right)
        FloatUnary op ty value -> FloatUnary op ty (operand value)
        Convert op from value to -> Convert op from (operand value) to
        PtrToInt value -> PtrToInt (operand value)
        PtrFromInt value -> PtrFromInt (operand value)
        Select ty condition whenTrue whenFalse -> Select ty (operand condition) (operand whenTrue) (operand whenFalse)
        Load ty address align -> Load ty (resolveAddress address) align
        Store ty value address align -> Store ty (operand value) (resolveAddress address) align
        PtrAdd base offset -> PtrAdd (operand base) (operand offset)
        StackAlloc _ _ -> operation
        GlobalGet _ -> operation
        GlobalSet symbol value -> GlobalSet symbol (operand value)
        Call symbol arguments -> Call symbol (map operand arguments)
        CallIndirect callee arguments signature -> CallIndirect (operand callee) (map operand arguments) signature
    resolveAddress address = address {addressBase = operand (addressBase address)}
    resolveCase switchCase =
      let target = resolveTarget (switchCaseTarget switchCase)
       in case switchCase of
            SwitchCase value _ -> SwitchCase value target
            SwitchCaseConstant symbol _ ->
              maybe (SwitchCaseConstant symbol target) (`SwitchCase` target) (Map.lookup symbol constants)

    resolveTerminator terminator =
      case terminator of
        Jump target -> Jump (resolveTarget target)
        Branch condition whenTrue whenFalse -> Branch (operand condition) (resolveTarget whenTrue) (resolveTarget whenFalse)
        Switch ty scrutinee cases fallback ->
          Switch ty (operand scrutinee) [resolveCase switchCase | switchCase <- cases] (fmap resolveTarget fallback)
        Return values -> Return (map operand values)
        TailCall symbol arguments -> TailCall symbol (map operand arguments)
        TailCallIndirect callee arguments signature -> TailCallIndirect (operand callee) (map operand arguments) signature
        Trap _ -> terminator
    resolveTarget target = target {targetArguments = map operand (targetArguments target)}
    operand value =
      case value of
        OperandLiteral (LitSymbol symbol) | Just constant <- Map.lookup symbol constants -> OperandLiteral (LitInt constant)
        _ -> value

-- | The linter rejects a reference to an undefined constant and
-- 'resolveConstants' substitutes every defined one, so a backend that meets
-- a reference after both is looking at a bug in this pipeline, not at a
-- module a user can fix.
unresolvedConstant :: Symbol -> a
unresolvedConstant symbol = error ("Lir constant " <> T.unpack (renderDoc (prettySymbol symbol)) <> " reached a backend unresolved")

-- | Read a switch label after constant resolution.
resolvedSwitchCaseValue :: SwitchCase -> Integer
resolvedSwitchCaseValue (SwitchCase value _) = value
resolvedSwitchCaseValue (SwitchCaseConstant symbol _) = unresolvedConstant symbol
