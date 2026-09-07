-- | Structural and type validation for Lir modules. See the "Lint" section
-- of @docs/lir.md@.
module Aihc.Lir.Lint
  ( LintError (..),
    lintModule,
    renderLintError,
  )
where

import Aihc.Lir.Pretty (binaryOpName, compareOpName, convertOpName, floatBinaryOpName, floatUnaryOpName, prettyLabel, prettyLiteral, prettyQuoted, prettySymbol, prettyType, prettyVar, renderDoc, unaryOpName, wideOpName)
import Aihc.Lir.Syntax
import Data.Bits (popCount)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, comma, hsep, punctuate)

data LintError = LintError
  { lintErrorSymbol :: !(Maybe Symbol),
    lintErrorBlock :: !(Maybe Label),
    lintErrorMessage :: !Text
  }
  deriving (Eq, Show)

-- | @\@symbol/block: message@, with the parts that are known.
renderLintError :: LintError -> Text
renderLintError err =
  case (lintErrorSymbol err, lintErrorBlock err) of
    (Nothing, _) -> lintErrorMessage err
    (Just symbol, Nothing) -> renderDoc (prettySymbol symbol) <> ": " <> lintErrorMessage err
    (Just symbol, Just block) -> renderDoc (prettySymbol symbol) <> "/" <> renderDoc (prettyLabel block) <> ": " <> lintErrorMessage err

data SymbolInfo
  = SymbolFunction !Signature
  | SymbolGlobal !Type
  | SymbolData
  | SymbolConstant !Integer
  deriving (Eq, Show)

type Symbols = Map Symbol SymbolInfo

lintModule :: Module -> [LintError]
lintModule (Module items) =
  duplicateErrors <> concatMap (lintItem symbols) items
  where
    (symbols, duplicateErrors) = foldl' addSymbol (Map.empty, []) items
    addSymbol (table, errors) item =
      case itemSymbol item of
        Nothing -> (table, errors)
        Just (symbol, info)
          | Map.member symbol table -> (table, errors <> [LintError (Just symbol) Nothing ("duplicate definition of " <> renderSymbol symbol)])
          | otherwise -> (Map.insert symbol info table, errors)

-- | The symbol an item defines or declares. An include names no symbol.
itemSymbol :: Item -> Maybe (Symbol, SymbolInfo)
itemSymbol item =
  case item of
    ItemFunction function -> Just (functionName function, SymbolFunction (functionSignature function))
    ItemExternFunction external -> Just (externFunctionName external, SymbolFunction (externFunctionSignature external))
    ItemGlobal global -> Just (globalName global, SymbolGlobal (globalType global))
    ItemData dataItem -> Just (dataName dataItem, SymbolData)
    ItemExternData symbol -> Just (symbol, SymbolData)
    ItemConstant constant -> Just (constantName constant, SymbolConstant (constantValue constant))
    ItemInclude _ -> Nothing

lintItem :: Symbols -> Item -> [LintError]
lintItem symbols item =
  case item of
    ItemFunction function -> lintFunction symbols function
    ItemExternFunction external ->
      map (LintError (Just (externFunctionName external)) Nothing) (signatureErrors (externFunctionSignature external))
    ItemGlobal _ -> []
    ItemData dataItem -> map (LintError (Just (dataName dataItem)) Nothing) (lintData symbols dataItem)
    ItemExternData _ -> []
    ItemConstant _ -> []
    -- "Aihc.Lir.Resolve" expands includes before a module reaches the linter.
    ItemInclude path -> [LintError Nothing Nothing ("include " <> renderDoc (prettyQuoted path) <> " is not expanded")]

signatureErrors :: Signature -> [Text]
signatureErrors signature =
  [ "the c convention permits at most one result"
  | signatureConvention signature == CConvention,
    length (signatureResults signature) > 1
  ]

lintData :: Symbols -> DataItem -> [Text]
lintData symbols dataItem =
  alignmentErrors (dataAlignment dataItem) <> concatMap fieldErrors (dataFields dataItem)
  where
    fieldErrors field =
      case field of
        DataInt ty _
          | isIntegerType ty -> []
          | otherwise -> ["data field type " <> renderType ty <> " is not an integer type"]
        DataIntConstant ty symbol
          | isIntegerType ty -> constantErrors symbols ty symbol
          | otherwise -> ["data field type " <> renderType ty <> " is not an integer type"]
        DataFloat ty _
          | isFloatType ty -> []
          | otherwise -> ["data field type " <> renderType ty <> " is not a float type"]
        DataSymbol symbol _ -> dataSymbolErrors symbols symbol
        DataNull -> []
        DataWord value ->
          [ "data field word " <> tshow value <> " does not fit a 32-bit word"
          | not (literalFits I32 value)
          ]
        DataWordConstant symbol -> constantErrors symbols I32 symbol
        DataCode Nothing -> []
        DataCode (Just symbol) -> functionSymbolErrors symbols symbol
        DataBytes _ -> []
        DataZero _ -> []

-- | A constant reference names a constant whose value fits the type.
constantErrors :: Symbols -> Type -> Symbol -> [Text]
constantErrors symbols ty symbol =
  case Map.lookup symbol symbols of
    Just (SymbolConstant value) -> constantValueErrors ty symbol value
    Just _ -> [renderSymbol symbol <> " is not a constant"]
    Nothing -> ["unknown symbol " <> renderSymbol symbol]

constantValueErrors :: Type -> Symbol -> Integer -> [Text]
constantValueErrors ty symbol value =
  [ "constant " <> renderSymbol symbol <> " = " <> tshow value <> " does not fit " <> renderType ty
  | not (literalFits ty value)
  ]

-- | A @ptr@ literal or data field names a data object. A global has no
-- address and a function is a @code@ value.
dataSymbolErrors :: Symbols -> Symbol -> [Text]
dataSymbolErrors symbols symbol =
  case Map.lookup symbol symbols of
    Just SymbolData -> []
    Just _ -> [renderSymbol symbol <> " is not a data object"]
    Nothing -> ["unknown symbol " <> renderSymbol symbol]

-- | A @code@ literal or data field names a function.
functionSymbolErrors :: Symbols -> Symbol -> [Text]
functionSymbolErrors symbols symbol =
  case Map.lookup symbol symbols of
    Just (SymbolFunction _) -> []
    Just _ -> [renderSymbol symbol <> " is not a function"]
    Nothing -> ["unknown symbol " <> renderSymbol symbol]

alignmentErrors :: Integer -> [Text]
alignmentErrors alignment
  | alignment > 0 && popCount alignment == 1 = []
  | otherwise = ["alignment " <> tshow alignment <> " is not a power of two"]

-- Functions

data Definition = Definition
  { definitionType :: !(Maybe Type),
    definitionBlock :: !Int,
    definitionPosition :: !Int
  }

data Env = Env
  { envSymbols :: !Symbols,
    envFunction :: !Function,
    envBlockIndices :: !(Map Label Int),
    envDefinitions :: !(Map Var Definition),
    envDominators :: !(IntMap IntSet)
  }

-- | The block index and the instruction position of a use. Block parameters
-- have position @-1@.
data Location = Location !Int !Int

lintFunction :: Symbols -> Function -> [LintError]
lintFunction symbols function =
  map (LintError (Just (functionName function)) Nothing) (signatureErrors (functionSignature function))
    <> blockErrors
    <> definitionErrors
    <> concat (zipWith (lintBlock env) [0 ..] blocks)
  where
    blocks = functionBlocks function
    (blockIndices, blockErrors) = foldl' addBlock (Map.empty, []) (zip [0 ..] blocks)
    addBlock (table, errors) (index, block) =
      let name = blockLabel block
       in if Map.member name table
            then (table, errors <> [LintError (Just (functionName function)) (Just name) ("duplicate block " <> renderLabel name)])
            else (Map.insert name index table, errors)
    (definitions, definitionErrors) = collectDefinitions symbols function
    env =
      Env
        { envSymbols = symbols,
          envFunction = function,
          envBlockIndices = blockIndices,
          envDefinitions = definitions,
          envDominators = dominators blockIndices blocks
        }

collectDefinitions :: Symbols -> Function -> (Map Var Definition, [LintError])
collectDefinitions symbols function =
  foldl' addDefinition (Map.empty, []) allDefinitions
  where
    allDefinitions =
      [(var, Definition (Just ty) 0 (-1), Nothing) | (var, ty) <- functionParameters function]
        <> concat (zipWith blockDefinitions [0 ..] (functionBlocks function))
    blockDefinitions index block =
      [(var, Definition (Just ty) index (-1), Just (blockLabel block)) | (var, ty) <- blockParameters block]
        <> concat (zipWith (instructionDefinitions index (blockLabel block)) [0 ..] (blockInstructions block))
    instructionDefinitions index name position instruction =
      let types = operationResultTypes symbols (instructionOperation instruction)
          typeAt slot = types >>= \list -> if slot < length list then Just (list !! slot) else Nothing
       in [(var, Definition (typeAt slot) index position, Just name) | (slot, var) <- zip [0 ..] (instructionResults instruction)]
    addDefinition (table, errors) (var, definition, block) =
      if Map.member var table
        then (table, errors <> [LintError (Just (functionName function)) block ("duplicate definition of " <> renderVar var)])
        else (Map.insert var definition table, errors)

-- | The result types of an operation, when the operation itself determines
-- them. Type errors inside the operation are reported separately.
operationResultTypes :: Symbols -> Operation -> Maybe [Type]
operationResultTypes symbols operation =
  case operation of
    Binary _ ty _ _ -> Just [ty]
    Unary _ ty _ -> Just [ty]
    Wide op ty _ _ -> Just (if op `elem` [MulWideS, MulWideU] then [ty, ty] else [ty, I1])
    Compare {} -> Just [I1]
    FloatBinary _ ty _ _ -> Just [ty]
    FloatUnary _ ty _ -> Just [ty]
    Convert _ _ _ to -> Just [to]
    PtrToInt _ -> Just [I64]
    PtrFromInt _ -> Just [Ptr]
    Select ty _ _ _ -> Just [ty]
    Load ty _ _ -> Just [ty]
    Store {} -> Just []
    PtrAdd _ _ -> Just [Ptr]
    StackAlloc _ _ -> Just [Ptr]
    GlobalGet symbol ->
      case Map.lookup symbol symbols of
        Just (SymbolGlobal ty) -> Just [ty]
        _ -> Nothing
    GlobalSet _ _ -> Just []
    Call symbol _ ->
      case Map.lookup symbol symbols of
        Just (SymbolFunction signature) -> Just (signatureResults signature)
        _ -> Nothing
    CallIndirect _ _ signature -> Just (signatureResults signature)

-- | Dominator sets over block indices. Unreachable blocks are dominated by
-- every block.
dominators :: Map Label Int -> [Block] -> IntMap IntSet
dominators blockIndices blocks = fixpoint initial
  where
    fixpoint current =
      let next = step current
       in if next == current then current else fixpoint next
    count = length blocks
    allBlocks = IntSet.fromList [0 .. count - 1]
    successors = IntMap.fromList [(index, mapMaybe (`Map.lookup` blockIndices) (terminatorLabels (blockTerminator block))) | (index, block) <- zip [0 ..] blocks]
    reachable = go IntSet.empty [0]
      where
        go seen [] = seen
        go seen (next : rest)
          | IntSet.member next seen = go seen rest
          | otherwise = go (IntSet.insert next seen) (IntMap.findWithDefault [] next successors <> rest)
    predecessors =
      IntMap.fromListWith (<>) [(target, [source]) | (source, targets) <- IntMap.toList successors, IntSet.member source reachable, target <- targets]
    initial = IntMap.fromList [(index, if index == 0 then IntSet.singleton 0 else allBlocks) | index <- [0 .. count - 1]]
    step current =
      IntMap.mapWithKey
        ( \index doms ->
            if index == 0 || not (IntSet.member index reachable)
              then doms
              else
                IntSet.insert index (foldr IntSet.intersection allBlocks [current IntMap.! predecessor | predecessor <- IntMap.findWithDefault [] index predecessors])
        )
        current

terminatorLabels :: Terminator -> [Label]
terminatorLabels terminator =
  case terminator of
    Jump target -> [targetLabel target]
    Branch _ whenTrue whenFalse -> [targetLabel whenTrue, targetLabel whenFalse]
    Switch _ _ cases fallback -> map (targetLabel . switchCaseTarget) cases <> maybe [] (pure . targetLabel) fallback
    Return _ -> []
    TailCall _ _ -> []
    TailCallIndirect {} -> []
    Trap _ -> []

lintBlock :: Env -> Int -> Block -> [LintError]
lintBlock env index block =
  map located (entryErrors <> concat (zipWith (lintInstruction env index) [0 ..] (blockInstructions block)) <> lintTerminator env index (blockTerminator block))
  where
    located = LintError (Just (functionName (envFunction env))) (Just (blockLabel block))
    entryErrors =
      ["entry block " <> renderLabel (blockLabel block) <> " has parameters" | index == 0, not (null (blockParameters block))]

lintInstruction :: Env -> Int -> Int -> Instruction -> [Text]
lintInstruction env blockIndex position instruction =
  countErrors <> operationErrors
  where
    location = Location blockIndex position
    operation = instructionOperation instruction
    operationName = operationKeyword operation
    countErrors =
      case operationResultTypes (envSymbols env) operation of
        Just types
          | length types /= length (instructionResults instruction) ->
              [operationName <> " defines " <> tshow (length (instructionResults instruction)) <> " values, expected " <> tshow (length types)]
        _ -> []
    check = checkOperand env location
    rejects ty = [operationName <> " does not accept " <> renderType ty]
    operationErrors =
      case operation of
        Binary op ty left right ->
          let accepted = isIntegerType ty || (ty == I1 && op `elem` [And, Or, Xor])
           in (if accepted then [] else rejects ty) <> check ty left <> check ty right
        Unary _ ty value ->
          (if isIntegerType ty then [] else rejects ty) <> check ty value
        Wide _ ty left right ->
          (if isIntegerType ty then [] else rejects ty) <> check ty left <> check ty right
        Compare op ty left right ->
          (if compareAccepts op ty then [] else rejects ty) <> check ty left <> check ty right
        FloatBinary _ ty left right ->
          (if isFloatType ty then [] else rejects ty) <> check ty left <> check ty right
        FloatUnary _ ty value ->
          (if isFloatType ty then [] else rejects ty) <> check ty value
        Convert op from value to -> convertErrors op from to <> check from value
        PtrToInt value -> check Ptr value
        PtrFromInt value -> check I64 value
        Select ty condition left right -> check I1 condition <> check ty left <> check ty right
        Load _ address alignment -> check Ptr (addressBase address) <> alignmentErrors alignment
        Store ty value address alignment -> check ty value <> check Ptr (addressBase address) <> alignmentErrors alignment
        PtrAdd base offset -> check Ptr base <> check I64 offset
        StackAlloc _ alignment ->
          ["stack.alloc outside the entry block" | blockIndex /= 0] <> alignmentErrors alignment
        GlobalGet symbol -> globalErrors symbol
        GlobalSet symbol value ->
          case Map.lookup symbol (envSymbols env) of
            Just (SymbolGlobal ty) -> check ty value
            _ -> globalErrors symbol
        Call symbol arguments -> callErrors env location symbol arguments
        CallIndirect target arguments signature ->
          check Code target <> signatureErrors signature <> argumentErrors env location (renderOperand target) (signatureParameters signature) arguments
    globalErrors symbol =
      case Map.lookup symbol (envSymbols env) of
        Just (SymbolGlobal _) -> []
        Just _ -> [renderSymbol symbol <> " is not a global"]
        Nothing -> ["unknown symbol " <> renderSymbol symbol]

compareAccepts :: CompareOp -> Type -> Bool
compareAccepts op ty
  | op `elem` [Eq, Ne] = True
  | op `elem` [LtS, LeS, GtS, GeS] = isIntegerType ty
  | op `elem` [LtU, LeU, GtU, GeU] = isIntegerType ty || ty == Ptr
  | otherwise = isFloatType ty

convertErrors :: ConvertOp -> Type -> Type -> [Text]
convertErrors op from to =
  case op of
    SExt -> extension
    ZExt -> extension
    Trunc ->
      source (isIntegerType from)
        <> result (isIntegerType to || to == I1)
        <> [name <> " " <> renderType from <> " to " <> renderType to <> " requires a narrower target type" | typeBits to >= typeBits from]
    IToFS -> source (isIntegerType from) <> result (isFloatType to)
    IToFU -> source (isIntegerType from) <> result (isFloatType to)
    FToIS -> source (isFloatType from) <> result (isIntegerType to)
    FToIU -> source (isFloatType from) <> result (isIntegerType to)
    FpExt -> source (from == F32) <> result (to == F64)
    FpTrunc -> source (from == F64) <> result (to == F32)
    Bitcast ->
      let numeric ty = isIntegerType ty || isFloatType ty
       in source (numeric from)
            <> result (numeric to)
            <> [name <> " " <> renderType from <> " to " <> renderType to <> " requires equal widths" | numeric from, numeric to, typeBits from /= typeBits to]
            <> [name <> " " <> renderType from <> " to " <> renderType to <> " requires one integer and one float type" | numeric from, numeric to, isFloatType from == isFloatType to]
  where
    name = convertOpName op
    source accepted = [name <> " does not accept " <> renderType from | not accepted]
    result accepted = [name <> " does not produce " <> renderType to | not accepted]
    extension =
      source (isIntegerType from || from == I1)
        <> result (isIntegerType to)
        <> [name <> " " <> renderType from <> " to " <> renderType to <> " requires a wider target type" | typeBits to <= typeBits from]

callErrors :: Env -> Location -> Symbol -> [Operand] -> [Text]
callErrors env location symbol arguments =
  case Map.lookup symbol (envSymbols env) of
    Just (SymbolFunction signature) -> argumentErrors env location (renderSymbol symbol) (signatureParameters signature) arguments
    Just _ -> [renderSymbol symbol <> " is not a function"]
    Nothing -> ["unknown symbol " <> renderSymbol symbol]

argumentErrors :: Env -> Location -> Text -> [Type] -> [Operand] -> [Text]
argumentErrors env location callee parameters arguments
  | length parameters /= length arguments =
      [callee <> " expects " <> tshow (length parameters) <> " arguments, got " <> tshow (length arguments)]
  | otherwise = concat (zipWith (checkOperand env location) parameters arguments)

lintTerminator :: Env -> Int -> Terminator -> [Text]
lintTerminator env blockIndex terminator =
  case terminator of
    Jump target -> targetErrors target
    Branch condition whenTrue whenFalse -> check I1 condition <> targetErrors whenTrue <> targetErrors whenFalse
    Switch ty scrutinee cases fallback ->
      ["switch does not accept " <> renderType ty | not (isIntegerType ty)]
        <> check ty scrutinee
        <> caseErrors (envSymbols env) ty cases
        <> concatMap (targetErrors . switchCaseTarget) cases
        <> maybe [] targetErrors fallback
    Return values ->
      let expected = functionResults function
       in if length expected /= length values
            then ["return " <> tshow (length values) <> " values, expected " <> tshow (length expected)]
            else concat (zipWith check expected values)
    TailCall symbol arguments ->
      case Map.lookup symbol (envSymbols env) of
        Just (SymbolFunction signature) ->
          argumentErrors env location (renderSymbol symbol) (signatureParameters signature) arguments
            <> tailErrors ("tailcall " <> renderSymbol symbol) signature
        Just _ -> [renderSymbol symbol <> " is not a function"]
        Nothing -> ["unknown symbol " <> renderSymbol symbol]
    TailCallIndirect target arguments signature ->
      check Code target
        <> signatureErrors signature
        <> argumentErrors env location (renderOperand target) (signatureParameters signature) arguments
        <> tailErrors ("tailcall.indirect " <> renderOperand target) signature
    Trap _ -> []
  where
    function = envFunction env
    location = Location blockIndex maxBound
    check = checkOperand env location
    tailErrors callee signature =
      [ callee <> " has result types " <> renderTypes (signatureResults signature) <> ", expected " <> renderTypes (functionResults function)
      | signatureResults signature /= functionResults function
      ]
        <> [ callee <> " has calling convention " <> renderConvention (signatureConvention signature) <> ", expected " <> renderConvention (functionConvention function)
           | signatureConvention signature /= functionConvention function
           ]
    targetErrors (Target name arguments) =
      case Map.lookup name (envBlockIndices env) of
        Nothing -> ["unknown block " <> renderLabel name]
        Just 0 -> ["entry block " <> renderLabel name <> " has a predecessor"]
        Just index ->
          let parameters = map snd (blockParameters (functionBlocks function !! index))
           in if length parameters /= length arguments
                then ["block " <> renderLabel name <> " expects " <> tshow (length parameters) <> " arguments, got " <> tshow (length arguments)]
                else concat (zipWith check parameters arguments)

caseErrors :: Symbols -> Type -> [SwitchCase] -> [Text]
caseErrors symbols ty cases = snd (foldl' addCase (Set.empty, []) cases)
  where
    addCase :: (Set Integer, [Text]) -> SwitchCase -> (Set Integer, [Text])
    addCase state@(seen, errors) (SwitchCaseConstant symbol target) =
      case Map.lookup symbol symbols of
        Just (SymbolConstant value) -> addCase state (SwitchCase value target)
        Just _ -> (seen, errors <> [renderSymbol symbol <> " is not a constant"])
        Nothing -> (seen, errors <> ["unknown symbol " <> renderSymbol symbol])
    addCase (seen, errors) (SwitchCase value _)
      | not (literalFits ty value) = (seen, errors <> ["switch case " <> tshow value <> " does not fit " <> renderType ty])
      | Set.member canonical seen = (seen, errors <> ["duplicate switch case " <> tshow value])
      | otherwise = (Set.insert canonical seen, errors)
      where
        canonical = value `mod` (2 ^ typeBits ty)

-- | Check that an operand has the expected type and that its definition
-- dominates the use.
checkOperand :: Env -> Location -> Type -> Operand -> [Text]
checkOperand env (Location blockIndex position) expected operand =
  case operand of
    OperandVar var ->
      case Map.lookup var (envDefinitions env) of
        Nothing -> ["use of undefined value " <> renderVar var]
        Just definition ->
          [renderVar var <> " does not dominate this use" | not (dominatesUse definition)]
            <> [ renderVar var <> " has type " <> renderType actual <> ", expected " <> renderType expected
               | Just actual <- [definitionType definition],
                 actual /= expected
               ]
    OperandLiteral literal -> literalErrors env expected literal
  where
    dominatesUse definition
      | definitionBlock definition == blockIndex = definitionPosition definition < position
      | otherwise = IntSet.member (definitionBlock definition) (IntMap.findWithDefault IntSet.empty blockIndex (envDominators env))

literalErrors :: Env -> Type -> Literal -> [Text]
literalErrors env expected literal =
  case literal of
    LitInt value
      | isIntegerType expected || expected == I1 -> ["literal " <> tshow value <> " does not fit " <> renderType expected | not (literalFits expected value)]
      | isFloatType expected -> []
      | otherwise -> mismatch
    LitFloat _
      | isFloatType expected -> []
      | otherwise -> mismatch
    LitNull
      | expected `elem` [Ptr, Code] -> []
      | otherwise -> mismatch
    LitSymbol symbol
      | Just (SymbolConstant value) <- Map.lookup symbol (envSymbols env) ->
          if isIntegerType expected || expected == I1
            then constantValueErrors expected symbol value
            else mismatch
      | expected == Ptr -> dataSymbolErrors (envSymbols env) symbol
      | expected == Code -> functionSymbolErrors (envSymbols env) symbol
      | otherwise -> mismatch
  where
    mismatch = ["literal " <> renderDoc (prettyLiteral literal) <> " does not have type " <> renderType expected]

-- | A literal fits when it is in the signed range or the unsigned range.
literalFits :: Type -> Integer -> Bool
literalFits ty value = value >= negate (2 ^ (bits - 1)) && value < 2 ^ bits
  where
    bits = typeBits ty

operationKeyword :: Operation -> Text
operationKeyword operation =
  case operation of
    Binary op _ _ _ -> binaryOpName op
    Unary op _ _ -> unaryOpName op
    Wide op _ _ _ -> wideOpName op
    Compare op _ _ _ -> compareOpName op
    FloatBinary op _ _ _ -> floatBinaryOpName op
    FloatUnary op _ _ -> floatUnaryOpName op
    Convert op _ _ _ -> convertOpName op
    PtrToInt _ -> "ptr.to_int"
    PtrFromInt _ -> "ptr.from_int"
    Select {} -> "select"
    Load {} -> "load"
    Store {} -> "store"
    PtrAdd _ _ -> "ptr.add"
    StackAlloc _ _ -> "stack.alloc"
    GlobalGet _ -> "global.get"
    GlobalSet _ _ -> "global.set"
    Call _ _ -> "call"
    CallIndirect {} -> "call.indirect"

-- Rendering

renderSymbol :: Symbol -> Text
renderSymbol = renderDoc . prettySymbol

renderVar :: Var -> Text
renderVar = renderDoc . prettyVar

renderLabel :: Label -> Text
renderLabel = renderDoc . prettyLabel

renderType :: Type -> Text
renderType = renderDoc . prettyType

renderTypes :: [Type] -> Text
renderTypes types = renderDoc ("(" <> hsep (punctuate comma (map prettyType types)) <> ")" :: Doc ())

renderOperand :: Operand -> Text
renderOperand operand =
  case operand of
    OperandVar var -> renderVar var
    OperandLiteral literal -> renderDoc (prettyLiteral literal)

renderConvention :: CallingConvention -> Text
renderConvention convention =
  case convention of
    AihcConvention -> "aihc"
    CConvention -> "c"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
