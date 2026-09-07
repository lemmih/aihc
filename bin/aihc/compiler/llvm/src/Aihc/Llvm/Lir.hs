{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to textual LLVM IR.
--
-- The @aihc@ convention is @tailcc@, and every @tailcall@ is a @musttail@
-- call followed by @ret@, so LLVM verifies that the stack does not grow.
-- Block parameters become @phi@ instructions. Every edge with arguments
-- goes through its own block, so a target reached twice from one
-- predecessor still has one @phi@ entry per edge. The operations that trap
-- check their operands and branch to a block that reports the message.
module Aihc.Llvm.Lir
  ( LlvmLirError (..),
    compileLirModule,
  )
where

import Aihc.Lir.Lint (LintError, lintModule)
import Aihc.Lir.Resolve (resolveConstants, resolvedSwitchCaseValue, unresolvedConstant)
import Aihc.Lir.Syntax
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64, double2Float, float2Double)
import Numeric (showHex)

data LlvmLirError
  = LlvmLirLintErrors ![LintError]
  | LlvmLirUnsupported !Text
  deriving (Eq, Show)

-- | Lint the module, then render it.
compileLirModule :: Module -> Either LlvmLirError Text
compileLirModule lirModule =
  case lintModule lirModule of
    [] -> do
      (functions, traps) <- runStateT (mapM (compileFunction ctx) [function | ItemFunction function <- items]) Map.empty
      pure
        ( T.unlines
            ( preamble
                <> concatMap declareExtern [external | ItemExternFunction external <- items]
                <> [renderSymbol symbol <> " = external global i8" | ItemExternData symbol <- items]
                <> [renderSymbol (globalName global) <> " = internal global " <> renderType (globalType global) <> " " <> zeroValue (globalType global) | ItemGlobal global <- items]
                <> concatMap renderData [dataItem | ItemData dataItem <- items]
                <> [ "@" <> quote (trapMessageName index) <> " = private constant [" <> tshow (BS.length bytes) <> " x i8] c\"" <> escapeBytes bytes <> "\""
                   | (message, index) <- Map.toAscList traps,
                     let bytes = Text.encodeUtf8 (message <> "\n")
                   ]
                <> [""]
                <> concat functions
            )
        )
    errors -> Left (LlvmLirLintErrors errors)
  where
    Module items = resolveConstants lirModule
    ctx =
      Ctx
        { ctxSignatures =
            Map.fromList
              ( [(functionName function, functionSignature function) | ItemFunction function <- items]
                  <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
              ),
          ctxGlobals = Map.fromList [(globalName global, globalType global) | ItemGlobal global <- items]
        }

-- | The module-level facts the functions need.
data Ctx = Ctx
  { ctxSignatures :: !(Map Symbol Signature),
    ctxGlobals :: !(Map Symbol Type)
  }

preamble :: [Text]
preamble =
  [ "; Lir module compiled by Aihc.Llvm.Lir.",
    "declare i64 @write(i32, ptr, i64)",
    "declare void @_exit(i32) noreturn",
    "declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)",
    "declare float @llvm.fabs.f32(float)",
    "declare double @llvm.fabs.f64(double)",
    "declare float @llvm.sqrt.f32(float)",
    "declare double @llvm.sqrt.f64(double)"
  ]
    <> concat
      [ [ "declare {" <> ty <> ", i1} @llvm.uadd.with.overflow." <> ty <> "(" <> ty <> ", " <> ty <> ")",
          "declare {" <> ty <> ", i1} @llvm.usub.with.overflow." <> ty <> "(" <> ty <> ", " <> ty <> ")",
          -- The second argument states whether a zero operand is undefined.
          -- Lir defines it as the width, so it is always false.
          "declare " <> ty <> " @llvm.ctlz." <> ty <> "(" <> ty <> ", i1)",
          "declare " <> ty <> " @llvm.cttz." <> ty <> "(" <> ty <> ", i1)",
          "declare " <> ty <> " @llvm.ctpop." <> ty <> "(" <> ty <> ")"
        ]
      | ty <- ["i8", "i16", "i32", "i64"]
      ]

declareExtern :: ExternFunction -> [Text]
declareExtern external =
  [ "declare "
      <> renderConvention (signatureConvention signature)
      <> renderResults (signatureResults signature)
      <> " "
      <> renderSymbol (externFunctionName external)
      <> "("
      <> T.intercalate ", " (map renderType (signatureParameters signature))
      <> ")"
  ]
  where
    signature = externFunctionSignature external

-- | The name of the constant that holds one trap message.
trapMessageName :: Int -> Text
trapMessageName index = ".Llir_trap_" <> tshow index

-- Data

renderData :: DataItem -> [Text]
renderData dataItem =
  [ renderSymbol (dataName dataItem)
      <> " = "
      <> (if dataLinkage dataItem == Export then "" else "internal ")
      <> (if dataMutable dataItem then "global " else "constant ")
      <> "<{ "
      <> T.intercalate ", " (map fst fields)
      <> " }> <{ "
      <> T.intercalate ", " (map (\(ty, value) -> ty <> " " <> value) fields)
      <> " }>, align "
      <> tshow (dataAlignment dataItem)
  ]
  where
    fields = map renderField (dataFields dataItem)
    renderField field =
      case field of
        DataIntConstant _ constant -> unresolvedConstant constant
        DataInt I1 value -> ("i8", tshow (value .&. 1))
        DataInt ty value -> (renderType ty, renderInteger ty value)
        DataFloat ty value -> (renderType ty, renderFloat ty value)
        DataSymbol target 0 -> ("ptr", renderSymbol target)
        DataSymbol target addend -> ("ptr", "getelementptr (i8, ptr " <> renderSymbol target <> ", i64 " <> tshow addend <> ")")
        DataNull -> ("ptr", "null")
        DataWordConstant constant -> unresolvedConstant constant
        DataWord value -> ("i64", tshow value)
        DataCode Nothing -> ("ptr", "null")
        DataCode (Just target) -> ("ptr", renderSymbol target)
        DataBytes bytes -> ("[" <> tshow (BS.length bytes) <> " x i8]", "c\"" <> escapeBytes bytes <> "\"")
        DataZero count -> ("[" <> tshow count <> " x i8]", "zeroinitializer")

escapeBytes :: BS.ByteString -> Text
escapeBytes = T.concat . map escapeByte . BS.unpack
  where
    escapeByte :: Word8 -> Text
    escapeByte byte
      | byte >= 0x20 && byte < 0x7f && byte /= 0x22 && byte /= 0x5c = T.singleton (toEnum (fromIntegral byte))
      | otherwise = "\\" <> hexByte byte

hexByte :: Word8 -> Text
hexByte byte = T.pack (pad (showHex byte ""))
  where
    pad [digit] = ['0', digit]
    pad digits = map toUpperHex digits
    toUpperHex character = if character >= 'a' && character <= 'f' then toEnum (ord character - 32) else character

-- Names

-- | Every symbol and value is quoted, so any Lir name is a valid LLVM name.
quote :: Text -> Text
quote name = "\"" <> T.concatMap escape name <> "\""
  where
    escape character
      | character == '"' = "\\22"
      | character == '\\' = "\\5C"
      | character < ' ' || character > '~' = T.concat (map (("\\" <>) . hexByte) (BS.unpack (Text.encodeUtf8 (T.singleton character))))
      | otherwise = T.singleton character

renderSymbol :: Symbol -> Text
renderSymbol symbol = "@" <> quote (unSymbol symbol)

renderVar :: Var -> Text
renderVar var = "%" <> quote ("v." <> unVar var)

blockName :: Label -> Text
blockName label = quote ("b." <> unLabel label)

-- Types

renderType :: Type -> Text
renderType ty =
  case ty of
    I1 -> "i1"
    I8 -> "i8"
    I16 -> "i16"
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "float"
    F64 -> "double"
    Ptr -> "ptr"
    Code -> "ptr"

renderResults :: [Type] -> Text
renderResults types =
  case types of
    [] -> "void"
    [ty] -> renderType ty
    _ -> "{" <> T.intercalate ", " (map renderType types) <> "}"

renderConvention :: CallingConvention -> Text
renderConvention convention =
  case convention of
    AihcConvention -> "tailcc "
    CConvention -> ""

zeroValue :: Type -> Text
zeroValue ty
  | isFloatType ty = "0.0"
  | ty `elem` [Ptr, Code] = "null"
  | otherwise = "0"

-- | An integer literal in the signed range of its type.
renderInteger :: Type -> Integer -> Text
renderInteger ty value
  | ty == I1 = if value .&. 1 == 1 then "true" else "false"
  | wrapped >= 2 ^ (bits - 1) = tshow (wrapped - 2 ^ bits)
  | otherwise = tshow wrapped
  where
    bits = typeBits ty
    wrapped = value `mod` (2 ^ bits)

-- | A float literal as the hexadecimal double pattern LLVM accepts for both
-- widths.
renderFloat :: Type -> Double -> Text
renderFloat ty value = "0x" <> T.pack (pad (showHex (castDoubleToWord64 (if ty == F32 then float2Double (double2Float value) else value)) ""))
  where
    pad digits = replicate (16 - length digits) '0' <> map toUpperHex digits
    toUpperHex character = if character >= 'a' && character <= 'f' then toEnum (ord character - 32) else character

renderOperand :: Type -> Operand -> Text
renderOperand ty operand =
  case operand of
    OperandVar var -> renderVar var
    OperandLiteral literal ->
      case literal of
        LitInt value
          | isFloatType ty -> renderFloat ty (fromInteger value)
          | otherwise -> renderInteger ty value
        LitFloat value -> renderFloat ty value
        LitNull -> "null"
        LitSymbol symbol -> renderSymbol symbol

typed :: Type -> Operand -> Text
typed ty operand = renderType ty <> " " <> renderOperand ty operand

-- Functions

-- | The trap messages of the module, each with its constant index.
type Traps = Map Text Int

data FunctionState = FunctionState
  { stateTraps :: !Traps,
    -- | The trap blocks this function branches to.
    stateFunctionTraps :: !(Map Text Int),
    stateNextTemp :: !Int,
    -- | The finished blocks in reverse order, each as its lines.
    stateBlocksRev :: ![[Text]],
    -- | The lines of the open block in reverse order.
    stateOpenRev :: ![Text],
    -- | The phi entries of every block with parameters: for each edge, the
    -- edge block and the arguments.
    stateEdges :: !(Map Label [(Text, [Operand])]),
    stateNextEdge :: !Int
  }

type M = StateT FunctionState (Either LlvmLirError)

unsupported :: Text -> M value
unsupported = lift . Left . LlvmLirUnsupported

fresh :: M Text
fresh = do
  state <- get
  put state {stateNextTemp = stateNextTemp state + 1}
  pure ("%t" <> tshow (stateNextTemp state))

emit :: Text -> M ()
emit line = modify' $ \state -> state {stateOpenRev = ("  " <> line) : stateOpenRev state}

-- | Close the open block and open the next one.
beginBlock :: Text -> M ()
beginBlock name = modify' $ \state ->
  state
    { stateBlocksRev = reverse (stateOpenRev state) : stateBlocksRev state,
      stateOpenRev = [name <> ":"]
    }

-- | The block that reports one trap message.
trapBlock :: Text -> M Text
trapBlock message = do
  state <- get
  index <-
    case Map.lookup message (stateTraps state) of
      Just index -> pure index
      Nothing -> do
        let index = Map.size (stateTraps state)
        put state {stateTraps = Map.insert message index (stateTraps state)}
        pure index
  modify' $ \current -> current {stateFunctionTraps = Map.insert message index (stateFunctionTraps current)}
  pure (trapBlockName index)

trapBlockName :: Int -> Text
trapBlockName index = quote ("trap." <> tshow index)

-- | A jump edge. A target with parameters gets its own block so the phi of
-- the target sees one predecessor per edge.
edgeTo :: Map Label [(Var, Type)] -> Target -> M Text
edgeTo parameters (Target label arguments)
  | null (Map.findWithDefault [] label parameters) = pure ("%" <> blockName label)
  | otherwise = do
      state <- get
      let name = quote ("e." <> tshow (stateNextEdge state))
      put
        state
          { stateNextEdge = stateNextEdge state + 1,
            stateEdges = Map.insertWith (flip (<>)) label [(name, arguments)] (stateEdges state)
          }
      pure ("%" <> name)

compileFunction :: Ctx -> Function -> StateT Traps (Either LlvmLirError) [Text]
compileFunction ctx function = do
  traps <- get
  let initial =
        FunctionState
          { stateTraps = traps,
            stateFunctionTraps = Map.empty,
            stateNextTemp = 0,
            stateBlocksRev = [],
            stateOpenRev = [],
            stateEdges = Map.empty,
            stateNextEdge = 0
          }
      parameters = Map.fromList [(blockLabel block, blockParameters block) | block <- functionBlocks function]
  (bodies, final) <- lift (runStateT (mapM (compileBlock ctx function parameters) (functionBlocks function)) initial)
  put (stateTraps final)
  let edges = stateEdges final
      -- The phi instructions of a block are known only after every edge is
      -- compiled, so the blocks are rendered from their bodies here.
      rendered =
        concat
          [ [blockName (blockLabel block) <> ":"]
              <> [ "  " <> renderVar var <> " = phi " <> renderType ty <> " " <> T.intercalate ", " ["[" <> renderOperand ty (arguments !! index) <> ", %" <> edge <> "]" | (edge, arguments) <- Map.findWithDefault [] (blockLabel block) edges]
                 | (index, (var, ty)) <- zip [0 ..] (blockParameters block)
                 ]
              <> body
          | (block, body) <- zip (functionBlocks function) bodies
          ]
      edgeBlocks =
        concat
          [ [edge <> ":", "  br label %" <> blockName label]
          | (label, targets) <- Map.toAscList edges,
            (edge, _) <- targets
          ]
      trapBlocks =
        concat
          [ [ trapBlockName index <> ":",
              "  call i64 @write(i32 2, ptr @" <> quote (trapMessageName index) <> ", i64 " <> tshow (BS.length (Text.encodeUtf8 (message <> "\n"))) <> ")",
              "  call void @_exit(i32 1)",
              "  unreachable"
            ]
          | (message, index) <- Map.toAscList (stateFunctionTraps final)
          ]
  pure
    ( [ "define "
          <> (if functionLinkage function == Export then "" else "internal ")
          <> renderConvention (functionConvention function)
          <> renderResults (functionResults function)
          <> " "
          <> renderSymbol (functionName function)
          <> "("
          <> T.intercalate ", " [renderType ty <> " " <> renderVar var | (var, ty) <- functionParameters function]
          <> ") {"
      ]
        <> rendered
        <> edgeBlocks
        <> trapBlocks
        <> ["}", ""]
    )

-- | The lines of one block after its phi instructions. An instruction that
-- traps splits the block, so the result is a list of blocks: the first
-- continues the block of the Lir label and the rest are fresh blocks.
compileBlock :: Ctx -> Function -> Map Label [(Var, Type)] -> Block -> M [Text]
compileBlock ctx function parameters block = do
  modify' $ \state -> state {stateBlocksRev = [], stateOpenRev = []}
  mapM_ (compileInstruction ctx) (blockInstructions block)
  compileTerminator ctx function parameters (blockTerminator block)
  state <- get
  pure (concat (reverse (reverse (stateOpenRev state) : stateBlocksRev state)))

compileTerminator :: Ctx -> Function -> Map Label [(Var, Type)] -> Terminator -> M ()
compileTerminator ctx function parameters terminator =
  case terminator of
    Jump target -> do
      edge <- edgeTo parameters target
      emit ("br label " <> edge)
    Branch condition whenTrue whenFalse -> do
      trueEdge <- edgeTo parameters whenTrue
      falseEdge <- edgeTo parameters whenFalse
      emit ("br i1 " <> renderOperand I1 condition <> ", label " <> trueEdge <> ", label " <> falseEdge)
    Switch ty scrutinee cases fallback -> do
      fallbackEdge <-
        case fallback of
          Just target -> edgeTo parameters target
          Nothing -> ("%" <>) <$> trapBlock "switch without a matching case"
      edges <- forM cases $ \switchCase -> do
        edge <- edgeTo parameters (switchCaseTarget switchCase)
        pure (renderType ty <> " " <> renderInteger ty (resolvedSwitchCaseValue switchCase) <> ", label " <> edge)
      emit ("switch " <> typed ty scrutinee <> ", label " <> fallbackEdge <> " [" <> T.intercalate " " edges <> "]")
    Return values -> returnValues (functionResults function) (zipWith renderOperand (functionResults function) values)
    TailCall symbol arguments ->
      tailCall (renderSymbol symbol) (Map.findWithDefault (Signature [] [] AihcConvention) symbol (ctxSignatures ctx)) arguments
    TailCallIndirect target arguments signature -> do
      continue <- guardCallee target
      beginBlock continue
      tailCall (renderOperand Code target) signature arguments
    Trap message -> do
      name <- trapBlock message
      emit ("br label %" <> name)
  where
    tailCall callee signature arguments = do
      let call = renderConvention (signatureConvention signature) <> renderResults (signatureResults signature) <> " " <> callee <> "(" <> T.intercalate ", " (zipWith typed (signatureParameters signature) arguments) <> ")"
          keyword = case signatureConvention signature of
            AihcConvention -> "musttail call "
            CConvention -> "tail call "
      case signatureResults signature of
        [] -> do
          emit (keyword <> call)
          emit "ret void"
        results -> do
          result <- fresh
          emit (result <> " = " <> keyword <> call)
          emit ("ret " <> renderResults results <> " " <> result)
    returnValues types values =
      case (types, values) of
        ([], _) -> emit "ret void"
        ([ty], [value]) -> emit ("ret " <> renderType ty <> " " <> value)
        _ -> do
          aggregate <- buildAggregate types values
          emit ("ret " <> renderResults types <> " " <> aggregate)

-- | Build a struct of several results with @insertvalue@.
buildAggregate :: [Type] -> [Text] -> M Text
buildAggregate types values = go "undef" (zip3 [0 :: Int ..] types values)
  where
    aggregateType = renderResults types
    go current [] = pure current
    go current ((index, ty, value) : rest) = do
      next <- fresh
      emit (next <> " = insertvalue " <> aggregateType <> " " <> current <> ", " <> renderType ty <> " " <> value <> ", " <> tshow index)
      go next rest

-- | Branch to the trap block when a callee is null. Returns the name of the
-- block that continues the call.
guardCallee :: Operand -> M Text
guardCallee target = do
  trap <- trapBlock "indirect call to a non-function"
  isNull <- fresh
  continue <- fresh
  let continueName = quote ("c" <> T.drop 1 continue)
  emit (isNull <> " = icmp eq ptr " <> renderOperand Code target <> ", null")
  emit ("br i1 " <> isNull <> ", label %" <> trap <> ", label %" <> continueName)
  pure continueName

-- | Branch to a trap block when a condition holds and continue in a fresh
-- block otherwise.
trapWhen :: Text -> Text -> M ()
trapWhen condition message = do
  trap <- trapBlock message
  continue <- fresh
  let continueName = quote ("c" <> T.drop 1 continue)
  emit ("br i1 " <> condition <> ", label %" <> trap <> ", label %" <> continueName)
  beginBlock continueName

compileInstruction :: Ctx -> Instruction -> M ()
compileInstruction ctx (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      let a = renderOperand ty left
          b = renderOperand ty right
          llvmType = renderType ty
      case op of
        Add -> single ("add " <> llvmType <> " " <> a <> ", " <> b)
        Sub -> single ("sub " <> llvmType <> " " <> a <> ", " <> b)
        Mul -> single ("mul " <> llvmType <> " " <> a <> ", " <> b)
        And -> single ("and " <> llvmType <> " " <> a <> ", " <> b)
        Or -> single ("or " <> llvmType <> " " <> a <> ", " <> b)
        Xor -> single ("xor " <> llvmType <> " " <> a <> ", " <> b)
        Shl -> shift "shl" ty a b
        ShrS -> shift "ashr" ty a b
        ShrU -> shift "lshr" ty a b
        DivS -> do
          divisionChecks ty a b
          overflowCheck ty a b
          single ("sdiv " <> llvmType <> " " <> a <> ", " <> b)
        DivU -> do
          divisionChecks ty a b
          single ("udiv " <> llvmType <> " " <> a <> ", " <> b)
        RemS -> do
          divisionChecks ty a b
          -- The remainder of the minimum value by minus one is zero, which
          -- LLVM leaves undefined; divide by one instead in that case.
          isMinusOne <- fresh
          divisor <- fresh
          emit (isMinusOne <> " = icmp eq " <> llvmType <> " " <> b <> ", -1")
          emit (divisor <> " = select i1 " <> isMinusOne <> ", " <> llvmType <> " 1, " <> llvmType <> " " <> b)
          single ("srem " <> llvmType <> " " <> a <> ", " <> divisor)
        RemU -> do
          divisionChecks ty a b
          single ("urem " <> llvmType <> " " <> a <> ", " <> b)
    Unary op ty value ->
      case op of
        Clz -> single ("call " <> renderType ty <> " @llvm.ctlz." <> renderType ty <> "(" <> typed ty value <> ", i1 false)")
        Ctz -> single ("call " <> renderType ty <> " @llvm.cttz." <> renderType ty <> "(" <> typed ty value <> ", i1 false)")
        Popcount -> single ("call " <> renderType ty <> " @llvm.ctpop." <> renderType ty <> "(" <> typed ty value <> ")")
    Wide op ty left right -> do
      let a = renderOperand ty left
          b = renderOperand ty right
      case op of
        MulWideU -> wideMultiply "zext" ty a b
        MulWideS -> wideMultiply "sext" ty a b
        AddCarry -> overflowIntrinsic "uadd" ty a b
        SubBorrow -> overflowIntrinsic "usub" ty a b
    Compare op ty left right ->
      single (comparison op ty <> " " <> renderType ty <> " " <> renderOperand ty left <> ", " <> renderOperand ty right)
    FloatBinary op ty left right ->
      single (floatBinary op <> " " <> renderType ty <> " " <> renderOperand ty left <> ", " <> renderOperand ty right)
    FloatUnary op ty value ->
      case op of
        FNeg -> single ("fneg " <> typed ty value)
        FAbs -> single ("call " <> renderType ty <> " @llvm.fabs." <> floatSuffix ty <> "(" <> typed ty value <> ")")
        FSqrt -> single ("call " <> renderType ty <> " @llvm.sqrt." <> floatSuffix ty <> "(" <> typed ty value <> ")")
    Convert op from value to ->
      case op of
        SExt -> single ("sext " <> typed from value <> " to " <> renderType to)
        ZExt -> single ("zext " <> typed from value <> " to " <> renderType to)
        Trunc -> single ("trunc " <> typed from value <> " to " <> renderType to)
        IToFS -> single ("sitofp " <> typed from value <> " to " <> renderType to)
        IToFU -> single ("uitofp " <> typed from value <> " to " <> renderType to)
        FToIS -> do
          floatRangeChecks True from value to
          single ("fptosi " <> typed from value <> " to " <> renderType to)
        FToIU -> do
          floatRangeChecks False from value to
          single ("fptoui " <> typed from value <> " to " <> renderType to)
        FpExt -> single ("fpext " <> typed from value <> " to " <> renderType to)
        FpTrunc -> single ("fptrunc " <> typed from value <> " to " <> renderType to)
        Bitcast -> single ("bitcast " <> typed from value <> " to " <> renderType to)
    PtrToInt value -> single ("ptrtoint " <> typed Ptr value <> " to i64")
    PtrFromInt value -> single ("inttoptr " <> typed I64 value <> " to ptr")
    Select ty condition left right ->
      single ("select i1 " <> renderOperand I1 condition <> ", " <> typed ty left <> ", " <> typed ty right)
    Load ty address alignment -> do
      pointer <- effectiveAddress address
      case ty of
        I1 -> do
          byte <- fresh
          emit (byte <> " = load i8, ptr " <> pointer <> ", align " <> tshow alignment)
          single ("trunc i8 " <> byte <> " to i1")
        _ -> single ("load " <> renderType ty <> ", ptr " <> pointer <> ", align " <> tshow alignment)
    Store ty value address alignment -> do
      pointer <- effectiveAddress address
      case ty of
        I1 -> do
          byte <- fresh
          emit (byte <> " = zext i1 " <> renderOperand I1 value <> " to i8")
          emit ("store i8 " <> byte <> ", ptr " <> pointer <> ", align " <> tshow alignment)
        _ -> emit ("store " <> typed ty value <> ", ptr " <> pointer <> ", align " <> tshow alignment)
    PtrAdd base offset -> single ("getelementptr i8, ptr " <> renderOperand Ptr base <> ", i64 " <> renderOperand I64 offset)
    StackAlloc size alignment -> do
      case results of
        [var] -> do
          emit (renderVar var <> " = alloca [" <> tshow size <> " x i8], align " <> tshow alignment)
          emit ("call void @llvm.memset.p0.i64(ptr " <> renderVar var <> ", i8 0, i64 " <> tshow size <> ", i1 false)")
        _ -> unsupported "stack.alloc result count"
    GlobalGet symbol -> single ("load " <> renderType (globalTypeOf symbol) <> ", ptr " <> renderSymbol symbol)
    GlobalSet symbol value -> emit ("store " <> typed (globalTypeOf symbol) value <> ", ptr " <> renderSymbol symbol)
    Call symbol arguments ->
      call (renderSymbol symbol) (Map.findWithDefault (Signature [] [] AihcConvention) symbol (ctxSignatures ctx)) arguments
    CallIndirect target arguments signature -> do
      continue <- guardCallee target
      beginBlock continue
      call (renderOperand Code target) signature arguments
  where
    single body =
      case results of
        [var] -> emit (renderVar var <> " = " <> body)
        _ -> unsupported "instruction result count"

    globalTypeOf symbol = Map.findWithDefault I64 symbol (ctxGlobals ctx)

    shift keyword ty a b = do
      count <- fresh
      emit (count <> " = and " <> renderType ty <> " " <> b <> ", " <> tshow (typeBits ty - 1))
      single (keyword <> " " <> renderType ty <> " " <> a <> ", " <> count)

    divisionChecks ty _ b = do
      isZero <- fresh
      emit (isZero <> " = icmp eq " <> renderType ty <> " " <> b <> ", 0")
      trapWhen isZero "integer division by zero"

    overflowCheck ty a b = do
      isMinimum <- fresh
      isMinusOne <- fresh
      overflows <- fresh
      emit (isMinimum <> " = icmp eq " <> renderType ty <> " " <> a <> ", " <> renderInteger ty (negate (2 ^ (typeBits ty - 1))))
      emit (isMinusOne <> " = icmp eq " <> renderType ty <> " " <> b <> ", -1")
      emit (overflows <> " = and i1 " <> isMinimum <> ", " <> isMinusOne)
      trapWhen overflows "integer overflow"

    wideMultiply extend ty a b =
      case results of
        [low, high] -> do
          let bits = typeBits ty
              wide = "i" <> tshow (2 * bits)
          wideA <- fresh
          wideB <- fresh
          product' <- fresh
          shifted <- fresh
          emit (wideA <> " = " <> extend <> " " <> renderType ty <> " " <> a <> " to " <> wide)
          emit (wideB <> " = " <> extend <> " " <> renderType ty <> " " <> b <> " to " <> wide)
          emit (product' <> " = mul " <> wide <> " " <> wideA <> ", " <> wideB)
          emit (renderVar low <> " = trunc " <> wide <> " " <> product' <> " to " <> renderType ty)
          emit (shifted <> " = lshr " <> wide <> " " <> product' <> ", " <> tshow bits)
          emit (renderVar high <> " = trunc " <> wide <> " " <> shifted <> " to " <> renderType ty)
        _ -> unsupported "wide multiplication result count"

    overflowIntrinsic name ty a b =
      case results of
        [value, flag] -> do
          pair <- fresh
          let llvmType = renderType ty
          emit (pair <> " = call {" <> llvmType <> ", i1} @llvm." <> name <> ".with.overflow." <> llvmType <> "(" <> llvmType <> " " <> a <> ", " <> llvmType <> " " <> b <> ")")
          emit (renderVar value <> " = extractvalue {" <> llvmType <> ", i1} " <> pair <> ", 0")
          emit (renderVar flag <> " = extractvalue {" <> llvmType <> ", i1} " <> pair <> ", 1")
        _ -> unsupported "carry operation result count"

    -- NaN and values outside the range of the target trap. The bounds are
    -- powers of two, so they are exact in both float widths.
    floatRangeChecks signed from value to = do
      let bits = typeBits to
          lower = if signed then negate (2 ^^ (bits - 1)) else -1 :: Double
          upper = if signed then 2 ^^ (bits - 1) else 2 ^^ bits :: Double
          operand = renderOperand from value
          floatType = renderType from
      isNan <- fresh
      emit (isNan <> " = fcmp uno " <> floatType <> " " <> operand <> ", " <> operand)
      trapWhen isNan "invalid float to integer conversion"
      tooLow <- fresh
      emit (tooLow <> " = fcmp " <> (if signed then "olt" else "ole") <> " " <> floatType <> " " <> operand <> ", " <> renderFloat from lower)
      trapWhen tooLow "invalid float to integer conversion"
      tooHigh <- fresh
      emit (tooHigh <> " = fcmp oge " <> floatType <> " " <> operand <> ", " <> renderFloat from upper)
      trapWhen tooHigh "invalid float to integer conversion"

    effectiveAddress (Address base offset)
      | offset == 0 = pure (renderOperand Ptr base)
      | otherwise = do
          pointer <- fresh
          emit (pointer <> " = getelementptr i8, ptr " <> renderOperand Ptr base <> ", i64 " <> tshow offset)
          pure pointer

    call callee signature arguments = do
      let body = "call " <> renderConvention (signatureConvention signature) <> renderResults (signatureResults signature) <> " " <> callee <> "(" <> T.intercalate ", " (zipWith typed (signatureParameters signature) arguments) <> ")"
      case (signatureResults signature, results) of
        ([], []) -> emit body
        ([_], [var]) -> emit (renderVar var <> " = " <> body)
        (types, vars) | length types == length vars -> do
          aggregate <- fresh
          emit (aggregate <> " = " <> body)
          forM_ (zip [0 :: Int ..] vars) $ \(index, var) ->
            emit (renderVar var <> " = extractvalue " <> renderResults types <> " " <> aggregate <> ", " <> tshow index)
        _ -> unsupported "call result count"

comparison :: CompareOp -> Type -> Text
comparison op ty
  | isFloatType ty =
      case op of
        Eq -> "fcmp oeq"
        Ne -> "fcmp une"
        FLt -> "fcmp olt"
        FLe -> "fcmp ole"
        FGt -> "fcmp ogt"
        FGe -> "fcmp oge"
        _ -> "fcmp false"
  | otherwise =
      case op of
        Eq -> "icmp eq"
        Ne -> "icmp ne"
        LtS -> "icmp slt"
        LtU -> "icmp ult"
        LeS -> "icmp sle"
        LeU -> "icmp ule"
        GtS -> "icmp sgt"
        GtU -> "icmp ugt"
        GeS -> "icmp sge"
        GeU -> "icmp uge"
        FLt -> "icmp ult"
        FLe -> "icmp ule"
        FGt -> "icmp ugt"
        FGe -> "icmp uge"

floatBinary :: FloatBinaryOp -> Text
floatBinary op =
  case op of
    FAdd -> "fadd"
    FSub -> "fsub"
    FMul -> "fmul"
    FDiv -> "fdiv"

floatSuffix :: Type -> Text
floatSuffix ty = if ty == F64 then "f64" else "f32"

tshow :: (Show value) => value -> Text
tshow = T.pack . show
