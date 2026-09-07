-- | Human-readable Lir rendering. "Aihc.Lir.Parser" reads this format back.
module Aihc.Lir.Pretty
  ( prettyModule,
    prettyFunction,
    prettyOperation,
    prettyTerminator,
    prettyType,
    prettySymbol,
    prettyVar,
    prettyLabel,
    prettyOperand,
    prettyLiteral,
    prettyQuoted,
    renderModule,
    renderDoc,
    reservedWords,
    binaryOpName,
    unaryOpName,
    wideOpName,
    compareOpName,
    floatBinaryOpName,
    floatUnaryOpName,
    convertOpName,
  )
where

import Aihc.Lir.Syntax
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isAlphaNum, isPrint, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import Prettyprinter (Doc, comma, defaultLayoutOptions, hardline, hsep, indent, layoutPretty, pretty, punctuate, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)

renderModule :: Module -> Text
renderModule = renderDoc . prettyModule

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

prettyModule :: Module -> Doc ann
prettyModule (Module items) = vsep (punctuate hardline (map prettyItem items)) <> hardline

prettyItem :: Item -> Doc ann
prettyItem item =
  case item of
    ItemFunction function -> prettyFunction function
    ItemExternFunction external ->
      "extern func"
        <+> prettySymbol (externFunctionName external)
        <> prettySignature (externFunctionSignature external)
    ItemGlobal global ->
      "global"
        <+> prettySymbol (globalName global)
        <> ":"
        <+> prettyType (globalType global)
        <> (if globalPinned global then " pinned" else mempty)
    ItemData dataItem -> prettyData dataItem
    ItemExternData symbol -> "extern data" <+> prettySymbol symbol
    ItemConstant constant -> "const" <+> prettySymbol (constantName constant) <+> "=" <+> pretty (constantValue constant)
    ItemInclude path -> "include" <+> prettyQuoted path

prettyLinkage :: Linkage -> Doc ann
prettyLinkage linkage =
  case linkage of
    Internal -> mempty
    Export -> "export "

prettyFunction :: Function -> Doc ann
prettyFunction function =
  prettyLinkage (functionLinkage function)
    <> "func"
    <+> prettySymbol (functionName function)
    <> prettyParameters (functionParameters function)
    <> prettyResults (functionResults function)
    <> prettyConvention (functionConvention function)
    <+> "{"
    <> hardline
    <> vsep (punctuate hardline (map prettyBlock (functionBlocks function)))
    <> hardline
    <> "}"

prettyParameters :: [(Var, Type)] -> Doc ann
prettyParameters parameters =
  "(" <> hsep (punctuate comma (map prettyParameter parameters)) <> ")"
  where
    prettyParameter (var, ty) = prettyVar var <> ":" <+> prettyType ty

prettyResults :: [Type] -> Doc ann
prettyResults results =
  case results of
    [] -> mempty
    [ty] -> " ->" <+> prettyType ty
    _ -> " -> (" <> hsep (punctuate comma (map prettyType results)) <> ")"

prettyConvention :: CallingConvention -> Doc ann
prettyConvention convention =
  case convention of
    AihcConvention -> mempty
    CConvention -> " cc c"

prettySignature :: Signature -> Doc ann
prettySignature signature =
  "("
    <> hsep (punctuate comma (map prettyType (signatureParameters signature)))
    <> ")"
    <> prettyResults (signatureResults signature)
    <> prettyConvention (signatureConvention signature)

prettyData :: DataItem -> Doc ann
prettyData dataItem =
  prettyLinkage (dataLinkage dataItem)
    <> "data"
    <> (if dataMutable dataItem then " mut" else mempty)
    <+> prettySymbol (dataName dataItem)
    <+> "align"
    <+> pretty (dataAlignment dataItem)
    <+> "="
    <+> "{"
    <+> hsep (punctuate comma (map prettyDataField (dataFields dataItem)))
    <+> "}"

prettyDataField :: DataField -> Doc ann
prettyDataField field =
  case field of
    DataInt ty value -> prettyType ty <+> pretty value
    DataIntConstant ty symbol -> prettyType ty <+> prettySymbol symbol
    DataFloat ty value -> prettyType ty <+> prettyFloat value
    DataSymbol symbol addend -> "ptr" <+> prettySymbol symbol <> prettyAddend addend
    DataNull -> "ptr null"
    DataWord value -> "word" <+> pretty value
    DataWordConstant symbol -> "word" <+> prettySymbol symbol
    DataCode symbol -> "code" <+> maybe "null" prettySymbol symbol
    DataBytes bytes -> "bytes" <+> prettyBytes bytes
    DataZero count -> "zero" <+> pretty count

prettyAddend :: Integer -> Doc ann
prettyAddend addend
  | addend == 0 = mempty
  | addend < 0 = " -" <+> pretty (negate addend)
  | otherwise = " +" <+> pretty addend

prettyBlock :: Block -> Doc ann
prettyBlock block =
  prettyLabel (blockLabel block)
    <> parameters
    <> ":"
    <> hardline
    <> indent 2 (vsep (map prettyInstruction (blockInstructions block) <> [prettyTerminator (blockTerminator block)]))
  where
    parameters =
      case blockParameters block of
        [] -> mempty
        params -> prettyParameters params

prettyInstruction :: Instruction -> Doc ann
prettyInstruction instruction =
  case instructionResults instruction of
    [] -> prettyOperation (instructionOperation instruction)
    results -> hsep (punctuate comma (map prettyVar results)) <+> "=" <+> prettyOperation (instructionOperation instruction)

prettyOperation :: Operation -> Doc ann
prettyOperation operation =
  case operation of
    Binary op ty left right -> pretty (binaryOpName op) <+> prettyType ty <+> prettyOperands [left, right]
    Unary op ty value -> pretty (unaryOpName op) <+> prettyType ty <+> prettyOperand value
    Wide op ty left right -> pretty (wideOpName op) <+> prettyType ty <+> prettyOperands [left, right]
    Compare op ty left right -> pretty (compareOpName op) <+> prettyType ty <+> prettyOperands [left, right]
    FloatBinary op ty left right -> pretty (floatBinaryOpName op) <+> prettyType ty <+> prettyOperands [left, right]
    FloatUnary op ty operand -> pretty (floatUnaryOpName op) <+> prettyType ty <+> prettyOperand operand
    Convert op from operand to -> pretty (convertOpName op) <+> prettyType from <+> prettyOperand operand <+> "to" <+> prettyType to
    PtrToInt operand -> "ptr.to_int" <+> prettyOperand operand
    PtrFromInt operand -> "ptr.from_int" <+> prettyOperand operand
    Select ty condition left right -> "select" <+> prettyType ty <+> prettyOperands [condition, left, right]
    Load ty address alignment -> "load" <+> prettyType ty <+> prettyAddress address <+> prettyAlign alignment
    Store ty value address alignment -> "store" <+> prettyType ty <+> prettyOperand value <> "," <+> prettyAddress address <+> prettyAlign alignment
    PtrAdd base offset -> "ptr.add" <+> prettyOperands [base, offset]
    StackAlloc size alignment -> "stack.alloc" <+> pretty size <+> prettyAlign alignment
    GlobalGet symbol -> "global.get" <+> prettySymbol symbol
    GlobalSet symbol value -> "global.set" <+> prettySymbol symbol <> "," <+> prettyOperand value
    Call symbol arguments -> "call" <+> prettySymbol symbol <> prettyArguments arguments
    CallIndirect target arguments signature ->
      "call.indirect" <+> prettyOperand target <> prettyArguments arguments <+> ":" <+> prettySignature signature

prettyAlign :: Integer -> Doc ann
prettyAlign alignment = "align" <+> pretty alignment

prettyAddress :: Address -> Doc ann
prettyAddress (Address base offset) = "[" <> prettyOperand base <> prettyAddend offset <> "]"

prettyArguments :: [Operand] -> Doc ann
prettyArguments arguments = "(" <> prettyOperands arguments <> ")"

prettyOperands :: [Operand] -> Doc ann
prettyOperands = hsep . punctuate comma . map prettyOperand

prettyTerminator :: Terminator -> Doc ann
prettyTerminator terminator =
  case terminator of
    Jump target -> "jump" <+> prettyTarget target
    Branch condition whenTrue whenFalse ->
      "br" <+> prettyOperand condition <> "," <+> prettyTarget whenTrue <> "," <+> prettyTarget whenFalse
    Switch ty scrutinee cases fallback ->
      "switch"
        <+> prettyType ty
        <+> prettyOperand scrutinee
        <+> "{"
        <> hardline
        <> indent 2 (vsep (map prettyCase cases <> maybe [] (\target -> ["default ->" <+> prettyTarget target]) fallback))
        <> hardline
        <> "}"
    Return operands -> case operands of
      [] -> "return"
      _ -> "return" <+> prettyOperands operands
    TailCall symbol arguments -> "tailcall" <+> prettySymbol symbol <> prettyArguments arguments
    TailCallIndirect target arguments signature ->
      "tailcall.indirect" <+> prettyOperand target <> prettyArguments arguments <+> ":" <+> prettySignature signature
    Trap message -> "trap" <+> prettyQuoted message
  where
    prettyCase (SwitchCase value target) = pretty value <+> "->" <+> prettyTarget target
    prettyCase (SwitchCaseConstant symbol target) = prettySymbol symbol <+> "->" <+> prettyTarget target

prettyTarget :: Target -> Doc ann
prettyTarget (Target label arguments) =
  case arguments of
    [] -> prettyLabel label
    _ -> prettyLabel label <> prettyArguments arguments

prettyOperand :: Operand -> Doc ann
prettyOperand operand =
  case operand of
    OperandVar var -> prettyVar var
    OperandLiteral literal -> prettyLiteral literal

prettyLiteral :: Literal -> Doc ann
prettyLiteral literal =
  case literal of
    LitInt value -> pretty value
    LitFloat value -> prettyFloat value
    LitNull -> "null"
    LitSymbol symbol -> prettySymbol symbol

prettyFloat :: Double -> Doc ann
prettyFloat value
  | isNaN value = "nan"
  | isInfinite value = if value > 0 then "inf" else "-inf"
  | otherwise = pretty (show value)

prettyType :: Type -> Doc ann
prettyType ty =
  case ty of
    I1 -> "i1"
    I8 -> "i8"
    I16 -> "i16"
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "f32"
    F64 -> "f64"
    Ptr -> "ptr"
    Code -> "code"

prettySymbol :: Symbol -> Doc ann
prettySymbol (Symbol name) = "@" <> prettyName name

prettyVar :: Var -> Doc ann
prettyVar (Var name) = "%" <> prettyName name

-- | A bare label starts with a letter or an underscore. This keeps a label
-- distinct from a literal that may follow a @return@.
prettyLabel :: Label -> Doc ann
prettyLabel (Label name)
  | name `elem` reservedWords = prettyQuoted name
  | Just (first, _) <- T.uncons name, isAsciiLetter first || first == '_' = prettyName name
  | otherwise = prettyQuoted name
  where
    isAsciiLetter character = isAlpha character && character < '\x80'

prettyName :: Text -> Doc ann
prettyName name
  | not (T.null name) && T.all isBareNameCharacter name = pretty name
  | otherwise = prettyQuoted name
  where
    isBareNameCharacter character =
      isAlphaNum character && character < '\x80' || character `elem` ['_', '.', '$']

prettyQuoted :: Text -> Doc ann
prettyQuoted text = pretty ("\"" <> T.concatMap escapeChar text <> "\"")

escapeChar :: Char -> Text
escapeChar character =
  case character of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\0' -> "\\0"
    _
      | isPrint character -> T.singleton character
      | otherwise -> T.pack ("\\u{" <> showHex (ord character) "}")

prettyBytes :: BS.ByteString -> Doc ann
prettyBytes bytes = pretty ("\"" <> T.concat (map escapeByte (BS.unpack bytes)) <> "\"")
  where
    escapeByte byte
      | byte == 0x5c = "\\\\"
      | byte == 0x22 = "\\\""
      | byte >= 0x20 && byte < 0x7f = T.singleton (toEnum (fromIntegral byte))
      | byte < 0x10 = T.pack ("\\x0" <> showHex byte "")
      | otherwise = T.pack ("\\x" <> showHex byte "")

-- | Words that the parser recognizes before a bare label. A label with one of
-- these names is quoted.
reservedWords :: [Text]
reservedWords =
  ["default", "func", "export", "extern", "global", "data", "mut", "align", "cc", "to", "null", "inf", "nan", "pinned", "bytes", "zero"]
    <> ["jump", "br", "switch", "return", "tailcall", "tailcall.indirect", "trap"]
    <> map binaryOpName [minBound .. maxBound]
    <> map wideOpName [minBound .. maxBound]
    <> map compareOpName [minBound .. maxBound]
    <> map floatBinaryOpName [minBound .. maxBound]
    <> map floatUnaryOpName [minBound .. maxBound]
    <> map convertOpName [minBound .. maxBound]
    <> ["ptr.to_int", "ptr.from_int", "select", "load", "store", "ptr.add", "stack.alloc", "global.get", "global.set", "call", "call.indirect"]
    <> ["i1", "i8", "i16", "i32", "i64", "f32", "f64", "ptr", "code"]

binaryOpName :: BinaryOp -> Text
binaryOpName op =
  case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    DivS -> "div.s"
    DivU -> "div.u"
    RemS -> "rem.s"
    RemU -> "rem.u"
    And -> "and"
    Or -> "or"
    Xor -> "xor"
    Shl -> "shl"
    ShrS -> "shr.s"
    ShrU -> "shr.u"

wideOpName :: WideOp -> Text
wideOpName op =
  case op of
    MulWideS -> "mul.wide.s"
    MulWideU -> "mul.wide.u"
    AddCarry -> "add.carry"
    SubBorrow -> "sub.borrow"

unaryOpName :: UnaryOp -> Text
unaryOpName op =
  case op of
    Clz -> "clz"
    Ctz -> "ctz"
    Popcount -> "popcount"

compareOpName :: CompareOp -> Text
compareOpName op =
  case op of
    Eq -> "eq"
    Ne -> "ne"
    LtS -> "lt.s"
    LtU -> "lt.u"
    LeS -> "le.s"
    LeU -> "le.u"
    GtS -> "gt.s"
    GtU -> "gt.u"
    GeS -> "ge.s"
    GeU -> "ge.u"
    FLt -> "flt"
    FLe -> "fle"
    FGt -> "fgt"
    FGe -> "fge"

floatBinaryOpName :: FloatBinaryOp -> Text
floatBinaryOpName op =
  case op of
    FAdd -> "fadd"
    FSub -> "fsub"
    FMul -> "fmul"
    FDiv -> "fdiv"

floatUnaryOpName :: FloatUnaryOp -> Text
floatUnaryOpName op =
  case op of
    FNeg -> "fneg"
    FAbs -> "fabs"
    FSqrt -> "fsqrt"

convertOpName :: ConvertOp -> Text
convertOpName op =
  case op of
    SExt -> "sext"
    ZExt -> "zext"
    Trunc -> "trunc"
    IToFS -> "itof.s"
    IToFU -> "itof.u"
    FToIS -> "ftoi.s"
    FToIU -> "ftoi.u"
    FpExt -> "fpext"
    FpTrunc -> "fptrunc"
    Bitcast -> "bitcast"
