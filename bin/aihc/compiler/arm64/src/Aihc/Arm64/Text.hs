{-# LANGUAGE OverloadedStrings #-}

-- | Render the ARM64 statements of a compiled module as assembly text.
--
-- The object writer needs no text, so nothing in the compiler depends on this
-- module. It exists so that a test can read the code a backend produces, and
-- so that a change of instruction selection or of register allocation shows
-- up as a diff of the fixtures in @Test\/Fixtures\/lir\/asm@ instead of as a
-- change of some bytes.
module Aihc.Arm64.Text
  ( renderArm64Statements,
  )
where

import Aihc.Arm64.Assemble
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

renderArm64Statements :: [Arm64Statement] -> Text
renderArm64Statements statements = T.unlines (map (T.pack . statement) statements)

statement :: Arm64Statement -> String
statement value =
  case value of
    Arm64Section role -> section role
    Arm64Align alignment -> indent (".p2align " <> show alignment)
    Arm64Global symbol -> indent (".globl " <> T.unpack symbol)
    Arm64Label symbol -> T.unpack symbol <> ":"
    Arm64Quad word -> indent (".quad " <> hex (toInteger word))
    Arm64Word width word -> indent ("." <> widthDirective width <> " " <> hex (toInteger word))
    Arm64QuadSymbol symbol -> indent (".quad " <> T.unpack symbol)
    Arm64QuadSymbolAddend symbol addend -> indent (".quad " <> T.unpack symbol <> signed (toInteger addend))
    Arm64Bytes bytes -> indent (".byte " <> intercalate ", " (map (hex . toInteger) (BS.unpack bytes)))
    Arm64Code code -> indent (instruction code)

section :: SectionRole -> String
section role =
  case role of
    TextSection -> indent ".text"
    TextConstantsSection -> indent ".section __TEXT,__const"
    ReadOnlySection -> indent ".section __TEXT,__const"
    DataSection -> indent ".data"
    NoExecuteStackSection -> indent ".section __DATA,__nx_stack"

widthDirective :: Int -> String
widthDirective width =
  case width of
    1 -> "byte"
    2 -> "hword"
    4 -> "word"
    _ -> "quad"

indent :: String -> String
indent text = "\t" <> text

instruction :: Arm64Instruction -> String
instruction code =
  case code of
    ArmRet -> "ret"
    ArmBrk value -> "brk #" <> show value
    ArmBr register -> "br " <> reg register
    ArmBlr register -> "blr " <> reg register
    ArmB target -> "b " <> T.unpack target
    ArmBl target -> "bl " <> T.unpack target
    ArmBCond condition target -> "b." <> cond condition <> " " <> T.unpack target
    ArmCbz register target -> "cbz " <> reg register <> ", " <> T.unpack target
    ArmCbnz register target -> "cbnz " <> reg register <> ", " <> T.unpack target
    ArmAdr register target -> "adr " <> reg register <> ", " <> T.unpack target
    ArmAdrp register target -> "adrp " <> reg register <> ", " <> T.unpack target <> "@PAGE"
    ArmAddPageOffset destination source target ->
      "add " <> reg destination <> ", " <> reg source <> ", " <> T.unpack target <> "@PAGEOFF"
    ArmMov destination source -> "mov " <> reg destination <> ", " <> valueText source
    ArmLdr register target -> "ldr " <> reg register <> ", " <> address target
    ArmLdrImmediate register literal -> "ldr " <> reg register <> ", =" <> hex literal
    ArmStr register target -> "str " <> reg register <> ", " <> address target
    ArmLdp first second target -> "ldp " <> reg first <> ", " <> reg second <> ", " <> address target
    ArmStp first second target -> "stp " <> reg first <> ", " <> reg second <> ", " <> address target
    ArmAdd destination source operand -> arithmetic "add" destination source operand
    ArmAdds destination source operand -> arithmetic "adds" destination source operand
    ArmSub destination source operand -> arithmetic "sub" destination source operand
    ArmSubs destination source operand -> arithmetic "subs" destination source operand
    ArmCmp register operand -> "cmp " <> reg register <> ", " <> valueText operand
    ArmAnd destination source operand -> three "and" destination source operand
    ArmOrr destination source operand -> arithmetic "orr" destination source operand
    ArmEor destination source operand -> three "eor" destination source operand
    ArmMvn destination source -> "mvn " <> reg destination <> ", " <> reg source
    ArmMul destination left right -> three "mul" destination left right
    ArmUmulh destination left right -> three "umulh" destination left right
    ArmSmulh destination left right -> three "smulh" destination left right
    ArmUdiv destination left right -> three "udiv" destination left right
    ArmSdiv destination left right -> three "sdiv" destination left right
    ArmMsub destination left right addend ->
      "msub " <> reg destination <> ", " <> reg left <> ", " <> reg right <> ", " <> reg addend
    ArmLsl destination source count -> shifted "lsl" destination source count
    ArmLsr destination source count -> shifted "lsr" destination source count
    ArmAsr destination source count -> shifted "asr" destination source count
    ArmCset destination condition -> "cset " <> reg destination <> ", " <> cond condition
    ArmCsinv destination left right condition -> conditional "csinv" destination left right condition
    ArmCsel destination left right condition -> conditional "csel" destination left right condition
    ArmSxtw destination source -> "sxtw " <> reg destination <> ", " <> reg (wordName source)
    ArmSxtb destination source -> "sxtb " <> reg destination <> ", " <> reg (wordName source)
    ArmSxth destination source -> "sxth " <> reg destination <> ", " <> reg (wordName source)
    ArmClz destination source -> "clz " <> reg destination <> ", " <> reg source
    ArmRbit destination source -> "rbit " <> reg destination <> ", " <> reg source
    ArmAndMask destination source bits ->
      "and " <> reg destination <> ", " <> reg source <> ", #" <> hex (2 ^ bits - 1)
    ArmCnt destination source -> "cnt v" <> show destination <> ".8b, v" <> show source <> ".8b"
    ArmAddv destination source -> "addv b" <> show destination <> ", v" <> show source <> ".8b"
    ArmFmovToFloat wide float general ->
      "fmov " <> float' wide float <> ", " <> reg general
    ArmFmovFromFloat wide general float ->
      "fmov " <> reg general <> ", " <> float' wide float
    ArmFloat operation wide destination left right ->
      case operation of
        ArmFNeg -> unaryFloat "fneg" wide destination left
        ArmFAbs -> unaryFloat "fabs" wide destination left
        ArmFSqrt -> unaryFloat "fsqrt" wide destination left
        ArmFAdd -> binaryFloat "fadd" wide destination left right
        ArmFSub -> binaryFloat "fsub" wide destination left right
        ArmFMul -> binaryFloat "fmul" wide destination left right
        ArmFDiv -> binaryFloat "fdiv" wide destination left right
    ArmFcmp wide left right -> "fcmp " <> float' wide left <> ", " <> float' wide right
    ArmFcvt wide destination source ->
      "fcvt " <> float' wide destination <> ", " <> float' (not wide) source
    ArmScvtf wide destination source -> "scvtf " <> float' wide destination <> ", " <> reg source
    ArmUcvtf wide destination source -> "ucvtf " <> float' wide destination <> ", " <> reg source
    ArmFcvtzs wide destination source -> "fcvtzs " <> reg destination <> ", " <> float' wide source
    ArmFcvtzu wide destination source -> "fcvtzu " <> reg destination <> ", " <> float' wide source
    ArmLdrb destination base offset -> narrow "ldrb" destination base offset
    ArmLdrh destination base offset -> narrow "ldrh" destination base offset
    ArmStrb source base offset -> narrow "strb" source base offset
    ArmStrh source base offset -> narrow "strh" source base offset
  where
    arithmetic name destination source operand =
      name <> " " <> reg destination <> ", " <> reg source <> ", " <> valueText operand
    three name destination left right =
      name <> " " <> reg destination <> ", " <> reg left <> ", " <> reg right
    shifted name destination source count =
      name <> " " <> reg destination <> ", " <> reg source <> ", " <> shift count
    conditional name destination left right condition =
      name <> " " <> reg destination <> ", " <> reg left <> ", " <> reg right <> ", " <> cond condition
    unaryFloat name wide destination source =
      name <> " " <> float' wide destination <> ", " <> float' wide source
    binaryFloat name wide destination left right =
      name <> " " <> float' wide destination <> ", " <> float' wide left <> ", " <> float' wide right
    narrow name register base offset =
      name <> " " <> reg register <> ", [" <> reg base <> offsetText offset <> "]"

-- | The 32-bit name of a register, which the sign-extending instructions read.
wordName :: Arm64Register -> Arm64Register
wordName register
  | register >= X0 && register <= X30 = toEnum (fromEnum register - fromEnum X0 + fromEnum W0)
  | otherwise = register

reg :: Arm64Register -> String
reg = map toLower . show

float' :: Bool -> Int -> String
float' wide index = (if wide then "d" else "s") <> show index

valueText :: Arm64Value -> String
valueText operand =
  case operand of
    Arm64RegisterValue register -> reg register
    Arm64ImmediateValue literal -> "#" <> show literal

shift :: Arm64Shift -> String
shift count =
  case count of
    Arm64RegisterShift register -> reg register
    Arm64ImmediateShift amount -> "#" <> show (toInteger amount :: Integer)

address :: Arm64Address -> String
address target =
  case target of
    Arm64Offset base offset -> "[" <> reg base <> offsetText offset <> "]"
    Arm64PreIndex base offset -> "[" <> reg base <> offsetText offset <> "]!"
    Arm64PostIndex base offset -> "[" <> reg base <> "], #" <> show offset

offsetText :: Int64 -> String
offsetText offset
  | offset == 0 = ""
  | otherwise = ", #" <> show offset

signed :: Integer -> String
signed addend
  | addend < 0 = " - " <> show (negate addend)
  | otherwise = " + " <> show addend

hex :: Integer -> String
hex literal
  | literal < 0 = "-0x" <> showHex (negate literal) ""
  | otherwise = "0x" <> showHex literal ""

cond :: Arm64Condition -> String
cond condition = map toLower (drop (length ("Arm" :: String)) (show condition))
