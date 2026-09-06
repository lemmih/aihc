{-# LANGUAGE OverloadedStrings #-}

-- | Render the AMD64 statements of a compiled module as assembly text.
--
-- The object writer needs no text, so nothing in the compiler depends on this
-- module. It exists so that a test can read the code a backend produces, and
-- so that a change of instruction selection or of register allocation shows
-- up as a diff of the fixtures in @Test\/Fixtures\/lir\/asm@ instead of as a
-- change of some bytes.
--
-- The syntax is Intel: the destination comes first, and a memory operand is
-- a bracketed base and displacement.
module Aihc.Amd64.Text
  ( renderAmd64Statements,
  )
where

import Aihc.Amd64.Assemble
import Aihc.Native.Object (SectionRole (..))
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

renderAmd64Statements :: [Amd64Statement] -> Text
renderAmd64Statements statements = T.unlines (map (T.pack . statement) statements)

statement :: Amd64Statement -> String
statement value =
  case value of
    Amd64Section role -> section role
    Amd64Align alignment -> indent (".p2align " <> show alignment)
    Amd64Global symbol -> indent (".globl " <> T.unpack symbol)
    Amd64Label symbol -> T.unpack symbol <> ":"
    Amd64Quad word -> indent (".quad " <> hex (toInteger word))
    Amd64QuadSymbol symbol -> indent (".quad " <> T.unpack symbol)
    Amd64QuadSymbolAddend symbol addend -> indent (".quad " <> T.unpack symbol <> signed (toInteger addend))
    Amd64Bytes bytes -> indent (".byte " <> intercalate ", " (map (hex . toInteger) (BS.unpack bytes)))
    Amd64Code code -> indent (instruction code)

section :: SectionRole -> String
section role =
  case role of
    TextSection -> indent ".text"
    TextConstantsSection -> indent ".section .rodata.cst8"
    ReadOnlySection -> indent ".section .rodata"
    DataSection -> indent ".data"
    NoExecuteStackSection -> indent ".section .note.GNU-stack"

indent :: String -> String
indent text = "\t" <> text

instruction :: Amd64Instruction -> String
instruction code =
  case code of
    AmdRet -> "ret"
    AmdUd2 -> "ud2"
    AmdPush register -> "push " <> reg register
    AmdPop register -> "pop " <> reg register
    AmdCall target -> "call " <> T.unpack target
    AmdCallRegister register -> "call " <> reg register
    AmdJmp target -> "jmp " <> jump target
    AmdJe target -> "je " <> T.unpack target
    AmdJne target -> "jne " <> T.unpack target
    AmdJcc condition target -> "j" <> cond condition <> " " <> T.unpack target
    AmdMov destination source -> "mov " <> reg destination <> ", " <> moveSource source
    AmdStore destination source -> "mov " <> memory destination <> ", " <> storeSource source
    AmdMovsxd destination source -> two "movsxd" destination source
    AmdMovsxByte destination source -> two "movsx" destination source
    AmdMovsxWord destination source -> two "movsx" destination source
    AmdMovzx destination source -> two "movzx" destination source
    AmdMovzxWord destination source -> two "movzx" destination source
    AmdLea destination source -> "lea " <> reg destination <> ", " <> addressText source
    AmdAdd destination source -> binary "add" destination source
    AmdSub destination source -> binary "sub" destination source
    AmdAnd destination source -> binary "and" destination source
    AmdOr destination source -> binary "or" destination source
    AmdXor destination source -> binary "xor" destination source
    AmdCmp destination source -> binary "cmp" destination source
    AmdImul destination source -> two "imul" destination source
    AmdTest destination source -> "test " <> rm destination <> ", " <> reg source
    AmdShl destination -> "shl " <> rm destination <> ", cl"
    AmdShr destination -> "shr " <> rm destination <> ", cl"
    AmdSar destination -> "sar " <> rm destination <> ", cl"
    AmdNot destination -> "not " <> rm destination
    AmdNeg destination -> "neg " <> rm destination
    AmdMul source -> "mul " <> rm source
    AmdDiv source -> "div " <> rm source
    AmdIdiv source -> "idiv " <> rm source
    AmdImulWide source -> "imul " <> rm source
    AmdCqo -> "cqo"
    AmdSet condition destination -> "set" <> cond condition <> " " <> rm destination
    AmdRetImm bytes -> "ret " <> show bytes
    AmdCmov condition destination source -> "cmov" <> cond condition <> " " <> reg destination <> ", " <> rm source
    AmdStoreByte destination source -> "mov " <> memory destination <> ", " <> reg source
    AmdStoreWord destination source -> "mov " <> memory destination <> ", " <> reg source
    AmdMovqToXmm destination source -> "movq " <> xmm destination <> ", " <> reg source
    AmdMovqFromXmm destination source -> "movq " <> reg destination <> ", " <> xmm source
    AmdMovdToXmm destination source -> "movd " <> xmm destination <> ", " <> reg source
    AmdMovdFromXmm destination source -> "movd " <> reg destination <> ", " <> xmm source
    AmdSse operation wide destination source ->
      sseName operation wide <> " " <> xmm destination <> ", " <> xmm source
    AmdUcomis wide left right ->
      (if wide then "ucomisd " else "ucomiss ") <> xmm left <> ", " <> xmm right
    AmdCvtsi2s wide destination source ->
      (if wide then "cvtsi2sd " else "cvtsi2ss ") <> xmm destination <> ", " <> reg source
    AmdCvtts2si wide destination source ->
      (if wide then "cvttsd2si " else "cvttss2si ") <> reg destination <> ", " <> xmm source
    AmdBitCount operation destination source -> two (bitCountName operation) destination source
  where
    two name destination source = name <> " " <> reg destination <> ", " <> rm source
    binary name destination source = name <> " " <> rm destination <> ", " <> binarySource source

sseName :: Amd64SseOp -> Bool -> String
sseName operation wide =
  case operation of
    SseAdd -> "add" <> suffix
    SseSub -> "sub" <> suffix
    SseMul -> "mul" <> suffix
    SseDiv -> "div" <> suffix
    SseSqrt -> "sqrt" <> suffix
    -- The conversion reads the other width, so its name is the mirror of the
    -- flag: a double result comes from a single operand.
    SseConvertWidth -> if wide then "cvtss2sd" else "cvtsd2ss"
  where
    suffix = if wide then "sd" else "ss"

bitCountName :: Amd64BitCountOp -> String
bitCountName operation =
  case operation of
    AmdPopcnt -> "popcnt"
    AmdLzcnt -> "lzcnt"
    AmdTzcnt -> "tzcnt"

reg :: Amd64Register -> String
reg = map toLower . show

xmm :: Int -> String
xmm index = "xmm" <> show index

rm :: Amd64Rm -> String
rm operand =
  case operand of
    Amd64RmRegister register -> reg register
    Amd64RmMemory place -> memory place

memory :: Amd64Memory -> String
memory (Amd64Memory base displacement) = "[" <> reg base <> offsetText displacement <> "]"

addressText :: Amd64Address -> String
addressText target =
  case target of
    Amd64MemoryAddress place -> memory place
    Amd64RipAddress symbol -> "[rip + " <> T.unpack symbol <> "]"

moveSource :: Amd64MoveSource -> String
moveSource source =
  case source of
    Amd64MoveRegister register -> reg register
    Amd64MoveMemory place -> memory place
    Amd64MoveImmediate literal -> hex literal

storeSource :: Amd64StoreSource -> String
storeSource source =
  case source of
    Amd64StoreRegister register -> reg register
    Amd64StoreImmediate literal -> hex literal

binarySource :: Amd64BinarySource -> String
binarySource source =
  case source of
    Amd64BinaryRegister register -> reg register
    Amd64BinaryImmediate literal -> hex literal

jump :: Amd64JumpTarget -> String
jump target =
  case target of
    Amd64JumpLabel label -> T.unpack label
    Amd64JumpRegister register -> reg register

offsetText :: Int64 -> String
offsetText displacement
  | displacement == 0 = ""
  | displacement < 0 = " - " <> show (negate displacement)
  | otherwise = " + " <> show displacement

signed :: Integer -> String
signed addend
  | addend < 0 = " - " <> show (negate addend)
  | otherwise = " + " <> show addend

hex :: Integer -> String
hex literal
  | literal < 0 = "-0x" <> showHex (negate literal) ""
  | otherwise = "0x" <> showHex literal ""

cond :: Amd64Condition -> String
cond condition =
  case condition of
    AmdOverflow -> "o"
    AmdNotOverflow -> "no"
    AmdCarry -> "c"
    AmdBelow -> "b"
    AmdAboveOrEqual -> "ae"
    AmdEqual -> "e"
    AmdNotEqual -> "ne"
    AmdBelowOrEqual -> "be"
    AmdAbove -> "a"
    AmdLess -> "l"
    AmdGreaterOrEqual -> "ge"
    AmdLessOrEqual -> "le"
    AmdGreater -> "g"
    AmdSign -> "s"
    AmdNotSign -> "ns"
    AmdParity -> "p"
    AmdNotParity -> "np"
