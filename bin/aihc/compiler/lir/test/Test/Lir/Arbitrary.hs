{-# LANGUAGE OverloadedStrings #-}

module Test.Lir.Arbitrary
  ( prop_lirPrettyRoundTrip,
    genModule,
  )
where

import Aihc.Lir.Parser (parseModule, renderParseError)
import Aihc.Lir.Pretty (renderModule)
import Aihc.Lir.Syntax
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, annotate, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Every generated module renders to text that parses back to the same
-- module. The generator does not keep the module well-typed; the parser and
-- the pretty-printer are independent of the linter.
prop_lirPrettyRoundTrip :: Property
prop_lirPrettyRoundTrip = property $ do
  lirModule <- forAll genModule
  let rendered = renderModule lirModule
  case parseModule rendered of
    Left err -> do
      annotate ("failed to parse generated Lir:\n" <> T.unpack rendered <> "\n\n" <> renderParseError err)
      failure
    Right reparsed -> do
      annotate ("rendered Lir:\n" <> T.unpack rendered)
      -- Float literals may be NaN, so compare the rendering of the structure.
      show reparsed === show lirModule

genModule :: Gen Module
genModule = Module <$> Gen.list (Range.linear 0 4) genItem

genItem :: Gen Item
genItem =
  Gen.frequency
    [ (4, ItemFunction <$> genFunction),
      (1, ItemExternFunction <$> (ExternFunction <$> genSymbol <*> genSignature)),
      (1, ItemGlobal <$> (Global <$> genSymbol <*> genType <*> Gen.bool)),
      (2, ItemData <$> genData),
      (1, ItemExternData <$> genSymbol),
      (1, ItemConstant <$> (Constant <$> genSymbol <*> genInteger)),
      (1, ItemInclude <$> genIncludePath)
    ]

-- | An include path, with the characters a string literal escapes.
genIncludePath :: Gen Text
genIncludePath = Gen.text (Range.linear 1 12) (Gen.frequency [(8, Gen.alphaNum), (1, Gen.element ("/._-\"\\\n" :: String))])

genFunction :: Gen Function
genFunction =
  Function
    <$> genSymbol
    <*> genLinkage
    <*> smallList genParameter
    <*> smallList genType
    <*> genConvention
    <*> Gen.list (Range.linear 1 3) genBlock

genBlock :: Gen Block
genBlock =
  Block
    <$> genLabel
    <*> smallList genParameter
    <*> Gen.list (Range.linear 0 4) genInstruction
    <*> genTerminator

genInstruction :: Gen Instruction
genInstruction = Instruction <$> Gen.list (Range.linear 0 2) genVar <*> genOperation

genOperation :: Gen Operation
genOperation =
  Gen.choice
    [ Binary <$> Gen.enumBounded <*> genType <*> genOperand <*> genOperand,
      Unary <$> Gen.enumBounded <*> genType <*> genOperand,
      Wide <$> Gen.enumBounded <*> genType <*> genOperand <*> genOperand,
      Compare <$> Gen.enumBounded <*> genType <*> genOperand <*> genOperand,
      FloatBinary <$> Gen.enumBounded <*> genType <*> genOperand <*> genOperand,
      FloatUnary <$> Gen.enumBounded <*> genType <*> genOperand,
      Convert <$> Gen.enumBounded <*> genType <*> genOperand <*> genType,
      PtrToInt <$> genOperand,
      PtrFromInt <$> genOperand,
      Select <$> genType <*> genOperand <*> genOperand <*> genOperand,
      Load <$> genType <*> genAddress <*> genNatural,
      Store <$> genType <*> genOperand <*> genAddress <*> genNatural,
      PtrAdd <$> genOperand <*> genOperand,
      StackAlloc <$> genNatural <*> genNatural,
      GlobalGet <$> genSymbol,
      GlobalSet <$> genSymbol <*> genOperand,
      Call <$> genSymbol <*> smallList genOperand,
      CallIndirect <$> genOperand <*> smallList genOperand <*> genSignature
    ]

genTerminator :: Gen Terminator
genTerminator =
  Gen.choice
    [ Jump <$> genTarget,
      Branch <$> genOperand <*> genTarget <*> genTarget,
      Switch <$> genType <*> genOperand <*> smallList (Gen.choice [SwitchCase <$> genInteger <*> genTarget, SwitchCaseConstant <$> genSymbol <*> genTarget]) <*> Gen.maybe genTarget,
      Return <$> smallList genOperand,
      TailCall <$> genSymbol <*> smallList genOperand,
      TailCallIndirect <$> genOperand <*> smallList genOperand <*> genSignature,
      Trap <$> genText
    ]

genTarget :: Gen Target
genTarget = Target <$> genLabel <*> smallList genOperand

genAddress :: Gen Address
genAddress = Address <$> genOperand <*> genInteger

genData :: Gen DataItem
genData =
  DataItem
    <$> genSymbol
    <*> genLinkage
    <*> Gen.bool
    <*> genNatural
    <*> Gen.list (Range.linear 1 4) genDataField

genDataField :: Gen DataField
genDataField =
  Gen.choice
    [ DataInt <$> Gen.element [I1, I8, I16, I32, I64] <*> genInteger,
      DataIntConstant <$> Gen.element [I1, I8, I16, I32, I64] <*> genSymbol,
      DataWordConstant <$> genSymbol,
      DataFloat <$> Gen.element [F32, F64] <*> genDouble,
      DataSymbol <$> genSymbol <*> genInteger,
      pure DataNull,
      DataWord <$> genWordFieldValue,
      DataCode <$> Gen.maybe genSymbol,
      DataBytes . BS.pack <$> Gen.list (Range.linear 0 8) (Gen.word8 Range.constantBounded),
      DataZero <$> genNatural
    ]

genSignature :: Gen Signature
genSignature = Signature <$> smallList genType <*> smallList genType <*> genConvention

genParameter :: Gen (Var, Type)
genParameter = (,) <$> genVar <*> genType

genOperand :: Gen Operand
genOperand = Gen.choice [OperandVar <$> genVar, OperandLiteral <$> genLiteral]

genLiteral :: Gen Literal
genLiteral =
  Gen.choice
    [ LitInt <$> genInteger,
      LitFloat <$> genDouble,
      pure LitNull,
      LitSymbol <$> genSymbol
    ]

genDouble :: Gen Double
genDouble =
  Gen.frequency
    [ (6, Gen.double (Range.exponentialFloatFrom 0 (-1.0e300) 1.0e300)),
      (2, fromInteger <$> Gen.integral (Range.linearFrom 0 (-1000) 1000)),
      (1, Gen.element [1 / 0, -(1 / 0), 0 / 0, -0.0, 5.0e-324, 1.7976931348623157e308])
    ]

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linearFrom 0 (-(2 ^ (64 :: Int))) (2 ^ (64 :: Int)))

-- | A @word@ field holds a value the linter accepts on every target, so the
-- round-trip generator stays inside the 32-bit range.
genWordFieldValue :: Gen Integer
genWordFieldValue = Gen.integral (Range.linearFrom 0 (-(2 ^ (31 :: Int))) (2 ^ (32 :: Int) - 1))

genNatural :: Gen Integer
genNatural = Gen.integral (Range.linear 0 128)

genType :: Gen Type
genType = Gen.enumBounded

genLinkage :: Gen Linkage
genLinkage = Gen.element [Internal, Export]

genConvention :: Gen CallingConvention
genConvention = Gen.element [AihcConvention, CConvention]

genSymbol :: Gen Symbol
genSymbol = Symbol <$> genName

genVar :: Gen Var
genVar = Var <$> genName

genLabel :: Gen Label
genLabel = Label <$> genName

-- | Bare names, names that need quotes, reserved words, and empty names.
genName :: Gen Text
genName =
  Gen.frequency
    [ (5, Gen.text (Range.linear 1 8) (Gen.element bareCharacters)),
      (2, genText),
      (1, Gen.element ["default", "jump", "return", "null", "inf", "nan", "i64", "add", "123", ".x", "$k"])
    ]
  where
    bareCharacters = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_.$"

genText :: Gen Text
genText = Gen.text (Range.linear 0 6) Gen.unicode

smallList :: Gen a -> Gen [a]
smallList = Gen.list (Range.linear 0 3)
