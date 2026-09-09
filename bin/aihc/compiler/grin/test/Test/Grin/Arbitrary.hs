{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Arbitrary
  ( prop_grinPrettyRoundTrip,
  )
where

import Aihc.Grin.Parser (parseProgram, renderParseError)
import Aihc.Grin.Pretty (prettyProgram)
import Aihc.Grin.Syntax
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, annotate, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)

prop_grinPrettyRoundTrip :: Property
prop_grinPrettyRoundTrip = property $ do
  program <- forAll genGrinProgram
  let rendered = T.pack (renderString (layoutPretty defaultLayoutOptions (prettyProgram program)))
  case parseProgram rendered of
    Left err -> do
      annotate ("failed to parse generated GRIN:\n" <> T.unpack rendered <> "\n\n" <> renderParseError err)
      failure
    Right reparsed -> do
      -- GrinVar's semantic Eq instance intentionally ignores runtime
      -- representations, so compare the complete derived structure.
      annotate ("rendered GRIN:\n" <> T.unpack rendered)
      show reparsed === show program

genGrinProgram :: Gen GrinProgram
genGrinProgram =
  GrinProgram
    <$> smallList ((,) <$> genName <*> smallList (smallList genRuntimeRep))
    <*> smallList ((,) <$> genVar <*> genInt)
    <*> smallList genForeignCall
    <*> smallList ((,) <$> genName <*> genNode)
    <*> smallList genFunction

genFunction :: Gen GrinFunction
genFunction =
  GrinFunction
    <$> genFunctionName
    <*> smallList genVar
    <*> genRuntimeRep
    <*> genExpr

genExpr :: Gen GrinExpr
genExpr =
  Gen.recursive
    Gen.choice
    [ GrinConstant <$> smallList genValue,
      GrinStore <$> genNode,
      GrinEnsureHeap <$> genValue <*> smallList genValue,
      GrinStoreUnchecked <$> genNode,
      GrinUpdate <$> genValue <*> genValue,
      GrinEval <$> genRuntimeRep <*> genValue,
      GrinCpsEval <$> genRuntimeRep <*> genValue <*> genValue <*> genValue,
      GrinCall <$> genRuntimeRep <*> genFunctionName <*> smallList genValue,
      GrinPrimitiveCall <$> genRuntimeRep <*> genText <*> smallList genValue,
      GrinCpsPrimitiveCall <$> genRuntimeRep <*> genText <*> smallList genValue <*> genValue,
      GrinApply <$> genRuntimeRep <*> genValue <*> smallList genValue,
      GrinCpsApply <$> genRuntimeRep <*> genValue <*> smallList genValue <*> genValue,
      GrinContinue <$> genValue <*> smallList genValue,
      GrinCpsRaise <$> genValue <*> genValue,
      GrinUpdateBlackhole <$> genValue <*> genValue,
      GrinHalt <$> smallList genValue,
      GrinExit <$> genValue,
      GrinThrow <$> genValue,
      GrinCatch <$> genRuntimeRep <*> genValue <*> genValue <*> smallList genValue,
      GrinForeignCallExpr <$> genForeignCall <*> smallList genValue
    ]
    [ GrinBind <$> smallList genVar <*> genExpr <*> genExpr,
      GrinStoreRec <$> smallList ((,) <$> genVar <*> genNode) <*> genExpr,
      GrinStoreRecUnchecked <$> smallList ((,) <$> genVar <*> genNode) <*> genExpr,
      GrinCase <$> genValue <*> genVar <*> smallList (genAlt genExpr)
    ]

genAlt :: Gen GrinExpr -> Gen GrinAlt
genAlt rhs = GrinAlt <$> genAltCon <*> smallList genVar <*> rhs

genAltCon :: Gen GrinAltCon
genAltCon =
  Gen.choice
    [ GrinDataAlt <$> genName,
      GrinLitAlt <$> genLiteral,
      pure GrinDefaultAlt
    ]

genValue :: Gen GrinValue
genValue = Gen.choice [GrinVarValue <$> genVar, GrinGlobalValue <$> genName, GrinLitValue <$> genLiteral]

genNode :: Gen GrinNode
genNode = GrinNode <$> genNodeTag <*> smallList genValue

genNodeTag :: Gen GrinNodeTag
genNodeTag =
  Gen.choice
    [ GrinConstructor <$> genName <*> genInt,
      GrinClosure <$> genFunctionName <*> smallList (smallList genRuntimeRep),
      GrinThunk <$> genFunctionName
    ]

genLiteral :: Gen GrinLiteral
genLiteral =
  Gen.choice
    [ GrinLitInt <$> genRuntimeRep <*> Gen.integral (Range.linearFrom 0 (-100000) 100000),
      GrinLitChar <$> genRuntimeRep <*> Gen.unicodeAll,
      GrinLitAddr . BS.pack <$> smallList (Gen.word8 Range.constantBounded)
    ]

genVar :: Gen GrinVar
genVar = GrinVar <$> genText <*> genInt <*> genRuntimeRep

genFunctionName :: Gen FunctionName
genFunctionName = FunctionName <$> genText

genForeignCall :: Gen GrinForeignCall
genForeignCall =
  GrinForeignCall
    <$> genText
    <*> genText
    <*> genForeignTarget
    <*> (GrinForeignSignature <$> smallList genForeignType <*> genForeignType <*> genForeignEffect)

genForeignTarget :: Gen GrinForeignTarget
genForeignTarget = Gen.element [GrinForeignFunction, GrinForeignAddress]

genForeignEffect :: Gen GrinForeignEffect
genForeignEffect = Gen.element [GrinForeignPure, GrinForeignRealWorld]

genForeignType :: Gen GrinForeignType
genForeignType = Gen.element [minBound .. maxBound]

genRuntimeRep :: Gen GrinRep
genRuntimeRep =
  Gen.recursive
    Gen.choice
    [ VecRep <$> Gen.element allVecCounts <*> Gen.element allVecElems,
      BoxedRep <$> Gen.element [Lifted, Unlifted],
      Gen.element
        [ IntRep,
          Int8Rep,
          Int16Rep,
          Int32Rep,
          Int64Rep,
          WordRep,
          Word8Rep,
          Word16Rep,
          Word32Rep,
          Word64Rep,
          AddrRep,
          FloatRep,
          DoubleRep
        ]
    ]
    [ TupleRep <$> smallList genRuntimeRep,
      SumRep <$> smallList genRuntimeRep
    ]

allVecCounts :: [GrinVecCount]
allVecCounts = [Vec2, Vec4, Vec8, Vec16, Vec32, Vec64]

allVecElems :: [GrinVecElem]
allVecElems =
  [ Int8ElemRep,
    Int16ElemRep,
    Int32ElemRep,
    Int64ElemRep,
    Word8ElemRep,
    Word16ElemRep,
    Word32ElemRep,
    Word64ElemRep,
    FloatElemRep,
    DoubleElemRep
  ]

-- | A top-level name. Half of these names have a scope, so that the printer
-- and the parser meet numbered scopes often.
genName :: Gen Text
genName =
  Gen.choice
    [ genText,
      grinScopedName <$> genText <*> genText <*> genText
    ]

genText :: Gen Text
genText = T.pack <$> Gen.string (Range.linear 0 8) Gen.unicodeAll

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-1000) 1000)

smallList :: Gen value -> Gen [value]
smallList = Gen.list (Range.linear 0 3)
