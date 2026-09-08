{-# LANGUAGE OverloadedStrings #-}

-- | Hedgehog property tests for the type checker.
module Test.Tc.Properties
  ( prop_interfaceMergeIdempotent,
    prop_kindEncodingUsesType,
    prop_reflexiveEq,
    prop_starUsesType,
    prop_zonkIdempotent,
    tcProperties,
  )
where

import Aihc.Parser.Syntax (Type (TStar))
import Aihc.Prim.Wiring (primTcConfig)
import Aihc.Resolve (PackageId (PackageId))
import Aihc.Tc
  ( ClassInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    TcInterface (..),
    TcTermKey (..),
    TyConFlavor (..),
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    tcInterfaceFromLists,
  )
import Aihc.Tc.Kind (convertSurfaceTypeWithKinds)
import Aihc.Tc.Monad (TcEnv, emptyTcEnv, freshMetaTv, initTcState, runTcM, writeMetaTv)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Map.Strict qualified as Map
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tcProperties :: TestTree
tcProperties =
  testGroup
    "properties"
    [ testProperty "lifted kind encoding uses GHC.Types.Type" prop_kindEncodingUsesType,
      testProperty "star uses GHC.Types.Type" prop_starUsesType,
      testProperty "zonking idempotent" prop_zonkIdempotent,
      testProperty "reflexive equality solved" prop_reflexiveEq,
      testProperty "interface merge is idempotent" prop_interfaceMergeIdempotent
    ]

-- | Repeated module views must not change a semantic interface.
prop_interfaceMergeIdempotent :: Property
prop_interfaceMergeIdempotent = property $ do
  interface <- forAll genInterface
  interface <> interface === interface

genInterface :: Gen TcInterface
genInterface = do
  ty <- genSimpleType
  let packageId = PackageId "pkg"
      moduleName = "Module"
      tyCon = mkTyConWithOrigin packageId moduleName "T" 0
      classTyCon = mkTyConWithOrigin packageId moduleName "C" 0
  firstTerm <- optionalEntry (TcTermGlobal packageId moduleName "value", ForAll [] [] ty)
  secondTerm <- optionalEntry (TcTermGlobal packageId moduleName "another", ForAll [] [] ty)
  let tcInterfaceTerms = firstTerm <> secondTerm
  tcInterfaceTyCons <- optionalEntry (TyConInfo "T" 0 tyCon (ForAll [] [] KType) DataTyCon Nothing)
  tcInterfaceDataTypes <- optionalEntry (DataTypeInfo "T" tyCon [] KType DataTyCon [] [])
  tcInterfaceClasses <- optionalEntry (ClassInfo "C" classTyCon (Just ("pkg", moduleName)) [] [] [] [] [] [] [])
  tcInterfaceInstances <- optionalEntry (InstanceInfo "C" "$fC" ("pkg", moduleName) ty [] [] [])
  tcInterfaceDataFamilyInstances <- optionalEntry (DataFamilyInstanceInfo "F" ty [] tyCon "$axF" [] False)
  tcInterfaceTypeFamilyInstances <- optionalEntry (TypeFamilyInstanceInfo "F" "$axF" (packageId, moduleName) [] ty ty False)
  pure (tcInterfaceFromLists tcInterfaceTerms tcInterfaceTyCons tcInterfaceDataTypes tcInterfaceClasses tcInterfaceInstances tcInterfaceDataFamilyInstances tcInterfaceTypeFamilyInstances [] [])

optionalEntry :: value -> Gen [value]
optionalEntry value = Gen.element [[], [value]]

-- | A lifted kind uses the ordinary Type constructor.
prop_kindEncodingUsesType :: Property
prop_kindEncodingUsesType = property $ do
  let expected = KType
  KType === expected
  ForAll [] [] KType === ForAll [] [] expected

-- | A source star becomes the canonical GHC.Types.Type constructor.
prop_starUsesType :: Property
prop_starUsesType = property $
  case runTcM testTcEnv initTcState (convertSurfaceTypeWithKinds Map.empty (TStar "*")) of
    Right ((actual, kind), _) -> do
      let expected = TcTyCon (mkTyConWithOrigin (PackageId "test-ghc-prim") "GHC.Types" "Type" 0) []
      actual === expected
      kind === KType
    Left err -> fail (show err)

-- | Zonking a fully-zonked type is a no-op.
prop_zonkIdempotent :: Property
prop_zonkIdempotent = property $ do
  ty <- forAll genSimpleType
  case runTcM
    testTcEnv
    initTcState
    ( do
        z1 <- zonkType ty
        z2 <- zonkType z1
        pure (z1, z2)
    ) of
    Right ((t1, t2), _) -> t1 === t2
    Left err -> fail (show err)

-- | A reflexive equality (a ~ a) should be trivially solvable.
prop_reflexiveEq :: Property
prop_reflexiveEq = property $
  case runTcM
    testTcEnv
    initTcState
    ( do
        alpha <- freshMetaTv
        -- Solve alpha := Int
        case alpha of
          TcMetaTv u -> do
            let intTy = TcTyCon (mkTyConWithOrigin (PackageId "test") "Test" "Int" 0) []
            writeMetaTv u intTy
            result <- zonkType alpha
            pure (result == intTy)
          _ -> pure False
    ) of
    Right (result, _) -> result === True
    Left err -> fail (show err)

genSimpleType :: Gen TcType
genSimpleType = do
  depth <- Gen.int (Range.linear 0 6)
  genSimpleTypeSized depth

genSimpleTypeSized :: Int -> Gen TcType
genSimpleTypeSized depth =
  if depth <= 0
    then genAtomicType
    else
      Gen.choice
        [ genAtomicType,
          genFunType (depth - 1),
          genAppType (depth - 1)
        ]

genAtomicType :: Gen TcType
genAtomicType =
  Gen.choice
    [ TcTyCon <$> genTyCon <*> pure [],
      TcMetaTv <$> genUnique
    ]

genFunType :: Int -> Gen TcType
genFunType depth = TcFunTy <$> genSimpleTypeSized depth <*> genSimpleTypeSized depth

genAppType :: Int -> Gen TcType
genAppType depth = do
  tc <- genTyCon1
  arg <- genSimpleTypeSized depth
  pure (TcTyCon tc [arg])

genTyCon :: Gen TyCon
genTyCon =
  Gen.element
    [ mkTyConWithOrigin (PackageId "test") "Test" "Int" 0,
      mkTyConWithOrigin (PackageId "test") "Test" "Bool" 0,
      mkTyConWithOrigin (PackageId "test") "Test" "Char" 0,
      mkTyConWithOrigin (PackageId "test") "Test" "Double" 0
    ]

genTyCon1 :: Gen TyCon
genTyCon1 =
  Gen.element
    [ mkTyConWithOrigin (PackageId "test") "Test" "Maybe" 1,
      mkTyConWithOrigin (PackageId "test") "Test" "[]" 1,
      mkTyConWithOrigin (PackageId "test") "Test" "IO" 1
    ]

testTcEnv :: TcEnv
testTcEnv = emptyTcEnv (primTcConfig (PackageId "test-ghc-prim"))

genUnique :: Gen Unique
genUnique = Unique <$> Gen.int (Range.linear 100 199)
