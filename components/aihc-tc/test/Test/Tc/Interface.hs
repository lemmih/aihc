{-# LANGUAGE OverloadedStrings #-}

module Test.Tc.Interface (tcInterfaceTests) where

import Aihc.Prim.Wiring (primTcWiring)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc
import Aihc.Tc.Types (mkTyConWithOrigin)
import Control.Exception (ErrorCall, evaluate, try)
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tcInterfaceTests :: TestTree
tcInterfaceTests =
  testGroup
    "type interface"
    [ -- Source text cannot create two interface values for one global type identity.
      -- This test verifies rejection of inconsistent internal artifacts.
      testCase "rejects conflicting type constructor interface values" $ do
        result <- try (evaluate (length (tcInterfaceTyCons (canonicalInterface <> supportInterface)))) :: IO (Either ErrorCall Int)
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "expected a conflicting interface value exception"
    ]
  where
    kinds = mkTcKinds (primTcWiring (PackageId "aihc-prim"))
    listTyCon = mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types" "[]" 1
    listKind = ForAll [] [] (TcFunTy (typeKind kinds) (typeKind kinds))
    canonicalInfo = TyConInfo "List" 1 listTyCon listKind DataTyCon Nothing
    supportInfo = TyConInfo "[]" 1 listTyCon listKind DataTyCon Nothing
    canonicalInterface = emptyTcInterface {tcInterfaceTyConMap = Map.singleton (tyConKey listTyCon) canonicalInfo}
    supportInterface = emptyTcInterface {tcInterfaceTyConMap = Map.singleton (tyConKey listTyCon) supportInfo}
