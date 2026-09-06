{-# LANGUAGE OverloadedStrings #-}

module Test.Fc.Properties
  ( fcPropertyTests,
  )
where

import Aihc.Fc
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, annotate, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

fcPropertyTests :: TestTree
fcPropertyTests =
  testGroup
    "SystemFC properties"
    [ testProperty "parseProgram . renderProgram = id" prop_programRoundTrip,
      testProperty "tidyProgram is idempotent" prop_tidyIdempotent,
      testProperty "tidyProgram output round trips" prop_tidyRoundTrip,
      testProperty "t prefix stores Bool" prop_prefixStrip,
      testProperty "uses have no type suffix" prop_noUseTypes
    ]

prop_programRoundTrip :: Property
prop_programRoundTrip = property $ do
  program <- forAll genProgram
  let printed = renderProgram program
  annotate (T.unpack printed)
  case parseProgram printed of
    Left parseError -> do
      annotate (renderParseError parseError)
      failure
    Right parsed -> parsed === program

prop_tidyIdempotent :: Property
prop_tidyIdempotent = property $ do
  program <- forAll genTidyProgram
  tidyProgram (tidyProgram program) === tidyProgram program

prop_tidyRoundTrip :: Property
prop_tidyRoundTrip = property $ do
  program <- tidyProgram <$> forAll genTidyProgram
  let printed = renderProgram program
  annotate (T.unpack printed)
  case parseProgram printed of
    Left parseError -> do
      annotate (renderParseError parseError)
      failure
    Right parsed -> parsed === program

prop_prefixStrip :: Property
prop_prefixStrip = property $ do
  let printed = renderProgram boolProgram
  annotate (T.unpack printed)
  T.isInfixOf "1.tBool" printed === True
  nameText (typeName (boolDecl boolProgram)) === "Bool"

prop_noUseTypes :: Property
prop_noUseTypes = property $ do
  let printed = renderProgram boolProgram
  annotate (T.unpack printed)
  T.isInfixOf " : tycon" printed === False
  T.isInfixOf ") : " printed === False

boolDecl :: Program -> TypeDecl
boolDecl program =
  case programDecls program of
    DeclType declaration : _ -> declaration
    _ -> error "expected Bool type"

boolProgram :: Program
boolProgram = identityProgram

primPackage :: PackageId
primPackage = PackageId "aihc-prim"

testPackage :: PackageId
testPackage = PackageId ""

scopes :: ScopeTable
scopes =
  insertScope 2 primPackage "GHC.Types" (insertScope 1 testPackage "Test" emptyScopeTable)

typeNameTop :: Text -> Name
typeNameTop text = Name text SortTypeConstructor (OriginTop testPackage "Test")

valueNameTop :: Text -> Name
valueNameTop text = Name text SortValue (OriginTop testPackage "Test")

dataNameTop :: Text -> Name
dataNameTop text = Name text SortDataConstructor (OriginTop testPackage "Test")

synonymNameTop :: Text -> Name
synonymNameTop text = Name text SortSynonym (OriginTop testPackage "Test")

axiomNameTop :: Text -> Name
axiomNameTop text = Name text SortAxiom (OriginTop testPackage "Test")

typeWired :: Text -> Name
typeWired text = Name text SortSynonym (OriginTop primPackage "GHC.Types")

localType :: Text -> Name
localType text = Name text SortTypeVariable (OriginLocal (Unique 0))

localValue :: Text -> Name
localValue text = Name text SortValue (OriginLocal (Unique 0))

localTypeWith :: Int -> Text -> Name
localTypeWith unique text = Name text SortTypeVariable (OriginLocal (Unique unique))

localValueWith :: Int -> Text -> Name
localValueWith unique text = Name text SortValue (OriginLocal (Unique unique))

identityProgram :: Program
identityProgram =
  Program
    { programScopes = scopes,
      programImports = Imports mempty mempty mempty mempty,
      programDecls =
        [ DeclType
            TypeDecl
              { typeVis = Pub,
                typeName = typeNameTop "Bool",
                typeBinders = [],
                typeResult = TyCon (typeWired "Type"),
                typeRoles = [],
                typeCons =
                  [ ConDecl Pub (dataNameTop "False") (TyCon (typeNameTop "Bool")),
                    ConDecl Pub (dataNameTop "True") (TyCon (typeNameTop "Bool"))
                  ]
              },
          DeclVal
            ValDecl
              { valVis = Private,
                valName = valueNameTop "id",
                valType =
                  TyForAll
                    (Binder (localType "a") (TyCon (typeWired "Type")))
                    ( TyFun
                        (TyCon (typeWired "LiftedRep"))
                        (TyCon (typeWired "LiftedRep"))
                        (TyVar (localType "a"))
                        (TyVar (localType "a"))
                    ),
                valBody =
                  ExTyLam
                    (Binder (localType "a") (TyCon (typeWired "Type")))
                    ( ExLam
                        (Binder (localValue "x") (TyVar (localType "a")))
                        (ExVar (localValue "x"))
                    )
              }
        ]
    }

genProgram :: Gen Program
genProgram = Program scopes <$> genImports <*> Gen.list (Range.linear 0 10) genDecl

genImports :: Gen Imports
genImports = do
  headers <- genMap genHeaderName genType
  synonyms <- genMap (synonymNameTop . ("ImportedS" <>) <$> genSuffix) genType
  axioms <- Map.fromList <$> Gen.list (Range.linear 0 5) genImportedAxiom
  binders <- genMap genLocalName genType
  pure (Imports headers synonyms axioms binders)
  where
    genMap makeName makeValue = Map.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> makeName <*> makeValue)
    genHeaderName = do
      suffix <- genSuffix
      Gen.element
        [ typeNameTop ("ImportedT" <> suffix),
          dataNameTop ("ImportedC" <> suffix),
          synonymNameTop ("ImportedS" <> suffix),
          valueNameTop ("importedV" <> suffix)
        ]
    genLocalName = Gen.choice [genLocalTypeName, genLocalValueName]
    genImportedAxiom = do
      name <- axiomNameTop . ("importedAxiom" <>) <$> genSuffix
      declaration <- AxiomDecl Private name <$> Gen.list (Range.linear 0 3) genTypeBinder <*> genRole <*> genType <*> genType
      pure (name, declaration)

genDecl :: Gen Decl
genDecl =
  Gen.choice
    [ DeclType <$> genTypeDecl,
      DeclSynonym <$> genSynonymDecl,
      DeclAxiom <$> genAxiomDecl,
      DeclVal <$> genValDecl
    ]

genTypeDecl :: Gen TypeDecl
genTypeDecl = do
  name <- typeNameTop . ("T" <>) <$> genSuffix
  binders <- Gen.list (Range.linear 0 3) genTypeBinder
  roles <- traverse (const genRole) binders
  TypeDecl
    <$> genVis
    <*> pure name
    <*> pure binders
    <*> genType
    <*> pure roles
    <*> Gen.list (Range.linear 0 3) (genConDecl name)

genConDecl :: Name -> Gen ConDecl
genConDecl typeName =
  ConDecl
    <$> genVis
    <*> (dataNameTop . ("C" <>) <$> genSuffix)
    <*> Gen.choice [pure (TyCon typeName), genType]

genSynonymDecl :: Gen SynonymDecl
genSynonymDecl =
  SynonymDecl
    <$> genVis
    <*> (synonymNameTop . ("S" <>) <$> genSuffix)
    <*> Gen.list (Range.linear 0 3) genTypeBinder
    <*> genType
    <*> genType

genAxiomDecl :: Gen AxiomDecl
genAxiomDecl =
  AxiomDecl
    <$> genVis
    <*> (axiomNameTop . ("axiom" <>) <$> genSuffix)
    <*> Gen.list (Range.linear 0 3) genTypeBinder
    <*> genRole
    <*> genType
    <*> genType

genValDecl :: Gen ValDecl
genValDecl =
  ValDecl
    <$> genVis
    <*> (valueNameTop . ("f" <>) <$> genSuffix)
    <*> genType
    <*> Gen.choice [ExVar <$> genLocalValueName, genForeignCallExpr]

genForeignCallExpr :: Gen Expr
genForeignCallExpr =
  ExForeignCall
    <$> genForeignCall
    <*> Gen.list (Range.linear 0 2) genType
    <*> Gen.list (Range.linear 0 3) (ExVar <$> genLocalValueName)

genForeignCall :: Gen ForeignCall
genForeignCall =
  (ForeignCall . valueNameTop . ("foreign" <>) <$> genSuffix)
    <*> genCallingConvention
    <*> Gen.list (Range.linear 0 4) genForeignImportDependency
    <*> genType

genForeignImportDependency :: Gen ForeignImportDependency
genForeignImportDependency =
  Gen.choice
    [ ForeignAxiom . axiomNameTop . ("foreignAxiom" <>) <$> genSuffix,
      ForeignConstructor . dataNameTop . ("ForeignConstructor" <>) <$> genSuffix
    ]

genCallingConvention :: Gen CallingConvention
genCallingConvention =
  Gen.choice
    [ pure Prim,
      CCall <$> genCCallSpec
    ]

genCCallSpec :: Gen CCallSpec
genCCallSpec =
  CCallSpec
    <$> genForeignSymbol
    <*> genCCallTarget
    <*> genForeignSafety
    <*> Gen.list (Range.linear 0 6) genCAbiType
    <*> genCAbiType
    <*> genForeignEffect

genCCallTarget :: Gen CCallTarget
genCCallTarget = Gen.element [CCallFunction, CCallAddress]

genForeignSymbol :: Gen Text
genForeignSymbol =
  Gen.text
    (Range.linear 0 16)
    ( Gen.frequency
        [ (12, Gen.alphaNum),
          (1, Gen.element ['_', '.', '-', ' ', '\'', '"', '\\', '\n'])
        ]
    )

genCAbiType :: Gen CAbiType
genCAbiType = Gen.element [minBound .. maxBound]

genForeignSafety :: Gen ForeignSafety
genForeignSafety = Gen.element [ForeignUnsafe, ForeignSafe]

genForeignEffect :: Gen ForeignEffect
genForeignEffect = Gen.element [ForeignPure, ForeignRealWorld]

genType :: Gen Type
genType =
  Gen.recursive
    Gen.choice
    [ TyVar <$> genLocalTypeName,
      TyCon <$> genTypeName
    ]
    [ TyApp <$> genType <*> genType,
      TyFun <$> genType <*> genType <*> genType <*> genType,
      TyForAll <$> genTypeBinder <*> genType,
      TyEq <$> genType <*> genType
    ]

genTypeName :: Gen Name
genTypeName =
  Gen.choice
    [ pure (typeWired "Type"),
      pure (typeWired "LiftedRep"),
      pure (typeWired "UnliftedRep"),
      typeNameTop . ("T" <>) <$> genSuffix
    ]

genTypeBinder :: Gen Binder
genTypeBinder =
  Binder
    <$> genLocalTypeName
    <*> pure (TyCon (typeWired "Type"))

genLocalTypeName :: Gen Name
genLocalTypeName =
  localTypeWith
    <$> Gen.int (Range.linear 0 10000)
    <*> (("tv" <>) <$> genSuffix)

genLocalValueName :: Gen Name
genLocalValueName =
  localValueWith
    <$> Gen.int (Range.linear 0 10000)
    <*> (("x" <>) <$> genSuffix)

genVis :: Gen Vis
genVis = Gen.element [Pub, Private]

genRole :: Gen Role
genRole = Gen.element [Nominal, Representational, Phantom]

genSuffix :: Gen Text
genSuffix = Gen.text (Range.linear 1 4) Gen.lower

genTidyProgram :: Gen Program
genTidyProgram = do
  typeUnique <- Gen.int (Range.linear 0 10000)
  outerUnique <- Gen.int (Range.linear 0 10000)
  innerOffset <- Gen.int (Range.linear 1 10000)
  let typeVar = localTypeWith typeUnique "a"
      outer = localValueWith outerUnique "a"
      inner = localValueWith (outerUnique + innerOffset) "a"
      kind = TyCon (typeWired "Type")
      valueType = TyVar typeVar
      lifted = TyCon (typeWired "LiftedRep")
      functionType = TyFun lifted lifted valueType valueType
  pure
    Program
      { programScopes = scopes,
        programImports = Imports mempty mempty mempty mempty,
        programDecls =
          [ DeclVal
              ValDecl
                { valVis = Pub,
                  valName = valueNameTop "shadow",
                  valType = TyForAll (Binder typeVar kind) functionType,
                  valBody =
                    ExTyLam
                      (Binder typeVar kind)
                      ( ExLam
                          (Binder outer valueType)
                          ( ExApp
                              (ExLam (Binder inner valueType) (ExVar inner))
                              (ExVar outer)
                          )
                      )
                }
          ]
      }
