module ParserFuzz.Arbitrary
  ( generateCandidate,
    normalizeCandidateAst,
    qcGenStream,
    shrinkGeneratedModule,
  )
where

import Data.Functor (void)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts qualified as HSE
import Language.Haskell.Exts.Build qualified as HSEB
import ParserFuzz.Types (Candidate (..))
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (QCGen, mkQCGen)

newtype GenModule = GenModule {unGenModule :: HSE.Module ()}
  deriving (Show)

newtype GenModuleHead = GenModuleHead {unGenModuleHead :: Maybe (HSE.ModuleHead ())}
  deriving (Show)

newtype GenDecl = GenDecl {unGenDecl :: HSE.Decl ()}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    GenModuleHead header <- arbitrary
    declCount <- chooseInt (0, 8)
    decls <- vectorOf declCount (unGenDecl <$> (arbitrary :: Gen GenDecl))
    pure (GenModule (HSE.Module () header [] [] decls))

  shrink (GenModule modu) =
    case modu of
      HSE.Module () header pragmas imports decls ->
        [ GenModule (HSE.Module () header' pragmas imports decls)
        | GenModuleHead header' <- shrink (GenModuleHead header)
        ]
          <> [GenModule (HSE.Module () header pragmas imports decls') | decls' <- shrinkList shrinkDecl decls]
      HSE.XmlPage {} -> []
      HSE.XmlHybrid {} -> []
    where
      shrinkDecl decl = unGenDecl <$> shrink (GenDecl decl)

instance Arbitrary GenModuleHead where
  arbitrary =
    oneof
      [ pure (GenModuleHead Nothing),
        do
          moduName <- genModuleName
          exports <- genMaybeExportSpecList
          let header = HSE.ModuleHead () (HSE.ModuleName () moduName) Nothing exports
          pure (GenModuleHead (Just header))
      ]

  shrink (GenModuleHead Nothing) = []
  shrink (GenModuleHead (Just (HSE.ModuleHead () (HSE.ModuleName () moduName) warning exports))) =
    [GenModuleHead Nothing]
      <> [GenModuleHead (Just (HSE.ModuleHead () (HSE.ModuleName () moduName') warning exports)) | moduName' <- shrinkModuleName moduName]
      <> [GenModuleHead (Just (HSE.ModuleHead () (HSE.ModuleName () moduName) warning exports')) | exports' <- shrinkMaybeExportSpecList exports]

instance Arbitrary GenDecl where
  arbitrary = GenDecl <$> genDataDecl
  shrink (GenDecl decl) =
    case decl of
      HSE.DataDecl () dataOrNew mContext declHead constructors derivings ->
        [ GenDecl (HSE.DataDecl () dataOrNew' mContext declHead constructors derivings)
        | dataOrNew' <- shrinkDataOrNew dataOrNew
        ]
          <> [ GenDecl (HSE.DataDecl () dataOrNew mContext' declHead constructors derivings)
             | mContext' <- shrinkMaybeContext mContext
             ]
          <> [ GenDecl (HSE.DataDecl () dataOrNew mContext declHead' constructors derivings)
             | declHead' <- shrinkDeclHead declHead
             ]
          <> [ GenDecl (HSE.DataDecl () dataOrNew mContext declHead constructors' derivings)
             | constructors' <- shrinkConstructors dataOrNew constructors
             ]
          <> [ GenDecl (HSE.DataDecl () dataOrNew mContext declHead constructors derivings')
             | derivings' <- shrinkDerivings derivings
             ]
      _ -> []

-- TODO: Add generators for module pragmas/imports/declarations.
-- TODO: Extend generator to cover additional Haskell syntax supported by HSE.

generateCandidate :: Int -> QCGen -> Candidate
generateCandidate size seed =
  let generated = unGen (arbitrary :: Gen GenModule) seed size
   in materializeCandidate generated

materializeCandidate :: GenModule -> Candidate
materializeCandidate (GenModule modu0) =
  case normalizeCandidateSource "materializeCandidate" (HSE.prettyPrint modu0) of
    Left msg -> error msg
    Right candidate -> candidate

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<generated>",
      HSE.extensions = HSE.glasgowExts
    }

normalizeCandidateAst :: String -> HSE.Module HSE.SrcSpanInfo -> Either String Candidate
normalizeCandidateAst context modu0 =
  normalizeCandidateSource context (HSE.prettyPrint modu0)

normalizeCandidateSource :: String -> String -> Either String Candidate
normalizeCandidateSource context source0 =
  let mode = hseParseMode
   in case HSE.parseFileContentsWithMode mode source0 of
        HSE.ParseFailed loc err ->
          Left
            ( context
                <> ": internal bug: candidate AST failed to parse after prettyPrint at "
                <> show loc
                <> " with error: "
                <> err
                <> "\nsource:\n"
                <> source0
            )
        HSE.ParseOk modu1 ->
          let source1 = HSE.exactPrint modu1 []
           in case HSE.parseFileContentsWithMode mode source1 of
                HSE.ParseFailed loc err ->
                  Left
                    ( context
                        <> ": internal bug: normalized exactPrint failed to parse at "
                        <> show loc
                        <> " with error: "
                        <> err
                        <> "\nsource:\n"
                        <> source1
                    )
                HSE.ParseOk modu2 ->
                  Right
                    Candidate
                      { candAst = modu2,
                        candSource = HSE.exactPrint modu2 []
                      }

shrinkGeneratedModule :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkGeneratedModule modu =
  mapMaybe (reparseShrunk . unGenModule) (shrink (GenModule (void modu)))
  where
    reparseShrunk :: HSE.Module () -> Maybe (HSE.Module HSE.SrcSpanInfo)
    reparseShrunk shrunkMod =
      case HSE.parseFileContentsWithMode hseParseMode (HSE.prettyPrint shrunkMod) of
        HSE.ParseFailed _ _ -> Nothing
        HSE.ParseOk parsed -> Just parsed

genDataDecl :: Gen (HSE.Decl ())
genDataDecl = do
  dataOrNew <- genDataOrNew
  mContext <- genMaybeContext
  declHead <- genDeclHead
  constructors <- genConstructors dataOrNew
  HSE.DataDecl () dataOrNew mContext declHead constructors <$> genDerivings

genDataOrNew :: Gen (HSE.DataOrNew ())
genDataOrNew = elements [HSE.DataType (), HSE.NewType ()]

shrinkDataOrNew :: HSE.DataOrNew () -> [HSE.DataOrNew ()]
shrinkDataOrNew _ = []

genMaybeContext :: Gen (Maybe (HSE.Context ()))
genMaybeContext =
  frequency
    [ (3, pure Nothing),
      (7, Just <$> genContext)
    ]

genContext :: Gen (HSE.Context ())
genContext =
  oneof
    [ HSE.CxSingle () <$> genAsst,
      do
        n <- chooseInt (2, 3)
        HSE.CxTuple () <$> vectorOf n genAsst
    ]

genAsst :: Gen (HSE.Asst ())
genAsst = do
  className <- genConIdent
  argCount <- chooseInt (0, 2)
  args <- vectorOf argCount (HSE.TyVar () <$> genVarNameNode)
  pure (HSE.TypeA () (foldl (HSE.TyApp ()) (HSE.TyCon () (HSE.UnQual () (HSE.Ident () className))) args))

genDeclHead :: Gen (HSE.DeclHead ())
genDeclHead = do
  typeName <- genConNameNode
  paramCount <- chooseInt (0, 3)
  params <- vectorOf paramCount (HSE.UnkindedVar () <$> genVarNameNode)
  pure (foldl (HSE.DHApp ()) (HSE.DHead () typeName) params)

shrinkDeclHead :: HSE.DeclHead () -> [HSE.DeclHead ()]
shrinkDeclHead declHead =
  let (headName, params) = flattenDeclHead declHead
   in [buildDeclHead headName params' | params' <- shrinkList shrinkTyVarBind params]
        <> [buildDeclHead headName' params | headName' <- shrinkConNameNode headName]

flattenDeclHead :: HSE.DeclHead () -> (HSE.Name (), [HSE.TyVarBind ()])
flattenDeclHead declHead =
  case declHead of
    HSE.DHead () name -> (name, [])
    HSE.DHApp () head0 param ->
      let (name, params) = flattenDeclHead head0
       in (name, params <> [param])
    HSE.DHParen () head0 -> flattenDeclHead head0
    HSE.DHInfix () lhs name ->
      (name, [lhs])

buildDeclHead :: HSE.Name () -> [HSE.TyVarBind ()] -> HSE.DeclHead ()
buildDeclHead name = foldl (HSE.DHApp ()) (HSE.DHead () name)

shrinkTyVarBind :: HSE.TyVarBind () -> [HSE.TyVarBind ()]
shrinkTyVarBind tyVarBind =
  case tyVarBind of
    HSE.UnkindedVar () name -> HSE.UnkindedVar () <$> shrinkVarNameNode name
    HSE.KindedVar () name _kind -> [HSE.UnkindedVar () name] <> (HSE.UnkindedVar () <$> shrinkVarNameNode name)

shrinkMaybeExportSpecList :: Maybe (HSE.ExportSpecList ()) -> [Maybe (HSE.ExportSpecList ())]
shrinkMaybeExportSpecList mExports =
  case mExports of
    Nothing -> []
    Just (HSE.ExportSpecList () specs) ->
      Nothing : [Just (HSE.ExportSpecList () specs') | specs' <- shrinkList shrinkExportSpec specs]

shrinkExportSpec :: HSE.ExportSpec () -> [HSE.ExportSpec ()]
shrinkExportSpec spec =
  case spec of
    HSE.EVar () qname -> HSE.EVar () <$> shrinkQNameNode qname
    HSE.EAbs () ns qname -> HSE.EAbs () ns <$> shrinkQNameNode qname
    HSE.EThingWith () noWildcard qname cnames ->
      [HSE.EAbs () (HSE.NoNamespace ()) qname]
        <> [HSE.EThingWith () noWildcard qname' cnames | qname' <- shrinkQNameNode qname]
        <> [HSE.EThingWith () noWildcard qname cnames' | cnames' <- shrinkList shrinkCName cnames]
    HSE.EModuleContents () moduleName -> HSE.EModuleContents () <$> shrinkModuleNameNode moduleName

shrinkCName :: HSE.CName () -> [HSE.CName ()]
shrinkCName cname =
  case cname of
    HSE.VarName () name -> HSE.VarName () <$> shrinkVarNameNode name
    HSE.ConName () name -> HSE.ConName () <$> shrinkConNameNode name

shrinkMaybeContext :: Maybe (HSE.Context ()) -> [Maybe (HSE.Context ())]
shrinkMaybeContext mContext =
  case mContext of
    Nothing -> []
    Just context ->
      Nothing : [Just context' | context' <- shrinkContext context]

shrinkContext :: HSE.Context () -> [HSE.Context ()]
shrinkContext context =
  case context of
    HSE.CxEmpty () -> []
    HSE.CxSingle () asst -> HSE.CxEmpty () : [HSE.CxSingle () asst' | asst' <- shrinkAsst asst]
    HSE.CxTuple () assts ->
      HSE.CxEmpty ()
        : [HSE.CxSingle () asst | asst <- assts]
          <> [HSE.CxTuple () assts' | assts' <- shrinkList shrinkAsst assts]

shrinkAsst :: HSE.Asst () -> [HSE.Asst ()]
shrinkAsst asst =
  case asst of
    HSE.TypeA () ty -> HSE.TypeA () <$> shrinkType ty
    HSE.IParam {} -> []
    HSE.ParenA () asst0 -> asst0 : [HSE.ParenA () asst1 | asst1 <- shrinkAsst asst0]

genConstructors :: HSE.DataOrNew () -> Gen [HSE.QualConDecl ()]
genConstructors dataOrNew =
  case dataOrNew of
    HSE.NewType () -> (: []) <$> genNewtypeConstructor
    HSE.DataType () -> do
      count <- chooseInt (0, 4)
      vectorOf count genConstructor

shrinkConstructors :: HSE.DataOrNew () -> [HSE.QualConDecl ()] -> [[HSE.QualConDecl ()]]
shrinkConstructors dataOrNew constructors =
  case dataOrNew of
    HSE.NewType () ->
      [ [constructor']
      | [constructor] <- [constructors],
        constructor' <- shrinkConstructor constructor,
        isValidNewtypeConstructor constructor'
      ]
    HSE.DataType () ->
      shrinkList shrinkConstructor constructors

genConstructor :: Gen (HSE.QualConDecl ())
genConstructor = do
  constructorName <- genConNameNode
  pure (HSE.QualConDecl () Nothing Nothing (HSE.ConDecl () constructorName []))

genNewtypeConstructor :: Gen (HSE.QualConDecl ())
genNewtypeConstructor = do
  constructorName <- genConNameNode
  fieldTy <- genTypeField
  pure (HSE.QualConDecl () Nothing Nothing (HSE.ConDecl () constructorName [fieldTy]))

genTypeField :: Gen (HSE.Type ())
genTypeField =
  frequency
    [ (3, HSE.TyVar () <$> genVarNameNode),
      (2, HSE.TyCon () . HSE.UnQual () <$> genConNameNode),
      (1, HSE.TyList () . HSE.TyVar () <$> genVarNameNode)
    ]

isValidNewtypeConstructor :: HSE.QualConDecl () -> Bool
isValidNewtypeConstructor qualConDecl =
  case qualConDecl of
    HSE.QualConDecl () _ _ conDecl ->
      case conDecl of
        HSE.ConDecl () _ [_] -> True
        HSE.RecDecl () _ fields -> not (null fields)
        _ -> False

shrinkConstructor :: HSE.QualConDecl () -> [HSE.QualConDecl ()]
shrinkConstructor (HSE.QualConDecl () tyVars mContext conDecl) =
  [HSE.QualConDecl () tyVars mContext conDecl' | conDecl' <- shrinkConDecl conDecl]
    <> [HSE.QualConDecl () tyVars mContext' conDecl | mContext' <- shrinkMaybeContext mContext]
    <> [ HSE.QualConDecl () tyVars' mContext conDecl
       | tyVars' <- shrinkTyVars tyVars
       ]

shrinkTyVars :: Maybe [HSE.TyVarBind ()] -> [Maybe [HSE.TyVarBind ()]]
shrinkTyVars mTyVars =
  case mTyVars of
    Nothing -> []
    Just tyVars ->
      Nothing : [Just tyVars' | tyVars' <- shrinkList shrinkTyVarBind tyVars]

shrinkConDecl :: HSE.ConDecl () -> [HSE.ConDecl ()]
shrinkConDecl conDecl =
  case conDecl of
    HSE.ConDecl () name tys ->
      [HSE.ConDecl () name' tys | name' <- shrinkConNameNode name]
        <> [HSE.ConDecl () name tys' | tys' <- shrinkList shrinkType tys]
    HSE.InfixConDecl () lhs name rhs ->
      [HSE.ConDecl () name [lhs, rhs]]
    HSE.RecDecl () name fields ->
      [HSE.ConDecl () name (fieldDeclType <$> fields)]

fieldDeclType :: HSE.FieldDecl () -> HSE.Type ()
fieldDeclType (HSE.FieldDecl () _ ty) = ty

genDerivings :: Gen [HSE.Deriving ()]
genDerivings =
  frequency
    [ (6, pure []),
      (4, (: []) <$> genDeriving)
    ]

shrinkDerivings :: [HSE.Deriving ()] -> [[HSE.Deriving ()]]
shrinkDerivings derivings =
  [[] | not (null derivings)]
    <> [ [deriving']
       | [deriving0] <- [derivings],
         deriving' <- shrinkDeriving deriving0
       ]

genDeriving :: Gen (HSE.Deriving ())
genDeriving = do
  classCount <- chooseInt (1, 3)
  classNames <- vectorOf classCount genConIdent
  let instRules =
        [ HSE.IRule () Nothing Nothing (HSE.IHCon () (HSE.UnQual () (HSE.Ident () className)))
        | className <- classNames
        ]
  pure (HSE.Deriving () Nothing instRules)

shrinkDeriving :: HSE.Deriving () -> [HSE.Deriving ()]
shrinkDeriving (HSE.Deriving () strategy instRules) =
  [HSE.Deriving () strategy' instRules | strategy' <- shrinkDerivStrategy strategy]
    <> [HSE.Deriving () strategy instRules' | instRules' <- shrinkList shrinkInstRule instRules]

shrinkDerivStrategy :: Maybe (HSE.DerivStrategy ()) -> [Maybe (HSE.DerivStrategy ())]
shrinkDerivStrategy mStrategy =
  case mStrategy of
    Nothing -> []
    Just _ -> [Nothing]

shrinkInstRule :: HSE.InstRule () -> [HSE.InstRule ()]
shrinkInstRule instRule =
  case instRule of
    HSE.IRule () mTyVars mContext instHead ->
      [HSE.IRule () mTyVars' mContext instHead | mTyVars' <- shrinkTyVars mTyVars]
        <> [HSE.IRule () mTyVars mContext' instHead | mContext' <- shrinkMaybeContext mContext]
        <> [HSE.IRule () mTyVars mContext instHead' | instHead' <- shrinkInstHead instHead]
    HSE.IParen () rule0 -> HSE.IParen () <$> shrinkInstRule rule0

shrinkInstHead :: HSE.InstHead () -> [HSE.InstHead ()]
shrinkInstHead instHead =
  case instHead of
    HSE.IHCon () qname -> HSE.IHCon () <$> shrinkQNameNode qname
    HSE.IHParen () head0 -> head0 : [HSE.IHParen () head1 | head1 <- shrinkInstHead head0]
    HSE.IHApp () head0 ty ->
      [head0, HSE.IHParen () head0]
        <> [HSE.IHApp () head1 ty | head1 <- shrinkInstHead head0]
        <> [HSE.IHApp () head0 ty' | ty' <- shrinkType ty]
    HSE.IHInfix () lhs qname ->
      [HSE.IHCon () qname]
        <> [HSE.IHCon () qname' | qname' <- shrinkQNameNode qname]
        <> [HSE.IHInfix () lhs' qname | lhs' <- shrinkType lhs]

shrinkType :: HSE.Type () -> [HSE.Type ()]
shrinkType ty =
  case ty of
    HSE.TyVar () name -> HSE.TyVar () <$> shrinkVarNameNode name
    HSE.TyCon () qname -> HSE.TyCon () <$> shrinkQNameNode qname
    HSE.TyParen () ty0 -> ty0 : [HSE.TyParen () ty1 | ty1 <- shrinkType ty0]
    HSE.TyApp () lhs rhs ->
      [lhs, rhs]
        <> [HSE.TyApp () lhs' rhs | lhs' <- shrinkType lhs]
        <> [HSE.TyApp () lhs rhs' | rhs' <- shrinkType rhs]
    _ -> []

shrinkConNameNode :: HSE.Name () -> [HSE.Name ()]
shrinkConNameNode name =
  case name of
    HSE.Ident () txt -> [HSE.Ident () txt' | txt' <- shrinkConIdent txt]
    HSE.Symbol () txt -> [HSE.Symbol () txt' | txt' <- shrinkConIdent txt]

shrinkVarNameNode :: HSE.Name () -> [HSE.Name ()]
shrinkVarNameNode name =
  case name of
    HSE.Ident () txt -> [HSE.Ident () txt' | txt' <- shrinkVarIdent txt]
    HSE.Symbol () txt -> [HSE.Symbol () txt' | txt' <- shrinkVarIdent txt]

shrinkQNameNode :: HSE.QName () -> [HSE.QName ()]
shrinkQNameNode qname =
  case qname of
    HSE.UnQual () name -> HSE.UnQual () <$> shrinkConNameNode name
    HSE.Qual () modu name ->
      [HSE.UnQual () name]
        <> [HSE.Qual () modu' name | modu' <- shrinkModuleNameNode modu]
        <> [HSE.Qual () modu name' | name' <- shrinkConNameNode name]
    HSE.Special () _ -> []

genConNameNode :: Gen (HSE.Name ())
genConNameNode = HSE.Ident () <$> genConIdent

genVarNameNode :: Gen (HSE.Name ())
genVarNameNode = HSE.Ident () <$> genVarIdent

shrinkModuleNameNode :: HSE.ModuleName () -> [HSE.ModuleName ()]
shrinkModuleNameNode (HSE.ModuleName () moduName) = HSE.ModuleName () <$> shrinkModuleName moduName

genModuleName :: Gen String
genModuleName = do
  segmentCount <- chooseInt (1, 4)
  segments <- vectorOf segmentCount genModuleSegment
  pure (intercalate "." segments)

genModuleSegment :: Gen String
genModuleSegment = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 10)
  rest <- vectorOf restLen (elements moduleSegmentChars)
  pure (first : rest)

moduleSegmentChars :: [Char]
moduleSegmentChars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "'"

genMaybeExportSpecList :: Gen (Maybe (HSE.ExportSpecList ()))
genMaybeExportSpecList =
  frequency
    [ (3, pure Nothing),
      (7, Just <$> genExportSpecList)
    ]

genExportSpecList :: Gen (HSE.ExportSpecList ())
genExportSpecList = do
  n <- chooseInt (0, 4)
  specs <- vectorOf n genExportSpec
  pure (HSE.ExportSpecList () specs)

genExportSpec :: Gen (HSE.ExportSpec ())
genExportSpec =
  frequency
    [ (4, HSE.EVar () <$> genVarQName),
      (3, HSE.EAbs () (HSE.NoNamespace ()) <$> genTypeQName),
      (3, genEThingWith),
      (2, HSE.EModuleContents () <$> genModuleNameNode)
    ]

genEThingWith :: Gen (HSE.ExportSpec ())
genEThingWith = do
  qtycon <- genTypeQName
  n <- chooseInt (0, 3)
  cnames <- vectorOf n genCName
  pure (HSE.EThingWith () (HSE.NoWildcard ()) qtycon cnames)

genVarQName :: Gen (HSE.QName ())
genVarQName =
  frequency
    [ (4, HSE.UnQual () . HSEB.name <$> genVarIdent),
      ( 1,
        do
          modu <- genModuleNameNode
          HSE.Qual () modu . HSEB.name <$> genVarIdent
      )
    ]

genTypeQName :: Gen (HSE.QName ())
genTypeQName =
  frequency
    [ (4, HSE.UnQual () . HSEB.name <$> genConIdent),
      ( 1,
        do
          modu <- genModuleNameNode
          HSE.Qual () modu . HSEB.name <$> genConIdent
      )
    ]

genCName :: Gen (HSE.CName ())
genCName =
  frequency
    [ (1, HSE.VarName () . HSEB.name <$> genVarIdent),
      (2, HSE.ConName () . HSEB.name <$> genConIdent)
    ]

genModuleNameNode :: Gen (HSE.ModuleName ())
genModuleNameNode = HSE.ModuleName () <$> genModuleName

genVarIdent :: Gen String
genVarIdent = do
  first <- elements ['a' .. 'z']
  restLen <- chooseInt (0, 10)
  rest <- vectorOf restLen (elements moduleSegmentChars)
  let ident = first : rest
  if ident `elem` reservedWords
    then genVarIdent
    else pure ident

genConIdent :: Gen String
genConIdent = genModuleSegment

shrinkConIdent :: String -> [String]
shrinkConIdent ident =
  [ ident'
  | ident' <- shrink ident,
    case ident' of
      first : rest -> first `elem` ['A' .. 'Z'] && all isIdentChar rest
      [] -> False
  ]

shrinkVarIdent :: String -> [String]
shrinkVarIdent ident =
  [ ident'
  | ident' <- shrink ident,
    case ident' of
      first : rest ->
        first `elem` ['a' .. 'z']
          && all isIdentChar rest
          && ident' `notElem` reservedWords
      [] -> False
  ]

isIdentChar :: Char -> Bool
isIdentChar ch = ch `elem` moduleSegmentChars || ch == '_'

shrinkModuleName :: String -> [String]
shrinkModuleName name =
  let parts = splitModuleName name
      joinedShrinks =
        [ intercalate "." parts'
        | parts' <- shrinkModuleNameParts parts,
          isValidModuleName parts'
        ]
      rawShrinks =
        [ raw
        | raw <- shrink name,
          isValidModuleName (splitModuleName raw)
        ]
   in unique (joinedShrinks <> rawShrinks)

shrinkModuleNameParts :: [String] -> [[String]]
shrinkModuleNameParts parts =
  dropSegmentShrinks parts <> shrinkSegmentShrinks parts

dropSegmentShrinks :: [String] -> [[String]]
dropSegmentShrinks parts =
  [ before <> after
  | idx <- [0 .. length parts - 1],
    let (before, rest) = splitAt idx parts,
    (_ : after) <- [rest],
    not (null (before <> after))
  ]

shrinkSegmentShrinks :: [String] -> [[String]]
shrinkSegmentShrinks parts =
  [ replaceAt idx segment' parts
  | (idx, segment) <- zip [0 ..] parts,
    segment' <- shrink segment,
    isValidModuleSegment segment'
  ]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx value xs =
  let (before, rest) = splitAt idx xs
   in case rest of
        [] -> xs
        (_ : after) -> before <> (value : after)

splitModuleName :: String -> [String]
splitModuleName raw =
  case raw of
    "" -> []
    _ -> splitOnDot raw

splitOnDot :: String -> [String]
splitOnDot s =
  case break (== '.') s of
    (prefix, []) -> [prefix]
    (prefix, _ : rest) -> prefix : splitOnDot rest

isValidModuleName :: [String] -> Bool
isValidModuleName segments = not (null segments) && all isValidModuleSegment segments

isValidModuleSegment :: String -> Bool
isValidModuleSegment segment =
  case segment of
    first : rest -> first `elem` ['A' .. 'Z'] && all isIdentChar rest
    [] -> False

unique :: (Eq a) => [a] -> [a]
unique = foldr keep []
  where
    keep x acc
      | x `elem` acc = acc
      | otherwise = x : acc

reservedWords :: [String]
reservedWords =
  [ "case",
    "class",
    "forall",
    "family",
    "role",
    "pattern",
    "data",
    "default",
    "deriving",
    "do",
    "mdo",
    "proc",
    "rec",
    "group",
    "by",
    "using",
    "else",
    "foreign",
    "export",
    "safe",
    "unsafe",
    "stdcall",
    "ccall",
    "dynamic",
    "static",
    "interruptible",
    "stock",
    "anyclass",
    "via",
    "as",
    "hiding",
    "qualified",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]

qcGenStream :: Int -> [QCGen]
qcGenStream seed = map mkQCGen [seed ..]
