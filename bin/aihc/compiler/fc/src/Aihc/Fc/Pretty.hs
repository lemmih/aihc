{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable System FC text.
module Aihc.Fc.Pretty
  ( renderProgram,
    renderType,
    renderExpr,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..), packageIdText)
import Aihc.Tc.Types (Unique (..))
import Data.ByteString qualified as BS
import Data.Char (chr, isAscii, isPrint, ord)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Numeric (showHex)
import Prettyprinter (Doc, defaultLayoutOptions, hardline, hsep, indent, layoutPretty, parens, pretty, punctuate, space, vsep, (<+>))
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)

data Prec
  = PrecAtom
  | PrecApp
  | PrecFun
  | PrecEq
  | PrecForAll
  deriving (Eq, Ord)

renderProgram :: Program -> Text
renderProgram = renderStrict . layoutPretty defaultLayoutOptions . prettyProgram

prettyProgram :: Program -> Doc ann
prettyProgram program =
  vsep (punctuate hardline documents)
  where
    scopes = programScopes program
    scopeDocuments =
      case scopeEntries scopes of
        [] -> []
        entries -> [prettyScopes entries]
    importDocuments = prettyImports scopes (programImports program)
    documents = scopeDocuments <> importDocuments <> map (prettyDecl scopes) (programDecls program)

prettyImports :: ScopeTable -> Imports -> [Doc ann]
prettyImports scopes imports =
  prettyImportGroup "headers" headerEntries
    <> prettyImportGroup "synonyms" synonymEntries
    <> prettyImportGroup "axioms" axiomEntries
    <> prettyImportGroup "type-binders" typeBinderEntries
    <> prettyImportGroup "value-binders" valueBinderEntries
  where
    headerEntries =
      map (\(name, ty) -> prettyTopName scopes name <+> "::" <+> prettyTypeWith scopes PrecForAll ty) (Map.toAscList (importHeaders imports))
    synonymEntries =
      map (\(name, ty) -> prettyTopName scopes name <+> "=" <+> prettyTypeWith scopes PrecForAll ty) (Map.toAscList (importSynonyms imports))
    axiomEntries =
      map (\(name, axiom) -> prettyTopName scopes name <> prettyForAllBinders scopes (axiomBinders axiom) <+> ":" <+> prettyTypeWith scopes PrecEq (axiomLeft axiom) <+> prettyAxiomRole (axiomRole axiom) <+> prettyTypeWith scopes PrecEq (axiomRight axiom)) (Map.toAscList (importAxioms imports))
    typeBinderEntries = map prettyBinderEntry (filter ((== SortTypeVariable) . nameSort . fst) binderEntries)
    valueBinderEntries = map prettyBinderEntry (filter ((/= SortTypeVariable) . nameSort . fst) binderEntries)
    binderEntries = Map.toAscList (importBinders imports)
    prettyBinderEntry (name, ty) = prettyName scopes name <+> "::" <+> prettyTypeWith scopes PrecForAll ty

prettyImportGroup :: Doc ann -> [Doc ann] -> [Doc ann]
prettyImportGroup _ [] = []
prettyImportGroup group entries =
  ["import" <+> group <> hardline <> indent 2 (vsep (punctuate ";" entries))]

prettyScopes :: [(Int, PackageId, Text)] -> Doc ann
prettyScopes = vsep . map prettyScopeEntry

prettyScopeEntry :: (Int, PackageId, Text) -> Doc ann
prettyScopeEntry (scopeId, package, moduleName) =
  "scope" <+> pretty scopeId <+> "=" <+> pretty (show (T.unpack (packageIdText package))) <+> pretty moduleName

prettyDecl :: ScopeTable -> Decl -> Doc ann
prettyDecl scopes decl =
  case decl of
    DeclType declaration -> prettyTypeDecl scopes declaration
    DeclSynonym declaration -> prettySynonymDecl scopes declaration
    DeclAxiom declaration -> prettyAxiomDecl scopes declaration
    DeclVal declaration -> prettyValDecl scopes declaration
    DeclForeignImport declaration -> prettyForeignImportDecl scopes declaration

prettyVis :: Vis -> Doc ann
prettyVis Pub = "pub "
prettyVis Private = mempty

prettyTypeDecl :: ScopeTable -> TypeDecl -> Doc ann
prettyTypeDecl scopes declaration =
  prettyVis (typeVis declaration)
    <> "type "
    <> prettyTopName scopes (typeName declaration)
    <> prettyHeaderBinders scopes (typeBinders declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (typeResult declaration)
    <> prettyRoleList (typeRoles declaration)
    <> prettyConstructors scopes (typeCons declaration)

prettyHeaderBinders :: ScopeTable -> [Binder] -> Doc ann
prettyHeaderBinders scopes =
  foldMap ((space <>) . prettyPiBinder scopes)

prettyConstructors :: ScopeTable -> [ConDecl] -> Doc ann
prettyConstructors _ [] = " {}"
prettyConstructors scopes constructors =
  " {"
    <> hardline
    <> indent 4 (vsep (punctuate ";" (map (prettyConDecl scopes) constructors)))
    <> hardline
    <> "}"

prettyConDecl :: ScopeTable -> ConDecl -> Doc ann
prettyConDecl scopes declaration =
  prettyVis (conVis declaration)
    <> prettyTopName scopes (conName declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (conType declaration)

prettySynonymDecl :: ScopeTable -> SynonymDecl -> Doc ann
prettySynonymDecl scopes declaration =
  prettyVis (synVis declaration)
    <> "type "
    <> prettyTopName scopes (synName declaration)
    <> prettyHeaderBinders scopes (synBinders declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (synResult declaration)
    <> " ="
    <> hardline
    <> indent 1 (prettyTypeWith scopes PrecForAll (synBody declaration))

prettyAxiomDecl :: ScopeTable -> AxiomDecl -> Doc ann
prettyAxiomDecl scopes declaration =
  prettyVis (axiomVis declaration)
    <> "axiom "
    <> prettyTopName scopes (axiomName declaration)
    <> prettyForAllBinders scopes (axiomBinders declaration)
    <> " : "
    <> prettyTypeWith scopes PrecForAll (axiomLeft declaration)
    <+> prettyAxiomRole (axiomRole declaration)
    <+> prettyTypeWith scopes PrecForAll (axiomRight declaration)

prettyForAllBinders :: ScopeTable -> [Binder] -> Doc ann
prettyForAllBinders _ [] = mempty
prettyForAllBinders scopes binders =
  space <> hsep (map (prettyPiBinder scopes) binders)

prettyAxiomRole :: Role -> Doc ann
prettyAxiomRole Nominal = "~N"
prettyAxiomRole Representational = "~R"
prettyAxiomRole Phantom = "~P"

prettyRoleList :: [Role] -> Doc ann
prettyRoleList roles
  | all (== Representational) roles = mempty
  | otherwise = foldMap ((" @" <>) . prettyRoleTag) roles

prettyRoleTag :: Role -> Doc ann
prettyRoleTag Nominal = "N"
prettyRoleTag Representational = "R"
prettyRoleTag Phantom = "P"

prettyValDecl :: ScopeTable -> ValDecl -> Doc ann
prettyValDecl scopes declaration =
  prettyVis (valVis declaration)
    <> "val "
    <> prettyTopName scopes (valName declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (valType declaration)
    <> hardline
    <> " = "
    <> prettyExprWith scopes (valBody declaration)

prettyForeignImportDecl :: ScopeTable -> ForeignImportDecl -> Doc ann
prettyForeignImportDecl scopes declaration =
  prettyVis (foreignImportVis declaration)
    <> "foreign import "
    <> prettyCallingConvention (foreignImportCallingConvention declaration)
    <> prettyForeignImportDependencies scopes (foreignImportDependencies declaration)
    <> prettyTopName scopes (foreignImportName declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (foreignImportType declaration)

prettyForeignImportDependencies :: ScopeTable -> [ForeignImportDependency] -> Doc ann
prettyForeignImportDependencies _ [] = mempty
prettyForeignImportDependencies scopes dependencies =
  "using ["
    <> hsep (punctuate "," (map prettyDependency dependencies))
    <> "] "
  where
    prettyDependency dependency =
      case dependency of
        ForeignAxiom name -> "axiom" <+> prettyTopName scopes name
        ForeignConstructor name -> "constructor" <+> prettyTopName scopes name

prettyCallingConvention :: CallingConvention -> Doc ann
prettyCallingConvention convention =
  case convention of
    Prim -> "prim "
    CCall specification ->
      "ccall "
        <> prettyCCallTarget (ccallTarget specification)
        <> prettyForeignSafety (ccallSafety specification)
        <> " "
        <> pretty (show (T.unpack (ccallSymbol specification)))
        <> " ["
        <> hsep (punctuate "," (map prettyCAbiType (ccallArgumentTypes specification)))
        <> " → "
        <> prettyCAbiType (ccallResultType specification)
        <> "; "
        <> prettyForeignEffect (ccallEffect specification)
        <> "] "

prettyCCallTarget :: CCallTarget -> Doc ann
prettyCCallTarget target =
  case target of
    CCallFunction -> mempty
    CCallAddress -> "address "

prettyCAbiType :: CAbiType -> Doc ann
prettyCAbiType abiType =
  case abiType of
    CAbiInt -> "Int"
    CAbiInt8 -> "Int8"
    CAbiInt16 -> "Int16"
    CAbiInt32 -> "Int32"
    CAbiInt64 -> "Int64"
    CAbiWord -> "Word"
    CAbiWord8 -> "Word8"
    CAbiWord16 -> "Word16"
    CAbiWord32 -> "Word32"
    CAbiWord64 -> "Word64"
    CAbiFloat -> "Float"
    CAbiDouble -> "Double"
    CAbiAddr -> "Addr"
    CAbiVoid -> "Void"

prettyForeignSafety :: ForeignSafety -> Doc ann
prettyForeignSafety safety =
  case safety of
    ForeignUnsafe -> "unsafe"
    ForeignSafe -> "safe"

prettyForeignEffect :: ForeignEffect -> Doc ann
prettyForeignEffect effect =
  case effect of
    ForeignPure -> "pure"
    ForeignRealWorld -> "real-world"

renderType :: Program -> Type -> String
renderType program =
  renderDocument . prettyTypeWith (programScopes program) PrecForAll

prettyTypeWith :: ScopeTable -> Prec -> Type -> Doc ann
prettyTypeWith scopes prec ty =
  case ty of
    TyVar name -> prettyName scopes name
    TyCon name -> prettyName scopes name
    TyApp function argument ->
      parenthesize (prec < PrecApp) (prettyTypeWith scopes PrecApp function <+> prettyTypeWith scopes PrecAtom argument)
    TyFun r1 r2 argument result
      | Just scopeId <- liftedArrowScope scopes r1 r2 ->
          parenthesize
            (prec < PrecFun)
            (prettyTypeWith scopes PrecApp argument <+> (pretty scopeId <> ".→") <+> prettyTypeWith scopes PrecFun result)
      | otherwise ->
          parenthesize
            (prec < PrecFun)
            ( "FUN @"
                <> prettyTypeWith scopes PrecAtom r1
                <> " @"
                <> prettyTypeWith scopes PrecAtom r2
                <> space
                <> prettyTypeWith scopes PrecAtom argument
                <> space
                <> prettyTypeWith scopes PrecAtom result
            )
    TyForAll binder body ->
      parenthesize
        (prec < PrecForAll)
        ( "∀"
            <> prettyPiBinder scopes binder
            <> prettyForallTail scopes body
        )
    TyEq left right ->
      parenthesize (prec < PrecEq) (prettyTypeWith scopes PrecApp left <+> "~" <+> prettyTypeWith scopes PrecApp right)

liftedArrowScope :: ScopeTable -> Type -> Type -> Maybe Int
liftedArrowScope scopes left right =
  case (left, right) of
    (TyCon leftName, TyCon rightName)
      | leftName == rightName,
        nameText leftName == "LiftedRep",
        OriginTop package moduleName <- nameOrigin leftName ->
          lookupScopeId scopes package moduleName
    _ -> Nothing

prettyForallTail :: ScopeTable -> Type -> Doc ann
prettyForallTail scopes ty =
  case ty of
    TyForAll binder body ->
      space <> prettyPiBinder scopes binder <> prettyForallTail scopes body
    _ -> ". " <> prettyTypeWith scopes PrecForAll ty

prettyPiBinder :: ScopeTable -> Binder -> Doc ann
prettyPiBinder scopes binder =
  parens
    ( (pretty (nameText (binderName binder)) <> prettyUniqueSuffix (binderName binder))
        <> " : "
        <> prettyTypeWith scopes PrecForAll (binderType binder)
    )

renderExpr :: Program -> Expr -> String
renderExpr program =
  renderDocument . prettyExprWith (programScopes program)

prettyExprWith :: ScopeTable -> Expr -> Doc ann
prettyExprWith scopes expr =
  case expr of
    ExVar name -> prettyName scopes name
    ExLit literal -> prettyLiteral scopes literal
    ExApp function argument ->
      prettyApp scopes function <+> prettyExprAtom scopes argument
    ExTyApp function argument ->
      prettyApp scopes function <+> ("@" <> prettyTypeWith scopes PrecAtom argument)
    ExLam binder body ->
      "λ" <> prettyPiBinder scopes binder <> "." <> hardline <> indent 2 (prettyExprWith scopes body)
    ExTyLam binder body ->
      "Λ" <> prettyPiBinder scopes binder <> "." <> hardline <> indent 2 (prettyExprWith scopes body)
    ExLet bind body ->
      "let {"
        <> hardline
        <> indent 4 (prettyBind scopes bind)
        <> hardline
        <> "} in"
        <> hardline
        <> indent 4 (prettyExprWith scopes body)
    ExRec binds body ->
      "rec {"
        <> hardline
        <> prettyIndentedItems 4 (map (prettyBind scopes) binds)
        <> hardline
        <> "} in"
        <> hardline
        <> indent 4 (prettyExprWith scopes body)
    ExCase scrutinee binder resultType alts ->
      "case "
        <> prettyExprWith scopes scrutinee
        <> " as "
        <> prettyPiBinder scopes binder
        <> " return "
        <> parens (prettyTypeWith scopes PrecForAll resultType)
        <> " of {"
        <> hardline
        <> prettyIndentedItems 4 (map (prettyAlt scopes) alts)
        <> hardline
        <> "}"
    ExCast body coercion ->
      prettyExprAtom scopes body <+> "▷" <+> prettyCoercion scopes coercion

prettyApp :: ScopeTable -> Expr -> Doc ann
prettyApp scopes expr =
  case expr of
    ExApp {} -> prettyExprWith scopes expr
    ExTyApp {} -> prettyExprWith scopes expr
    _ -> prettyExprAtom scopes expr

prettyExprAtom :: ScopeTable -> Expr -> Doc ann
prettyExprAtom scopes expr =
  case expr of
    ExVar {} -> prettyExprWith scopes expr
    ExLit {} -> prettyExprWith scopes expr
    _ -> parens (prettyExprWith scopes expr)

prettyBind :: ScopeTable -> Bind -> Doc ann
prettyBind scopes bind =
  (pretty (nameText (binderName (bindBinder bind))) <> prettyUniqueSuffix (binderName (bindBinder bind)))
    <> " : "
    <> prettyTypeWith scopes PrecForAll (binderType (bindBinder bind))
    <> " ="
    <> hardline
    <> indent 4 (prettyExprWith scopes (bindRhs bind))

prettyAlt :: ScopeTable -> Alt -> Doc ann
prettyAlt scopes alternative =
  prettyAltHead scopes alternative
    <> " →"
    <> hardline
    <> indent 4 (prettyExprWith scopes (altRhs alternative))

prettyAltHead :: ScopeTable -> Alt -> Doc ann
prettyAltHead scopes alternative =
  case altCon alternative of
    AltDefault -> "_"
    AltLit literal -> prettyLiteral scopes literal <> prettyTypeBinders (altTypeBinders alternative) <> prettyTermBinders (altBinders alternative)
    AltData name -> prettyName scopes name <> prettyTypeBinders (altTypeBinders alternative) <> prettyTermBinders (altBinders alternative)
  where
    prettyTypeBinders binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> "@"
            <> prettyPiBinder scopes binder
            <> prettyTypeBinders rest
    prettyTermBinders binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> prettyPiBinder scopes binder
            <> prettyTermBinders rest

prettyIndentedItems :: Int -> [Doc ann] -> Doc ann
prettyIndentedItems _ [] = mempty
prettyIndentedItems amount documents = indent amount (vsep (punctuate ";" documents))

prettyCoercion :: ScopeTable -> Coercion -> Doc ann
prettyCoercion scopes coercion =
  case coercion of
    CoVar name -> prettyName scopes name
    CoRefl ty -> "refl " <> prettyTypeWith scopes PrecAtom ty
    CoSym inner -> "sym " <> parens (prettyCoercion scopes inner)
    CoTrans left right -> "trans " <> parens (prettyCoercion scopes left) <+> parens (prettyCoercion scopes right)
    CoTyConApp name arguments ->
      hsep ("tycon-co" : prettyName scopes name : map (parens . prettyCoercion scopes) arguments)
    CoAxiom name arguments ->
      hsep ("axiom-co" : prettyName scopes name : map (("@" <>) . prettyTypeWith scopes PrecAtom) arguments)

prettyLiteral :: ScopeTable -> Literal -> Doc ann
prettyLiteral scopes literal =
  case literal of
    LitInt representation value -> pretty value <> "#" <> prettyName scopes (repName representation)
    LitChar representation value -> "'" <> pretty (encodeCharLiteral value) <> "'#" <> prettyName scopes (repName representation)
    LitAddr representation value -> "\"" <> pretty (concatMap encodeByte (BS.unpack value)) <> "\"#" <> prettyName scopes (repName representation)

encodeCharLiteral :: Char -> String
encodeCharLiteral character
  | character == '\'' = "\\'"
  | character == '\\' = "\\\\"
  | character == '\n' = "\\n"
  | isPrint character = [character]
  | otherwise = "\\x{" <> showHex (ord character) "" <> "}"

encodeByte :: Word8 -> String
encodeByte byte
  | isAscii character && isPrint character && character `notElem` ("\"\\'" :: String) =
      [character]
  | otherwise = "\\x" <> padHex (showHex (fromIntegral byte :: Int) "")
  where
    character = chr (fromIntegral byte)
    padHex [digit] = ['0', digit]
    padHex digits = digits

repName :: Type -> Name
repName ty =
  case ty of
    TyCon name -> name
    TyVar name -> name
    _ -> Name "AddrRep" SortDataConstructor (OriginLocal (Unique 0))

prettyName :: ScopeTable -> Name -> Doc ann
prettyName scopes name =
  case nameOrigin name of
    OriginLocal {} -> pretty (nameText name) <> prettyUniqueSuffix name
    OriginTop {} -> prettyTopName scopes name

prettyTopName :: ScopeTable -> Name -> Doc ann
prettyTopName scopes name =
  case nameOrigin name of
    OriginTop package moduleName ->
      prettyScopePrefix scopes package moduleName <> prettyPrintedName name
    OriginLocal {} ->
      prettyPrintedName name

prettyScopePrefix :: ScopeTable -> PackageId -> Text -> Doc ann
prettyScopePrefix scopes package moduleName =
  case lookupScopeId scopes package moduleName of
    Just scopeId -> pretty scopeId <> "."
    Nothing -> error ("missing System FC scope for " <> show (packageIdText package, moduleName))

lookupScopeId :: ScopeTable -> PackageId -> Text -> Maybe Int
lookupScopeId table package moduleName =
  lookup (package, moduleName) [((entryPackage, entryModule), scopeId) | (scopeId, entryPackage, entryModule) <- scopeEntries table]

prettyPrintedName :: Name -> Doc ann
prettyPrintedName name =
  case nameClass (nameSort name) of
    NameClassType -> "t" <> prettyRawPrinted (nameText name)
    NameClassValue -> "v" <> prettyRawPrinted (nameText name)
    NameClassAxiom -> pretty (nameText name)
    NameClassTypeVar -> pretty (nameText name)

prettyRawPrinted :: Text -> Doc ann
prettyRawPrinted = pretty

prettyUniqueSuffix :: Name -> Doc ann
prettyUniqueSuffix name =
  case nameOrigin name of
    OriginLocal (Unique unique)
      | unique /= 0 -> "{" <> pretty unique <> "}"
      | otherwise -> mempty
    OriginTop {} -> mempty

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize False value = value
parenthesize True value = parens value

renderDocument :: Doc ann -> String
renderDocument = renderString . layoutPretty defaultLayoutOptions
