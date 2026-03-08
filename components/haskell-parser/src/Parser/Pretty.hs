{-# LANGUAGE OverloadedStrings #-}

module Parser.Pretty
  ( prettyExpr,
    prettyModule,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    braces,
    brackets,
    comma,
    defaultLayoutOptions,
    hsep,
    layoutPretty,
    parens,
    punctuate,
    semi,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text (renderStrict)

prettyExpr :: Expr -> Text
prettyExpr = renderDoc . prettyExprPrec 0

prettyModule :: Module -> Text
prettyModule modu =
  renderDoc (vsep (headerLines <> importLines <> declLines))
  where
    headerLines =
      case moduleName modu of
        Just name ->
          [ hsep
              ( ["module", pretty name]
                  <> maybe [] (\specs -> [prettyExportSpecList specs]) (moduleExports modu)
                  <> ["where"]
              )
          ]
        Nothing -> []
    importLines = map prettyImportDecl (moduleImports modu)
    declLines = concatMap prettyDeclLines (moduleDecls modu)

prettyExportSpecList :: [ExportSpec] -> Doc ann
prettyExportSpecList specs =
  parens (hsep (punctuate comma (map prettyExportSpec specs)))

prettyExportSpec :: ExportSpec -> Doc ann
prettyExportSpec spec =
  case spec of
    ExportModule _ modName -> "module" <+> pretty modName
    ExportVar _ name -> prettyBinderName name
    ExportAbs _ name -> pretty name
    ExportAll _ name -> pretty name <> "(..)"
    ExportWith _ name members ->
      pretty name <> parens (hsep (punctuate comma (map prettyBinderName members)))

prettyImportDecl :: ImportDecl -> Doc ann
prettyImportDecl decl =
  hsep
    ( ["import"]
        <> ["qualified" | importDeclQualified decl]
        <> [pretty (importDeclModule decl)]
        <> maybe [] (\alias -> ["as", pretty alias]) (importDeclAs decl)
        <> maybe [] (\spec -> [prettyImportSpec spec]) (importDeclSpec decl)
    )

prettyImportSpec :: ImportSpec -> Doc ann
prettyImportSpec spec =
  hsep
    ( ["hiding" | importSpecHiding spec]
        <> [parens (hsep (punctuate comma (map prettyImportItem (importSpecItems spec))))]
    )

prettyImportItem :: ImportItem -> Doc ann
prettyImportItem item =
  case item of
    ImportItemVar _ name -> prettyBinderName name
    ImportItemAbs _ name -> pretty name
    ImportItemAll _ name -> pretty name <> "(..)"
    ImportItemWith _ name members ->
      pretty name <> parens (hsep (punctuate comma (map prettyBinderName members)))

prettyDeclLines :: Decl -> [Doc ann]
prettyDeclLines decl =
  case decl of
    DeclValue _ valueDecl -> prettyValueDeclLines valueDecl
    DeclTypeSig _ names ty -> [hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]]
    DeclFixity _ assoc prec ops ->
      [ hsep
          ( [prettyFixityAssoc assoc]
              <> maybe [] (pure . pretty . show) prec
              <> map prettyOperatorName ops
          )
      ]
    DeclTypeSyn _ synDecl ->
      [ hsep
          [ "type",
            pretty (typeSynName synDecl),
            hsep (map pretty (typeSynParams synDecl)),
            "=",
            prettyType (typeSynBody synDecl)
          ]
      ]
    DeclData _ dataDecl -> [prettyDataDecl dataDecl]
    DeclNewtype _ newtypeDecl -> [prettyNewtypeDecl newtypeDecl]
    DeclClass _ classDecl -> [prettyClassDecl classDecl]
    DeclInstance _ instanceDecl -> [prettyInstanceDecl instanceDecl]
    DeclDefault _ tys -> ["default" <+> parens (hsep (punctuate comma (map prettyType tys)))]
    DeclForeign _ foreignDecl -> [prettyForeignDecl foreignDecl]

prettyValueDeclLines :: ValueDecl -> [Doc ann]
prettyValueDeclLines valueDecl =
  case valueDecl of
    PatternBind _ pat rhs -> [prettyPattern pat <+> prettyRhs rhs]
    FunctionBind _ name matches ->
      concatMap (prettyFunctionMatchLines name) matches

prettyFunctionMatchLines :: Text -> Match -> [Doc ann]
prettyFunctionMatchLines name match =
  case matchRhs match of
    UnguardedRhs _ _ -> [prettyFunctionMatch name match]
    GuardedRhss _ grhss ->
      prettyFunctionHead name (matchPats match)
        : [ "  |"
              <+> hsep (punctuate comma (map (prettyExprPrec 0) (guardedRhsGuards grhs)))
              <+> "="
              <+> prettyExprPrec 0 (guardedRhsBody grhs)
          | grhs <- grhss
          ]

prettyFunctionMatch :: Text -> Match -> Doc ann
prettyFunctionMatch name match =
  prettyFunctionHead name (matchPats match) <+> prettyRhs (matchRhs match)

prettyFunctionHead :: Text -> [Pattern] -> Doc ann
prettyFunctionHead name pats =
  case pats of
    [lhs, rhsPat]
      | isOperatorToken name ->
          prettyPattern lhs <+> pretty name <+> prettyPattern rhsPat
    _ ->
      hsep (prettyFunctionBinder name : map prettyPattern pats)

prettyRhs :: Rhs -> Doc ann
prettyRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> "=" <+> prettyExprPrec 0 expr
    GuardedRhss _ guards ->
      hsep
        ( punctuate
            semi
            [ "|"
                <+> hsep (punctuate comma (map (prettyExprPrec 0) (guardedRhsGuards grhs)))
                <+> "="
                <+> prettyExprPrec 0 (guardedRhsBody grhs)
            | grhs <- guards
            ]
        )

prettyType :: Type -> Doc ann
prettyType ty =
  case ty of
    TVar _ name -> pretty name
    TCon _ name -> pretty name
    TApp _ f x -> parenthesizeTypeApp f <+> parenthesizeTypeArg x
    TFun _ a b -> parenthesizeTypeFunLeft a <+> "->" <+> prettyType b
    TTuple _ elems -> parens (hsep (punctuate comma (map prettyType elems)))
    TList _ inner -> brackets (prettyType inner)
    TParen _ inner -> parens (prettyType inner)
    TContext _ constraints inner ->
      prettyContext constraints <+> "=>" <+> prettyType inner

parenthesizeTypeFunLeft :: Type -> Doc ann
parenthesizeTypeFunLeft ty =
  case ty of
    TFun {} -> parens (prettyType ty)
    TContext {} -> parens (prettyType ty)
    _ -> prettyType ty

parenthesizeTypeApp :: Type -> Doc ann
parenthesizeTypeApp ty =
  case ty of
    TFun {} -> parens (prettyType ty)
    TContext {} -> parens (prettyType ty)
    _ -> prettyType ty

parenthesizeTypeArg :: Type -> Doc ann
parenthesizeTypeArg ty =
  case ty of
    TApp {} -> parens (prettyType ty)
    TFun {} -> parens (prettyType ty)
    TContext {} -> parens (prettyType ty)
    _ -> prettyType ty

prettyContext :: [Constraint] -> Doc ann
prettyContext constraints =
  case constraints of
    [single] -> prettyConstraint single
    _ -> parens (hsep (punctuate comma (map prettyConstraint constraints)))

prettyConstraint :: Constraint -> Doc ann
prettyConstraint constraint =
  let base =
        if constraintClass constraint == "()" && null (constraintArgs constraint)
          then "()"
          else hsep (pretty (constraintClass constraint) : map prettyTypeAtom (constraintArgs constraint))
   in if constraintParen constraint
        then parens base
        else base

prettyTypeAtom :: Type -> Doc ann
prettyTypeAtom ty =
  case ty of
    TVar _ _ -> prettyType ty
    TCon _ _ -> prettyType ty
    TList _ _ -> prettyType ty
    TTuple _ _ -> prettyType ty
    TParen _ _ -> prettyType ty
    _ -> parens (prettyType ty)

prettyPattern :: Pattern -> Doc ann
prettyPattern pat =
  case pat of
    PVar _ name -> pretty name
    PWildcard _ -> "_"
    PLit _ lit -> prettyLiteral lit
    PTuple _ elems -> parens (hsep (punctuate comma (map prettyPattern elems)))
    PList _ elems -> brackets (hsep (punctuate comma (map prettyPattern elems)))
    PCon _ con args -> hsep (pretty con : map prettyPatternAtom args)
    PInfix _ lhs op rhs -> prettyPatternAtom lhs <+> pretty op <+> prettyPatternAtom rhs
    PAs _ name inner -> pretty name <> "@" <> prettyPatternAtom inner
    PIrrefutable _ inner -> "~" <> prettyPatternAtom inner
    PNegLit _ lit -> "-" <> prettyLiteral lit
    PParen _ inner -> parens (prettyPattern inner)
    PRecord _ con fields ->
      pretty con
        <+> braces
          ( hsep
              ( punctuate
                  comma
                  [ pretty fieldName <+> "=" <+> prettyPattern fieldPat
                  | (fieldName, fieldPat) <- fields
                  ]
              )
          )

prettyPatternAtom :: Pattern -> Doc ann
prettyPatternAtom pat =
  case pat of
    PVar _ _ -> prettyPattern pat
    PWildcard _ -> prettyPattern pat
    PLit _ _ -> prettyPattern pat
    PNegLit _ _ -> prettyPattern pat
    PList _ _ -> prettyPattern pat
    PTuple _ _ -> prettyPattern pat
    PParen _ _ -> prettyPattern pat
    _ -> parens (prettyPattern pat)

prettyLiteral :: Literal -> Doc ann
prettyLiteral lit =
  case lit of
    LitInt _ n -> pretty (show n)
    LitIntBase _ _ repr -> pretty repr
    LitFloat _ n -> pretty (show n)
    LitChar _ c -> pretty (show c)
    LitString _ s -> pretty (show (T.unpack s))

prettyDataDecl :: DataDecl -> Doc ann
prettyDataDecl decl =
  hsep
    ( [ "data",
        prettyDeclHead (dataDeclContext decl) (dataDeclName decl) (dataDeclParams decl)
      ]
        <> ctorPart
        <> derivingPart (dataDeclDeriving decl)
    )
  where
    ctorPart =
      case dataDeclConstructors decl of
        [] -> []
        ctors -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

prettyNewtypeDecl :: NewtypeDecl -> Doc ann
prettyNewtypeDecl decl =
  hsep
    ( [ "newtype",
        prettyDeclHead (newtypeDeclContext decl) (newtypeDeclName decl) (newtypeDeclParams decl)
      ]
        <> ctorPart
        <> derivingPart (newtypeDeclDeriving decl)
    )
  where
    ctorPart =
      case newtypeDeclConstructor decl of
        Nothing -> []
        Just ctor -> ["=", prettyDataCon ctor]

derivingPart :: Maybe DerivingClause -> [Doc ann]
derivingPart mClause =
  case mClause of
    Nothing -> []
    Just (DerivingClause classes) ->
      case classes of
        [] -> ["deriving", "()"]
        [single] -> ["deriving", pretty single]
        _ -> ["deriving", parens (hsep (punctuate comma (map pretty classes)))]

prettyDeclHead :: [Constraint] -> Text -> [Text] -> Doc ann
prettyDeclHead constraints name params =
  hsep
    ( contextPrefix constraints
        <> [pretty name]
        <> map pretty params
    )

contextPrefix :: [Constraint] -> [Doc ann]
contextPrefix constraints =
  case constraints of
    [] -> []
    _ -> [prettyContext constraints, "=>"]

prettyDataCon :: DataConDecl -> Doc ann
prettyDataCon ctor =
  case ctor of
    PrefixCon _ name fields -> hsep (prettyConstructorName name : map prettyBangType fields)
    InfixCon _ lhs op rhs -> prettyBangTypeAtom lhs <+> pretty op <+> prettyBangTypeAtom rhs
    RecordCon _ name fields ->
      prettyConstructorName name
        <+> braces
          ( hsep
              ( punctuate
                  comma
                  [ hsep
                      [ hsep (punctuate comma (map pretty (fieldNames fld))),
                        "::",
                        prettyBangType (fieldType fld)
                      ]
                  | fld <- fields
                  ]
              )
          )

prettyBangType :: BangType -> Doc ann
prettyBangType bt
  | bangStrict bt = "!" <> prettyTypeAtom (bangType bt)
  | otherwise = prettyTypeAtom (bangType bt)

prettyBangTypeAtom :: BangType -> Doc ann
prettyBangTypeAtom bt =
  case bangType bt of
    TFun {} -> parens (prettyBangType bt)
    TContext {} -> parens (prettyBangType bt)
    _ -> prettyBangType bt

prettyClassDecl :: ClassDecl -> Doc ann
prettyClassDecl decl =
  let headDoc =
        hsep
          ( ["class"]
              <> contextPrefix (classDeclContext decl)
              <> [pretty (classDeclName decl), pretty (classDeclParam decl)]
          )
   in case classDeclItems decl of
        [] -> headDoc
        items -> headDoc <+> "where" <+> braces (hsep (punctuate semi (map prettyClassItem items)))

prettyClassItem :: ClassDeclItem -> Doc ann
prettyClassItem item =
  case item of
    ClassItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    ClassItemFixity _ assoc prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> map prettyOperatorName ops
        )
    ClassItemDefault _ valueDecl ->
      case prettyValueDeclLines valueDecl of
        [] -> ""
        (line : _) -> line

prettyInstanceDecl :: InstanceDecl -> Doc ann
prettyInstanceDecl decl =
  let headDoc =
        hsep
          ( ["instance"]
              <> contextPrefix (instanceDeclContext decl)
              <> [pretty (instanceDeclClassName decl)]
              <> map prettyTypeAtom (instanceDeclTypes decl)
          )
   in case instanceDeclItems decl of
        [] -> headDoc
        items -> headDoc <+> "where" <+> braces (hsep (punctuate semi (map prettyInstanceItem items)))

prettyInstanceItem :: InstanceDeclItem -> Doc ann
prettyInstanceItem item =
  case item of
    InstanceItemBind _ valueDecl ->
      case prettyValueDeclLines valueDecl of
        [] -> ""
        (line : _) -> line
    InstanceItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    InstanceItemFixity _ assoc prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> map prettyOperatorName ops
        )

prettyFixityAssoc :: FixityAssoc -> Doc ann
prettyFixityAssoc assoc =
  case assoc of
    Infix -> "infix"
    InfixL -> "infixl"
    InfixR -> "infixr"

prettyForeignDecl :: ForeignDecl -> Doc ann
prettyForeignDecl decl =
  hsep . catMaybes $
    [ Just "foreign",
      Just (prettyDirection (foreignDirection decl)),
      Just (prettyCallConv (foreignCallConv decl)),
      prettySafety <$> foreignSafety decl,
      prettyForeignEntity (foreignEntity decl),
      Just (pretty (foreignName decl)),
      Just "::",
      Just (prettyType (foreignType decl))
    ]

prettyDirection :: ForeignDirection -> Doc ann
prettyDirection direction =
  case direction of
    ForeignImport -> "import"
    ForeignExport -> "export"

prettyCallConv :: CallConv -> Doc ann
prettyCallConv cc =
  case cc of
    CCall -> "ccall"
    StdCall -> "stdcall"

prettySafety :: ForeignSafety -> Doc ann
prettySafety safety =
  case safety of
    Safe -> "safe"
    Unsafe -> "unsafe"

prettyForeignEntity :: ForeignEntitySpec -> Maybe (Doc ann)
prettyForeignEntity spec =
  case spec of
    ForeignEntityOmitted -> Nothing
    ForeignEntityDynamic -> Just (quoted "dynamic")
    ForeignEntityWrapper -> Just (quoted "wrapper")
    ForeignEntityStatic Nothing -> Just (quoted "static")
    ForeignEntityStatic (Just name) -> Just (quoted ("static " <> name))
    ForeignEntityAddress Nothing -> Just (quoted "&")
    ForeignEntityAddress (Just name) -> Just (quoted ("&" <> name))
    ForeignEntityNamed name -> Just (quoted name)

prettyOperatorName :: Text -> Doc ann
prettyOperatorName name
  | isOperatorToken name = pretty name
  | otherwise = parens (pretty name)

prettyFunctionBinder :: Text -> Doc ann
prettyFunctionBinder name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyBinderName :: Text -> Doc ann
prettyBinderName = prettyFunctionBinder

prettyExprOperator :: Text -> Doc ann
prettyExprOperator op
  | isOperatorToken op = pretty op
  | otherwise = "`" <> pretty op <> "`"

prettyConstructorName :: Text -> Doc ann
prettyConstructorName name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyExprPrec :: Int -> Expr -> Doc ann
prettyExprPrec prec expr =
  case expr of
    EApp _ fn arg ->
      parenthesize (prec > 2) (prettyExprPrec 2 fn <+> prettyExprPrec 3 arg)
    ETypeApp _ fn ty ->
      parenthesize (prec > 2) (prettyExprPrec 2 fn <+> "@" <> prettyTypeAtom ty)
    EVar _ name
      | isOperatorToken name -> parens (pretty name)
      | otherwise -> pretty name
    EInt _ value -> pretty (show value)
    EIntBase _ _ repr -> pretty repr
    EFloat _ value -> pretty (show value)
    EChar _ value -> pretty (show value)
    EString _ value -> pretty (show value)
    EIf _ cond yes no ->
      parenthesize
        (prec > 0)
        ("if" <+> prettyExprPrec 0 cond <+> "then" <+> prettyExprPrec 0 yes <+> "else" <+> prettyExprPrec 0 no)
    ELambdaPats _ pats body ->
      parenthesize (prec > 0) ("\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyExprPrec 0 body)
    EInfix _ lhs op rhs -> parenthesize (prec > 1) (prettyExprPrec 1 lhs <+> prettyExprOperator op <+> prettyExprPrec 1 rhs)
    ENegate _ inner -> parenthesize (prec > 2) ("-" <> prettyExprPrec 3 inner)
    ESectionL _ lhs op -> parens (prettyExprPrec 0 lhs <+> prettyExprOperator op)
    ESectionR _ op rhs -> parens (prettyExprOperator op <+> prettyExprPrec 0 rhs)
    ELetDecls _ decls body ->
      parenthesize
        (prec > 0)
        ( "let"
            <+> braces (prettyInlineDecls decls)
            <+> "in"
            <+> prettyExprPrec 0 body
        )
    ECase _ scrutinee alts ->
      parenthesize
        (prec > 0)
        ( "case"
            <+> prettyExprPrec 0 scrutinee
            <+> "of"
            <+> hsep (punctuate semi (map prettyCaseAlt alts))
        )
    EDo _ stmts ->
      parenthesize
        (prec > 0)
        ("do" <+> braces (hsep (punctuate semi (map prettyDoStmt stmts))))
    EListComp _ body quals ->
      brackets
        ( prettyExprPrec 0 body
            <+> "|"
            <+> hsep (punctuate comma (map prettyCompStmt quals))
        )
    EListCompParallel _ body qualifierGroups ->
      brackets
        ( prettyExprPrec 0 body
            <+> "|"
            <+> hsep
              ( punctuate
                  "|"
                  (map (hsep . punctuate comma . map prettyCompStmt) qualifierGroups)
              )
        )
    EArithSeq _ seqInfo -> prettyArithSeq seqInfo
    ERecordCon _ name fields ->
      pretty name <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ERecordUpd _ base fields ->
      prettyExprPrec 3 base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ETypeSig _ inner ty -> parenthesize (prec > 1) (prettyExprPrec 1 inner <+> "::" <+> prettyType ty)
    EParen _ inner -> parens (prettyExprPrec 0 inner)
    EWhereDecls _ body decls ->
      parenthesize
        (prec > 0)
        (prettyExprPrec 0 body <+> "where" <+> braces (prettyInlineDecls decls))
    EList _ values -> brackets (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETuple _ values -> parens (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETupleCon _ arity -> parens (pretty (T.replicate (max 1 (arity - 1)) ","))

prettyBinding :: (Text, Expr) -> Doc ann
prettyBinding (name, value) = pretty name <+> "=" <+> prettyExprPrec 0 value

prettyCaseAlt :: CaseAlt -> Doc ann
prettyCaseAlt (CaseAlt _ pat rhs) =
  case rhs of
    UnguardedRhs _ expr -> prettyPattern pat <+> "->" <+> prettyExprPrec 0 expr
    GuardedRhss _ grhss ->
      hsep
        [ prettyPattern pat,
          hsep
            ( punctuate
                semi
                [ "|"
                    <+> hsep (punctuate comma (map (prettyExprPrec 0) (guardedRhsGuards grhs)))
                    <+> "->"
                    <+> prettyExprPrec 0 (guardedRhsBody grhs)
                | grhs <- grhss
                ]
            )
        ]

prettyDoStmt :: DoStmt -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    DoLet _ bindings -> "let" <+> braces (hsep (punctuate semi (map prettyBinding bindings)))
    DoLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)
    DoExpr _ expr -> prettyExprPrec 0 expr

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    CompGuard _ expr -> prettyExprPrec 0 expr
    CompLet _ bindings -> "let" <+> hsep (punctuate semi (map prettyBinding bindings))
    CompLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)

prettyInlineDecls :: [Decl] -> Doc ann
prettyInlineDecls decls =
  hsep (punctuate semi (concatMap prettyDeclLines decls))

prettyArithSeq :: ArithSeq -> Doc ann
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom _ fromExpr -> brackets (prettyExprPrec 0 fromExpr <> " ..")
    ArithSeqFromThen _ fromExpr thenExpr -> brackets (prettyExprPrec 0 fromExpr <> ", " <> prettyExprPrec 0 thenExpr <> " ..")
    ArithSeqFromTo _ fromExpr toExpr -> brackets (prettyExprPrec 0 fromExpr <> " .. " <> prettyExprPrec 0 toExpr)
    ArithSeqFromThenTo _ fromExpr thenExpr toExpr ->
      brackets (prettyExprPrec 0 fromExpr <> ", " <> prettyExprPrec 0 thenExpr <> " .. " <> prettyExprPrec 0 toExpr)

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize shouldWrap doc
  | shouldWrap = parens doc
  | otherwise = doc

quoted :: Text -> Doc ann
quoted txt = pretty (show (T.unpack txt))

isOperatorToken :: Text -> Bool
isOperatorToken tok =
  not (T.null tok) && T.all (`elem` (":!#$%&*+./<=>?\\^|-~" :: String)) tok

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions
