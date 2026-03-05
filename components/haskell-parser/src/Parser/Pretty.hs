{-# LANGUAGE OverloadedStrings #-}

module Parser.Pretty
  ( prettyExpr,
    prettyModule,
  )
where

import Data.Maybe (catMaybes, fromMaybe)
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
  renderDoc (vsep (headerLines <> map prettyDecl (moduleDecls modu)))
  where
    headerLines =
      case moduleName modu of
        Just name -> ["module" <+> pretty name <+> "where"]
        Nothing -> []

prettyDecl :: Decl -> Doc ann
prettyDecl decl =
  case decl of
    Decl {declName = name, declExpr = expr} -> pretty name <+> "=" <+> prettyExprPrec 0 expr
    PatternDecl {patternLhs = lhs} -> pretty lhs <+> "=" <+> "undefined"
    TypeSigDecl {typeSigName = name} -> pretty name <+> "::" <+> "T"
    FunctionDecl {functionName = name} -> functionBinder name <+> "_x" <+> "=" <+> "undefined"
    TypeDecl {typeName = name} -> "type" <+> pretty name <+> "=" <+> pretty name
    DataDecl {dataTypeName = dataType, dataConstructors = ctors} ->
      if null ctors
        then "data" <+> pretty dataType
        else "data" <+> pretty dataType <+> "=" <+> hsep (punctuate "|" (map prettyConstructor ctors))
    NewtypeDecl {newtypeName = name, newtypeConstructor = ctor} ->
      "newtype" <+> pretty name <+> "=" <+> pretty (fromMaybe ("Mk" <> name) ctor)
    ClassDecl {className = name} -> "class" <+> pretty name <+> "where"
    InstanceDecl {instanceClassName = name} -> "instance" <+> pretty name <+> "where"
    FixityDecl {fixityAssoc = assoc, fixityPrecedence = prec, fixityOperator = op} ->
      hsep (pretty assoc : maybe [] (pure . pretty . show) prec <> [pretty op])
    DefaultDecl {defaultTypes = tys} -> "default" <+> parens (hsep (punctuate comma (map pretty tys)))
    ForeignDecl
      { foreignDirection = direction,
        foreignCallConv = callConv,
        foreignSafety = safety,
        foreignEntity = entity,
        foreignName = name
      } ->
        hsep . catMaybes $
          [ Just "foreign",
            Just (directionText direction),
            Just (callConvText callConv),
            safetyText <$> safety,
            quoted <$> entity,
            Just (pretty name),
            Just "::",
            Just "Int"
          ]
    StructuredDecl {structuredTokens = toks} -> pretty (renderDeclTokens toks)

functionBinder :: Text -> Doc ann
functionBinder name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyConstructor :: Text -> Doc ann
prettyConstructor name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyExprPrec :: Int -> Expr -> Doc ann
prettyExprPrec prec expr =
  case expr of
    EApp fn arg ->
      parenthesize (prec > 2) (prettyExprPrec 2 fn <+> prettyExprPrec 3 arg)
    EVar name -> pretty name
    EInt value -> pretty (show value)
    EFloat value -> pretty (show value)
    EChar value -> pretty (show value)
    EString value -> pretty (show value)
    EIf cond yes no ->
      parenthesize
        (prec > 0)
        ("if" <+> prettyExprPrec 0 cond <+> "then" <+> prettyExprPrec 0 yes <+> "else" <+> prettyExprPrec 0 no)
    ELambda params body ->
      parenthesize (prec > 0) ("\\" <> hsep (map pretty params) <+> "->" <+> prettyExprPrec 0 body)
    EInfix lhs op rhs -> parenthesize (prec > 1) (prettyExprPrec 1 lhs <+> pretty op <+> prettyExprPrec 1 rhs)
    ENegate inner -> parenthesize (prec > 2) ("-" <> prettyExprPrec 3 inner)
    ESectionL lhs op -> parens (prettyExprPrec 0 lhs <+> pretty op)
    ESectionR op rhs -> parens (pretty op <+> prettyExprPrec 0 rhs)
    ELet bindings body ->
      parenthesize
        (prec > 0)
        ( "let"
            <+> hsep (punctuate semi (map prettyBinding bindings))
            <+> "in"
            <+> prettyExprPrec 0 body
        )
    ECase scrutinee alts ->
      parenthesize
        (prec > 0)
        ( "case"
            <+> prettyExprPrec 0 scrutinee
            <+> "of"
            <+> hsep (punctuate semi (map prettyCaseAlt alts))
        )
    EDo stmts ->
      parenthesize
        (prec > 0)
        ("do" <+> braces (hsep (punctuate semi (map prettyDoStmt stmts))))
    EListComp body quals ->
      brackets
        ( prettyExprPrec 0 body
            <+> "|"
            <+> hsep (punctuate comma (map prettyCompStmt quals))
        )
    EArithSeq seqInfo -> prettyArithSeq seqInfo
    ERecordCon name fields ->
      pretty name <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ERecordUpd base fields ->
      prettyExprPrec 3 base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ETypeSig inner sigText -> parenthesize (prec > 1) (prettyExprPrec 1 inner <+> "::" <+> pretty sigText)
    EList values -> brackets (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETuple values -> parens (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETupleCon arity -> parens (pretty (T.replicate (max 1 (arity - 1)) ","))

prettyBinding :: (Text, Expr) -> Doc ann
prettyBinding (name, value) = pretty name <+> "=" <+> prettyExprPrec 0 value

prettyCaseAlt :: CaseAlt -> Doc ann
prettyCaseAlt (CaseAlt pat altExpr) = pretty pat <+> "->" <+> prettyExprPrec 0 altExpr

prettyDoStmt :: DoStmt -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoBind pat expr -> pretty pat <+> "<-" <+> prettyExprPrec 0 expr
    DoLet bindings -> "let" <+> hsep (punctuate semi (map prettyBinding bindings))
    DoExpr expr -> prettyExprPrec 0 expr

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompGen pat expr -> pretty pat <+> "<-" <+> prettyExprPrec 0 expr
    CompGuard expr -> prettyExprPrec 0 expr
    CompLet bindings -> "let" <+> hsep (punctuate semi (map prettyBinding bindings))

prettyArithSeq :: ArithSeq -> Doc ann
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom fromExpr -> brackets (prettyExprPrec 0 fromExpr <> "..")
    ArithSeqFromThen fromExpr thenExpr -> brackets (prettyExprPrec 0 fromExpr <> ", " <> prettyExprPrec 0 thenExpr <> "..")
    ArithSeqFromTo fromExpr toExpr -> brackets (prettyExprPrec 0 fromExpr <> ".." <> prettyExprPrec 0 toExpr)
    ArithSeqFromThenTo fromExpr thenExpr toExpr ->
      brackets (prettyExprPrec 0 fromExpr <> ", " <> prettyExprPrec 0 thenExpr <> ".." <> prettyExprPrec 0 toExpr)

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize shouldWrap doc
  | shouldWrap = parens doc
  | otherwise = doc

quoted :: Text -> Doc ann
quoted txt = pretty (show (T.unpack txt))

directionText :: ForeignDirection -> Doc ann
directionText direction =
  case direction of
    ForeignImport -> "import"
    ForeignExport -> "export"

callConvText :: CallConv -> Doc ann
callConvText callConv =
  case callConv of
    CCall -> "ccall"
    StdCall -> "stdcall"

safetyText :: ForeignSafety -> Doc ann
safetyText safety =
  case safety of
    Safe -> "safe"
    Unsafe -> "unsafe"

isOperatorToken :: Text -> Bool
isOperatorToken tok =
  not (T.null tok) && T.all (`elem` (":!#$%&*+./<=>?@\\^|-~" :: String)) tok

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

renderDeclTokens :: [DeclToken] -> Text
renderDeclTokens =
  snd . foldl step (Nothing, T.empty)
  where
    step (prevTok, acc) tok =
      let spaced =
            case prevTok of
              Nothing -> acc
              Just prev ->
                if needsSpace prev tok
                  then acc <> " "
                  else acc
       in (Just tok, spaced <> tokenText tok)

    tokenText tok =
      case tok of
        TokWord txt -> txt
        TokSymbol txt -> txt
        TokString txt -> txt
        TokChar txt -> txt
        TokPunct c -> T.singleton c

    needsSpace prev curr =
      case (prev, curr) of
        (TokPunct p, _) | p `elem` ("([{" :: String) -> False
        (_, TokPunct c) | c `elem` (")]},;" :: String) -> False
        (TokPunct ',', _) -> True
        (TokPunct ';', _) -> True
        (TokSymbol "!", TokWord _) -> False
        _ -> True
