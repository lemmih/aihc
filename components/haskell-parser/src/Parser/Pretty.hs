{-# LANGUAGE OverloadedStrings #-}

module Parser.Pretty
  ( prettyExpr,
    prettyModule,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast

prettyExpr :: Expr -> Text
prettyExpr = prettyExprPrec 0

prettyModule :: Module -> Text
prettyModule modu =
  T.unlines (headerLines <> map prettyDecl (moduleDecls modu))
  where
    headerLines =
      case moduleName modu of
        Just name -> ["module " <> name <> " where"]
        Nothing -> []

prettyDecl :: Decl -> Text
prettyDecl decl =
  case decl of
    Decl {declName = name, declExpr = expr} -> name <> " = " <> prettyExpr expr
    PatternDecl {patternLhs = lhs} -> lhs <> " = 0"
    TypeSigDecl {typeSigName = name} -> name <> " :: T"
    FunctionDecl {functionName = name} -> functionBinder name <> " _x = ?"
    TypeDecl {typeName = name} -> "type " <> name <> " = " <> name
    DataDecl {dataTypeName = typeName, dataConstructors = ctors} ->
      if null ctors
        then "data " <> typeName
        else "data " <> typeName <> " = " <> T.intercalate " | " ctors
    NewtypeDecl {newtypeName = name, newtypeConstructor = ctor} ->
      "newtype " <> name <> " = " <> fromMaybe ("Mk" <> name) ctor
    ClassDecl {className = name} -> "class " <> name <> " where"
    InstanceDecl {instanceClassName = name} -> "instance " <> name <> " where"
    FixityDecl {fixityAssoc = assoc, fixityPrecedence = prec, fixityOperator = op} ->
      assoc <> maybe "" ((" " <>) . T.pack . show) prec <> " " <> op
    DefaultDecl {defaultTypes = tys} -> "default (" <> T.intercalate ", " tys <> ")"
    ForeignDecl
      { foreignDirection = direction,
        foreignCallConv = callConv,
        foreignSafety = safety,
        foreignEntity = entity,
        foreignName = name
      } ->
        T.unwords
          [ "foreign",
            directionText direction,
            callConvText callConv,
            maybe "" safetyText safety,
            maybe "" quoted entity,
            name,
            "::",
            "Int"
          ]

functionBinder :: Text -> Text
functionBinder name
  | isOperatorToken name = "(" <> name <> ")"
  | otherwise = name

prettyExprPrec :: Int -> Expr -> Text
prettyExprPrec prec expr =
  case expr of
    EApp fn arg ->
      parenthesize (prec > 2) (prettyExprPrec 2 fn <> " " <> prettyExprPrec 3 arg)
    EVar name -> name
    EInt value -> T.pack (show value)
    EFloat value -> T.pack (show value)
    EChar value -> T.pack (show value)
    EString value -> T.pack (show value)
    EIf cond yes no ->
      parenthesize
        (prec > 0)
        ("if " <> prettyExpr cond <> " then " <> prettyExpr yes <> " else " <> prettyExpr no)
    ELambda params body -> parenthesize (prec > 0) ("\\" <> T.unwords params <> " -> " <> prettyExpr body)
    EInfix lhs op rhs -> parenthesize (prec > 1) (prettyExprPrec 1 lhs <> " " <> op <> " " <> prettyExprPrec 1 rhs)
    ENegate inner -> parenthesize (prec > 2) ("-" <> prettyExprPrec 3 inner)
    ESectionL lhs op -> parenthesize False ("(" <> prettyExpr lhs <> " " <> op <> ")")
    ESectionR op rhs -> parenthesize False ("(" <> op <> " " <> prettyExpr rhs <> ")")
    ELet bindings body ->
      parenthesize
        (prec > 0)
        ( "let "
            <> T.intercalate "; " [name <> " = " <> prettyExpr value | (name, value) <- bindings]
            <> " in "
            <> prettyExpr body
        )
    ECase scrutinee alts ->
      parenthesize
        (prec > 0)
        ( "case "
            <> prettyExpr scrutinee
            <> " of "
            <> T.intercalate "; " [pat <> " -> " <> prettyExpr altExpr | CaseAlt pat altExpr <- alts]
        )
    EDo stmts ->
      parenthesize
        (prec > 0)
        ( "do { "
            <> T.intercalate "; " (map prettyDoStmt stmts)
            <> " }"
        )
    EListComp body quals ->
      "["
        <> prettyExpr body
        <> " | "
        <> T.intercalate ", " (map prettyCompStmt quals)
        <> "]"
    EArithSeq seqInfo -> prettyArithSeq seqInfo
    ERecordCon name fields ->
      name <> " { " <> T.intercalate ", " [fname <> " = " <> prettyExpr fexpr | (fname, fexpr) <- fields] <> " }"
    ERecordUpd base fields ->
      prettyExprPrec 3 base <> " { " <> T.intercalate ", " [fname <> " = " <> prettyExpr fexpr | (fname, fexpr) <- fields] <> " }"
    ETypeSig inner sigText -> parenthesize (prec > 1) (prettyExprPrec 1 inner <> " :: " <> sigText)
    EList values -> "[" <> T.intercalate ", " (map prettyExpr values) <> "]"
    ETuple values -> "(" <> T.intercalate ", " (map prettyExpr values) <> ")"
    ETupleCon arity -> "(" <> T.replicate (max 1 (arity - 1)) "," <> ")"

prettyDoStmt :: DoStmt -> Text
prettyDoStmt stmt =
  case stmt of
    DoBind pat expr -> pat <> " <- " <> prettyExpr expr
    DoLet bindings -> "let " <> T.intercalate "; " [name <> " = " <> prettyExpr value | (name, value) <- bindings]
    DoExpr expr -> prettyExpr expr

prettyCompStmt :: CompStmt -> Text
prettyCompStmt stmt =
  case stmt of
    CompGen pat expr -> pat <> " <- " <> prettyExpr expr
    CompGuard expr -> prettyExpr expr
    CompLet bindings -> "let " <> T.intercalate "; " [name <> " = " <> prettyExpr value | (name, value) <- bindings]

prettyArithSeq :: ArithSeq -> Text
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom fromExpr -> "[" <> prettyExpr fromExpr <> "..]"
    ArithSeqFromThen fromExpr thenExpr -> "[" <> prettyExpr fromExpr <> ", " <> prettyExpr thenExpr <> "..]"
    ArithSeqFromTo fromExpr toExpr -> "[" <> prettyExpr fromExpr <> ".." <> prettyExpr toExpr <> "]"
    ArithSeqFromThenTo fromExpr thenExpr toExpr ->
      "[" <> prettyExpr fromExpr <> ", " <> prettyExpr thenExpr <> ".." <> prettyExpr toExpr <> "]"

parenthesize :: Bool -> Text -> Text
parenthesize shouldWrap txt
  | shouldWrap = "(" <> txt <> ")"
  | otherwise = txt

quoted :: Text -> Text
quoted txt = T.pack (show (T.unpack txt))

directionText :: ForeignDirection -> Text
directionText direction =
  case direction of
    ForeignImport -> "import"
    ForeignExport -> "export"

callConvText :: CallConv -> Text
callConvText callConv =
  case callConv of
    CCall -> "ccall"
    StdCall -> "stdcall"

safetyText :: ForeignSafety -> Text
safetyText safety =
  case safety of
    Safe -> "safe"
    Unsafe -> "unsafe"

isOperatorToken :: Text -> Bool
isOperatorToken tok =
  not (T.null tok) && T.all (`elem` (":!#$%&*+./<=>?@\\^|-~" :: String)) tok
