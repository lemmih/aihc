-- | Apply small System FC normalization rules.
module Aihc.Fc.Normalize
  ( normalizeProgram,
  )
where

import Aihc.Fc.Name (Name)
import Aihc.Fc.Syntax

normalizeProgram :: Program -> Program
normalizeProgram program =
  program {programDecls = map normalizeDecl (programDecls program)}

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclVal declaration ->
      DeclVal declaration {valBody = normalizeExpr (valBody declaration)}
    _ -> decl

normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    ExApp function argument ->
      normalizeApp (normalizeExpr function) (normalizeExpr argument)
    ExTyApp function argument -> ExTyApp (normalizeExpr function) argument
    ExLam binder body -> ExLam binder (normalizeExpr body)
    ExTyLam binder body -> ExTyLam binder (normalizeExpr body)
    ExLet bind body ->
      ExLet
        bind {bindRhs = normalizeExpr (bindRhs bind)}
        (normalizeExpr body)
    ExRec binds body ->
      ExRec
        (map normalizeBind binds)
        (normalizeExpr body)
    ExCase scrutinee binder resultType alternatives ->
      ExCase
        (normalizeExpr scrutinee)
        binder
        resultType
        (map normalizeAlt alternatives)
    ExCast body coercion -> ExCast (normalizeExpr body) coercion
    ExForeignCall call types arguments -> ExForeignCall call types (map normalizeExpr arguments)
    _ -> expr

normalizeApp :: Expr -> Expr -> Expr
normalizeApp function argument =
  case function of
    ExLam binder (ExCast (ExVar name) coercion)
      | name == binderName binder -> ExCast argument coercion
    ExLam binder body
      | Just reduced <- fillForeignArgument (binderName binder) argument body -> reduced
    _ -> ExApp function argument

-- | Apply a lambda that only passes its parameter to a foreign call. The
-- desugarer wraps each foreign call in one such lambda for each argument, so
-- an applied wrapper becomes a call with the argument in place. The parameter
-- has one use, so the argument moves without duplication of work.
fillForeignArgument :: Name -> Expr -> Expr -> Maybe Expr
fillForeignArgument parameter argument body =
  case body of
    ExLam binder inner -> ExLam binder <$> fillForeignArgument parameter argument inner
    ExForeignCall call types arguments
      | [_] <- filter isParameter arguments ->
          Just (ExForeignCall call types (map replace arguments))
    _ -> Nothing
  where
    isParameter expr =
      case expr of
        ExVar name -> name == parameter
        _ -> False
    replace expr
      | isParameter expr = argument
      | otherwise = expr

normalizeBind :: Bind -> Bind
normalizeBind bind = bind {bindRhs = normalizeExpr (bindRhs bind)}

normalizeAlt :: Alt -> Alt
normalizeAlt alternative =
  alternative {altRhs = normalizeExpr (altRhs alternative)}
