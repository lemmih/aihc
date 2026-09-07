-- | Apply small System FC normalization rules.
module Aihc.Fc.Normalize
  ( normalizeProgram,
  )
where

import Aihc.Fc.Name (Name, nameText)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, extendBinder, reduceType, repOf, typeEnvFromProgram)
import Aihc.Resolve (PackageId)

normalizeProgram :: PackageId -> Program -> Program
normalizeProgram primPackage program =
  program {programDecls = map (normalizeDecl env) (programDecls program)}
  where
    env = typeEnvFromProgram primPackage program

normalizeDecl :: TypeEnv -> Decl -> Decl
normalizeDecl env decl =
  case decl of
    DeclVal declaration ->
      DeclVal declaration {valBody = normalizeExpr env (valBody declaration)}
    _ -> decl

normalizeExpr :: TypeEnv -> Expr -> Expr
normalizeExpr env expr =
  case expr of
    ExApp function argument ->
      normalizeApp (normalizeExpr env function) (normalizeExpr env argument)
    ExTyApp function argument -> ExTyApp (normalizeExpr env function) argument
    ExLam binder body -> ExLam binder (normalizeExpr env body)
    ExTyLam binder body -> ExTyLam binder (normalizeExpr (extendBinder env binder) body)
    ExLet bind body ->
      normalizeLet
        env
        bind {bindRhs = normalizeExpr env (bindRhs bind)}
        (normalizeExpr env body)
    ExRec binds body ->
      ExRec
        (map (normalizeBind env) binds)
        (normalizeExpr env body)
    ExCase scrutinee binder resultType alternatives ->
      ExCase
        (normalizeExpr env scrutinee)
        binder
        resultType
        (map (normalizeAlt env) alternatives)
    ExCast body coercion -> ExCast (normalizeExpr env body) coercion
    ExForeignCall call types arguments -> ExForeignCall call types (map (normalizeExpr env) arguments)
    _ -> expr

-- | Inline a lifted let binding that its body uses at most once, outside any
-- lambda or recursive binding. Such a binding is a thunk that is forced at
-- most once, so its right-hand side can move to the use without duplicating
-- work, and without it the desugared @case@ scrutinee is evaluated directly
-- instead of being allocated and then forced. A binding with no use is
-- dropped. Names are unique before tidying, so the move captures nothing.
normalizeLet :: TypeEnv -> Bind -> Expr -> Expr
normalizeLet env bind body
  | isLiftedBinder env binder,
    Occurrences count False <- occurrences (binderName binder) body,
    count <= 1 =
      substExpr (binderName binder) (bindRhs bind) body
  | otherwise = ExLet bind body
  where
    binder = bindBinder bind

isLiftedBinder :: TypeEnv -> Binder -> Bool
isLiftedBinder env binder =
  case reduceType env <$> repOf env (binderType binder) of
    Just (TyCon name) -> nameText name == "LiftedRep"
    Just (TyApp (TyCon boxed) (TyCon levity)) ->
      nameText boxed == "BoxedRep" && nameText levity == "Lifted"
    _ -> False

-- | How often a name occurs in an expression, and whether any occurrence
-- sits under a lambda or inside a recursive binding, where the surrounding
-- code may run more than once.
data Occurrences = Occurrences !Int !Bool

instance Semigroup Occurrences where
  Occurrences count1 repeated1 <> Occurrences count2 repeated2 =
    Occurrences (count1 + count2) (repeated1 || repeated2)

instance Monoid Occurrences where
  mempty = Occurrences 0 False

occurrences :: Name -> Expr -> Occurrences
occurrences name = go
  where
    go expr =
      case expr of
        ExVar var
          | var == name -> Occurrences 1 False
          | otherwise -> mempty
        ExLit {} -> mempty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam _ body -> repeated (go body)
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) <> go body
        ExRec binds body -> repeated (foldMap (go . bindRhs) binds) <> go body
        ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExCast body _ -> go body
        ExCoercion {} -> mempty
        ExForeignCall _ _ arguments -> foldMap go arguments
    repeated (Occurrences count _) = Occurrences count (count > 0)

-- | Replace every occurrence of a name by an expression.
substExpr :: Name -> Expr -> Expr -> Expr
substExpr name replacement = go
  where
    go expr =
      case expr of
        ExVar var
          | var == name -> replacement
          | otherwise -> expr
        ExLit {} -> expr
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function argument -> ExTyApp (go function) argument
        ExLam binder body -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExLet bind body -> ExLet (substBind bind) (go body)
        ExRec binds body -> ExRec (map substBind binds) (go body)
        ExCase scrutinee binder resultType alternatives ->
          ExCase (go scrutinee) binder resultType (map substAlt alternatives)
        ExCast body coercion -> ExCast (go body) coercion
        ExCoercion {} -> expr
        ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)
    substBind bind = bind {bindRhs = go (bindRhs bind)}
    substAlt alternative = alternative {altRhs = go (altRhs alternative)}

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

normalizeBind :: TypeEnv -> Bind -> Bind
normalizeBind env bind = bind {bindRhs = normalizeExpr env (bindRhs bind)}

normalizeAlt :: TypeEnv -> Alt -> Alt
normalizeAlt env alternative =
  alternative {altRhs = normalizeExpr altEnv (altRhs alternative)}
  where
    altEnv = foldl' extendBinder env (altTypeBinders alternative)
