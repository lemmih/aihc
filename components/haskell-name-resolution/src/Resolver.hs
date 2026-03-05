{-# LANGUAGE OverloadedStrings #-}

module Resolver
  ( defaultResolveConfig,
    resolveModule,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Parser.Ast
import Resolver.Ast
import Resolver.Types

defaultResolveConfig :: ResolveConfig
defaultResolveConfig =
  ResolveConfig
    { preludeMode = ImplicitPreludeOn,
      enabledExtensions = S.empty
    }

data BindingInfo = BindingInfo
  { bindingId :: !NameId,
    bindingClass :: !NameClass
  }

data ResolveState = ResolveState
  { nextId :: !Int,
    env :: !(M.Map Text BindingInfo),
    diags :: [Diagnostic]
  }

resolveModule :: ResolveConfig -> Module -> ResolveResult ResolvedModule
resolveModule cfg modu =
  let (declsRev, finalState) = resolveDecls cfg (moduleDecls modu) emptyState
   in ResolveResult
        { resolved =
            ResolvedModule
              { resolvedModuleName = moduleName modu,
                resolvedDecls = reverse declsRev
              },
          diagnostics = reverse (diags finalState)
        }
  where
    emptyState =
      ResolveState
        { nextId = 0,
          env = preludeEnv (preludeMode cfg),
          diags = []
        }

resolveDecls :: ResolveConfig -> [Decl] -> ResolveState -> ([ResolvedDecl], ResolveState)
resolveDecls cfg decls st0 =
  let (binderState, binders) = assignTopLevelBinders decls st0
   in foldl' (resolveOne cfg binders) ([], binderState) decls
  where
    resolveOne cfg' binders (acc, st) decl =
      case decl of
        DeclValue valueDecl ->
          case valueDeclBinderName valueDecl of
            Nothing ->
              let (resolvedExprs, st') = resolveDeclValueExprs cfg' valueDecl st
               in case resolvedExprs of
                    (_ : _) -> (acc, st')
                    [] -> (acc, st')
            Just name ->
              let thisId = binders M.! name
                  (resolvedExprs, st') = resolveDeclValueExprs cfg' valueDecl st
                  pickedExpr =
                    case resolvedExprs of
                      [] -> RInt 0
                      (firstExpr : _) -> firstExpr
                  outDecl =
                    ResolvedDecl
                      { resolvedDeclName = name,
                        resolvedDeclId = thisId,
                        resolvedDeclExpr = pickedExpr
                      }
               in (outDecl : acc, st')
        _ -> (acc, st)

assignTopLevelBinders :: [Decl] -> ResolveState -> (ResolveState, M.Map Text NameId)
assignTopLevelBinders decls st0 = foldl' step (st0, M.empty) decls
  where
    step (st, ids) decl =
      case declValueBinderNames decl of
        [] -> (st, ids)
        [name] ->
          let thisId = NameId (nextId st)
              stWithId = st {nextId = nextId st + 1}
           in case M.lookup name ids of
                Just _ ->
                  let dupDiag =
                        Diagnostic
                          { diagCode = EDuplicateBinding,
                            diagSeverity = Error,
                            diagMessage = "duplicate top-level binding: " <> name,
                            diagSpan = NoSpan
                          }
                   in (stWithId {diags = dupDiag : diags stWithId}, ids)
                Nothing ->
                  let binding = BindingInfo {bindingId = thisId, bindingClass = TopLevelBinder}
                   in (stWithId {env = M.insert name binding (env stWithId)}, M.insert name thisId ids)
        _ -> (st, ids)

resolveDeclValueExprs :: ResolveConfig -> ValueDecl -> ResolveState -> ([ResolvedExpr], ResolveState)
resolveDeclValueExprs cfg valueDecl st0 =
  foldl' step ([], st0) (valueDeclExprs valueDecl)
  where
    step (acc, st) expr =
      let (expr', st') = resolveExpr cfg expr st
       in (acc <> [expr'], st')

valueDeclExprs :: ValueDecl -> [Expr]
valueDeclExprs valueDecl =
  case valueDecl of
    FunctionBind _ matches -> concatMap matchExprs matches
    PatternBind _ rhs -> rhsExprs rhs

matchExprs :: Match -> [Expr]
matchExprs match = rhsExprs (matchRhs match)

rhsExprs :: Rhs -> [Expr]
rhsExprs rhs =
  case rhs of
    UnguardedRhs expr -> [expr]
    GuardedRhss grhss -> map guardedRhsBody grhss

resolveExpr :: ResolveConfig -> Expr -> ResolveState -> (ResolvedExpr, ResolveState)
resolveExpr cfg expr st =
  case expr of
    EInt n -> (RInt n, st)
    EApp f x ->
      let (f', st1) = resolveExpr cfg f st
          (x', st2) = resolveExpr cfg x st1
       in (RApp f' x', st2)
    EParen inner -> resolveExpr cfg inner st
    EWhere body binds ->
      let (binds', st1) =
            foldl'
              ( \(acc, stAcc) (_, bindExpr) ->
                  let (resolvedBind, stNext) = resolveExpr cfg bindExpr stAcc
                   in (acc <> [resolvedBind], stNext)
              )
              ([], st)
              binds
          (body', st2) = resolveExpr cfg body st1
          combined = foldl' RApp body' binds'
       in (combined, st2)
    EVar name ->
      case M.lookup name (env st) of
        Just info ->
          ( RVar
              ResolvedName
                { rnText = name,
                  rnId = Just (bindingId info),
                  rnClass = bindingClass info
                },
            st
          )
        Nothing ->
          let diag =
                Diagnostic
                  { diagCode = EUnboundVariable,
                    diagSeverity = Error,
                    diagMessage = "unbound variable: " <> name,
                    diagSpan = NoSpan
                  }
           in ( RVar ResolvedName {rnText = name, rnId = Nothing, rnClass = Unresolved},
                st {diags = diag : diags st}
              )

preludeEnv :: PreludeMode -> M.Map Text BindingInfo
preludeEnv mode =
  case mode of
    ImplicitPreludeOff -> M.empty
    ImplicitPreludeOn ->
      M.fromList $
        zipWith
          (\ix name -> (name, BindingInfo {bindingId = NameId ix, bindingClass = PreludeBinder}))
          [100000 ..]
          preludeImplicitNames

preludeImplicitNames :: [Text]
preludeImplicitNames =
  ["return", ">>=", ">>", "fail", "fromInteger", "ifThenElse"]
