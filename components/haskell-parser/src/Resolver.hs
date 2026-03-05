{-# LANGUAGE OverloadedStrings #-}

module Resolver
  ( defaultResolveConfig
  , resolveModule
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Parser.Ast
import Resolver.Ast
import Resolver.Types

defaultResolveConfig :: ResolveConfig
defaultResolveConfig =
  ResolveConfig
    { preludeMode = ImplicitPreludeOn
    , enabledExtensions = S.empty
    }

data BindingInfo = BindingInfo
  { bindingId :: !NameId
  , bindingClass :: !NameClass
  }

data ResolveState = ResolveState
  { nextId :: !Int
  , env :: !(M.Map Text BindingInfo)
  , diags :: [Diagnostic]
  }

resolveModule :: ResolveConfig -> Module -> ResolveResult ResolvedModule
resolveModule cfg modu =
  let (declsRev, finalState) = resolveDecls cfg (moduleDecls modu) emptyState
   in ResolveResult
        { resolved =
            ResolvedModule
              { resolvedModuleName = moduleName modu
              , resolvedDecls = reverse declsRev
              }
        , diagnostics = reverse (diags finalState)
        }
  where
    emptyState =
      ResolveState
        { nextId = 0
        , env = preludeEnv (preludeMode cfg)
        , diags = []
        }

resolveDecls :: ResolveConfig -> [Decl] -> ResolveState -> ([ResolvedDecl], ResolveState)
resolveDecls cfg decls st0 =
  let (binderState, binders) = assignTopLevelBinders decls st0
   in foldl' (resolveOne cfg binders) ([], binderState) decls
  where
    resolveOne cfg' binders (acc, st) decl =
      let thisId = binders M.! declName decl
          (expr', st') = resolveExpr cfg' (declExpr decl) st
          outDecl =
            ResolvedDecl
              { resolvedDeclName = declName decl
              , resolvedDeclId = thisId
              , resolvedDeclExpr = expr'
              }
       in (outDecl : acc, st')

assignTopLevelBinders :: [Decl] -> ResolveState -> (ResolveState, M.Map Text NameId)
assignTopLevelBinders decls st0 = foldl' step (st0, M.empty) decls
  where
    step (st, ids) decl =
      let name = declName decl
          thisId = NameId (nextId st)
          stWithId = st {nextId = nextId st + 1}
       in case M.lookup name ids of
            Just _ ->
              let dupDiag =
                    Diagnostic
                      { diagCode = EDuplicateBinding
                      , diagSeverity = Error
                      , diagMessage = "duplicate top-level binding: " <> name
                      , diagSpan = NoSpan
                      }
               in (stWithId {diags = dupDiag : diags stWithId}, ids)
            Nothing ->
              let binding = BindingInfo {bindingId = thisId, bindingClass = TopLevelBinder}
               in (stWithId {env = M.insert name binding (env stWithId)}, M.insert name thisId ids)

resolveExpr :: ResolveConfig -> Expr -> ResolveState -> (ResolvedExpr, ResolveState)
resolveExpr cfg expr st =
  case expr of
    EInt n -> (RInt n, st)
    EApp f x ->
      let (f', st1) = resolveExpr cfg f st
          (x', st2) = resolveExpr cfg x st1
       in (RApp f' x', st2)
    EVar name ->
      case M.lookup name (env st) of
        Just info ->
          ( RVar
              ResolvedName
                { rnText = name
                , rnId = Just (bindingId info)
                , rnClass = bindingClass info
                }
          , st
          )
        Nothing ->
          let diag =
                Diagnostic
                  { diagCode = EUnboundVariable
                  , diagSeverity = Error
                  , diagMessage = "unbound variable: " <> name
                  , diagSpan = NoSpan
                  }
           in ( RVar ResolvedName {rnText = name, rnId = Nothing, rnClass = Unresolved}
              , st {diags = diag : diags st}
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
