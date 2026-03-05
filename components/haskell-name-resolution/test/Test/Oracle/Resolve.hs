{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle.Resolve
  ( oracleResolveFacts
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Parser.Canonical
import Resolver.Types
import Test.Oracle (oracleCanonicalModule)
import Test.ResolveFacts

oracleResolveFacts :: ResolveConfig -> Text -> Either Text ResolveFacts
oracleResolveFacts cfg source = do
  canon <- oracleCanonicalModule source
  pure (resolveCanonical cfg canon)

resolveCanonical :: ResolveConfig -> CanonicalModule -> ResolveFacts
resolveCanonical cfg modu =
  let (declMap, dupDiags) = gatherDecls (canonicalDecls modu)
      (varFacts, unboundDiags) = foldMap (resolveDeclExpr declMap) (canonicalDecls modu)
   in ResolveFacts
        { rfModuleName = canonicalModuleName modu
        , rfDeclNames = map canonicalDeclName (canonicalDecls modu)
        , rfVars = varFacts
        , rfDiagnosticCodes = dupDiags <> unboundDiags
        }
  where
    resolveDeclExpr declMap decl = resolveExpr declMap (canonicalDeclExpr decl)

    resolveExpr declMap expr =
      case expr of
        CInt _ -> ([], [])
        CApp f x ->
          let (fFacts, fDiags) = resolveExpr declMap f
              (xFacts, xDiags) = resolveExpr declMap x
           in (fFacts <> xFacts, fDiags <> xDiags)
        CVar name ->
          case M.lookup name declMap of
            Just _ ->
              ([VarFact {vfName = name, vfBinding = Just name, vfClass = TopLevelBinder}], [])
            Nothing ->
              if isPreludeName cfg name
                then ([VarFact {vfName = name, vfBinding = Just name, vfClass = PreludeBinder}], [])
                else ([VarFact {vfName = name, vfBinding = Nothing, vfClass = Unresolved}], [EUnboundVariable])

gatherDecls :: [CanonicalDecl] -> (M.Map Text (), [DiagnosticCode])
gatherDecls = foldl step (M.empty, [])
  where
    step (seen, diags) decl =
      let name = canonicalDeclName decl
       in case M.lookup name seen of
            Just _ -> (seen, diags <> [EDuplicateBinding])
            Nothing -> (M.insert name () seen, diags)

isPreludeName :: ResolveConfig -> Text -> Bool
isPreludeName cfg name =
  case preludeMode cfg of
    ImplicitPreludeOff -> False
    ImplicitPreludeOn -> name `elem` preludeNames

preludeNames :: [Text]
preludeNames = ["return", ">>=", ">>", "fail", "fromInteger", "ifThenElse"]
