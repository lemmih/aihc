{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Render
-- Description : Render syntax fragments to the one-line text used in the model
--
-- Signatures and declaration heads are stored in the model as text. This
-- module renders them from the parsed syntax with the parser's own pretty
-- printer, collapsed to one line.
module Aihc.Haddock.Render
  ( renderType,
    renderTypes,
    renderBinder,
    renderOccurrence,
    renderBinderHead,
    binderHeadTyVars,
    renderTyVarBinder,
    renderContext,
    renderForall,
    functionTypeArms,
    peelTypeAnnotations,
    typeHeadName,
    typeSpan,
    annotationSpan,
    isOperatorName,
  )
where

import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (LayoutOptions (..), PageWidth (..), Pretty (pretty), layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

renderType :: Type -> Text
renderType ty =
  T.unwords (T.words (renderStrict (layoutPretty (LayoutOptions Unbounded) (pretty ty))))

renderTypes :: [Type] -> Text
renderTypes = T.intercalate ", " . map renderType

-- | A binder as it appears at the start of a signature: operators are
-- parenthesised.
renderBinder :: UnqualifiedName -> Text
renderBinder name
  | isOperatorName name = "(" <> unqualifiedNameText name <> ")"
  | otherwise = unqualifiedNameText name

-- | The occurrence name without decoration, matching Haddock's stable names.
renderOccurrence :: UnqualifiedName -> Text
renderOccurrence = unqualifiedNameText

isOperatorName :: UnqualifiedName -> Bool
isOperatorName name =
  case unqualifiedNameType name of
    NameVarSym -> True
    NameConSym -> True
    _ -> False

renderBinderHead :: BinderHead UnqualifiedName -> Text
renderBinderHead head' =
  case head' of
    PrefixBinderHead name binders ->
      T.unwords (renderBinder name : map renderTyVarBinder binders)
    InfixBinderHead left name right rest ->
      T.unwords ((renderTyVarBinder left <> " " <> unqualifiedNameText name <> " " <> renderTyVarBinder right) : map renderTyVarBinder rest)

binderHeadTyVars :: BinderHead UnqualifiedName -> [TyVarBinder]
binderHeadTyVars head' =
  case head' of
    PrefixBinderHead _ binders -> binders
    InfixBinderHead left _ right rest -> left : right : rest

renderTyVarBinder :: TyVarBinder -> Text
renderTyVarBinder binder =
  case tyVarBinderKind binder of
    Nothing -> tyVarBinderName binder
    Just kind -> "(" <> tyVarBinderName binder <> " :: " <> renderType kind <> ")"

-- | Render a context with its arrow, or nothing for an empty context.
renderContext :: [Type] -> Text
renderContext ctx =
  case ctx of
    [] -> ""
    [single] -> renderType single <> " => "
    many -> "(" <> renderTypes many <> ") => "

renderForall :: [TyVarBinder] -> Text
renderForall binders =
  case binders of
    [] -> ""
    _ -> "forall " <> T.unwords (map renderTyVarBinder binders) <> ". "

-- | The arms of a function type: each argument followed by the result. Outer
-- annotations, quantifiers and contexts are skipped, matching how Haddock
-- numbers argument documentation. Each arm keeps its own annotation so it can
-- be located in the source.
functionTypeArms :: Type -> [Type]
functionTypeArms ty =
  case ty of
    TAnn _ inner
      | isSpine inner -> functionTypeArms inner
    TForall _ inner -> functionTypeArms inner
    TContext _ inner -> functionTypeArms inner
    TFun _ arg result -> arg : functionTypeArms result
    _ -> [ty]
  where
    isSpine inner =
      case inner of
        TAnn _ _ -> True
        TForall _ _ -> True
        TContext _ _ -> True
        TFun {} -> True
        _ -> False

peelTypeAnnotations :: Type -> Type
peelTypeAnnotations ty =
  case ty of
    TAnn _ inner -> peelTypeAnnotations inner
    TParen inner -> peelTypeAnnotations inner
    _ -> ty

-- | The head constructor of an applied type, such as @Eq@ in @Eq (Shape a)@.
typeHeadName :: Type -> Maybe Text
typeHeadName ty =
  case peelTypeAnnotations ty of
    TCon name _ -> Just (nameText name)
    TApp fun _ -> typeHeadName fun
    TInfix _ name _ _ -> Just (nameText name)
    TKindSig inner _ -> typeHeadName inner
    TTypeApp fun _ -> typeHeadName fun
    _ -> Nothing

typeSpan :: Type -> Maybe SourceSpan
typeSpan ty =
  case ty of
    TAnn ann _ -> annotationSpan ann
    _ -> Nothing

annotationSpan :: Annotation -> Maybe SourceSpan
annotationSpan ann =
  case fromAnnotation ann of
    Just sp@SourceSpan {} -> Just sp
    _ -> Nothing
