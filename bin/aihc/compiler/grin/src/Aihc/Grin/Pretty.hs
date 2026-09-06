-- | Human-readable GRIN rendering for diagnostics and golden tests.
module Aihc.Grin.Pretty
  ( prettyProgram,
    prettyExpr,
  )
where

import Aihc.Grin.Syntax
import Data.ByteString qualified as BS
import Data.Char (chr, isPrint, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Prettyprinter (Doc, comma, hardline, hsep, indent, parens, pretty, punctuate, space, vsep, (<+>))

-- | The number that the printed program gives to each scope. A name from a
-- numbered scope prints as @number.name@, so that the package and the module
-- occur one time only.
newtype Scopes = Scopes (Map GrinScope Int)

noScopes :: Scopes
noScopes = Scopes Map.empty

programScopes :: GrinProgram -> Scopes
programScopes program = Scopes (Map.fromList (zip (grinProgramScopes program) [1 ..]))

prettyProgram :: GrinProgram -> Doc ann
prettyProgram program =
  vsep (punctuate hardline documents)
  where
    scopes = programScopes program
    documents =
      prettyScopes scopes
        <> map (prettyConstructor scopes) (grinConstructors program)
        <> map prettyPrimitive (grinPrimitives program)
        <> map (prettyForeign scopes) (grinForeignCalls program)
        <> map (prettyGlobal scopes) (grinGlobals program)
        <> map (prettyFunction scopes) (grinFunctions program)

prettyScopes :: Scopes -> [Doc ann]
prettyScopes (Scopes numbers) =
  [ "scope" <+> pretty number <+> "=" <+> prettyQuoted (grinScopePackage scope) <+> prettyBareName (grinScopeModule scope)
  | (scope, number) <- Map.toAscList numbers
  ]

prettyConstructor :: Scopes -> (T.Text, [[GrinRep]]) -> Doc ann
prettyConstructor scopes (name, fieldLayouts) =
  "constructor" <+> prettyName scopes name <+> "[" <> hsep (punctuate comma (map prettyLayout fieldLayouts)) <> "]"
  where
    prettyLayout layout =
      case layout of
        [runtimeRep] -> prettyShow runtimeRep
        _ -> "[" <> hsep (punctuate comma (map prettyShow layout)) <> "]"

prettyPrimitive :: (GrinVar, Int) -> Doc ann
prettyPrimitive (var, arity) =
  "primitive" <+> prettyVar var <> "/" <> pretty arity

prettyForeign :: Scopes -> GrinForeignCall -> Doc ann
prettyForeign scopes foreignCall =
  "foreign" <+> prettyForeignCall scopes foreignCall

prettyGlobal :: Scopes -> (T.Text, GrinNode) -> Doc ann
prettyGlobal scopes (name, node) =
  "global" <+> prettyName scopes name <+> "=" <+> prettyNode scopes node

prettyFunction :: Scopes -> GrinFunction -> Doc ann
prettyFunction scopes function =
  prettyFunctionName (grinFunctionName function)
    <> foldMap ((space <>) . prettyVarAtom) (grinFunctionParameters function)
    <+> "->"
    <+> prettyShow (grinFunctionResultRep function)
    <+> "="
    <> hardline
    <> indent 2 (prettyExprWith scopes (grinFunctionBody function))

-- | Print one expression without a scope table. Diagnostics use this, where a
-- name has no numbered scope to refer to.
prettyExpr :: GrinExpr -> Doc ann
prettyExpr = prettyExprWith noScopes

prettyExprWith :: Scopes -> GrinExpr -> Doc ann
prettyExprWith scopes expr =
  case expr of
    GrinConstant values -> "constant" <> prettyValues scopes values
    GrinBind vars valueExpr body ->
      prettyBinders vars
        <+> "<-"
        <> hardline
        <> indent 2 (prettyExprWith scopes valueExpr)
        <> hardline
        <> prettyExprWith scopes body
    GrinStore node -> "store" <+> prettyNode scopes node
    GrinEnsureHeap requiredWords roots ->
      "ensure-heap" <+> prettyValue scopes requiredWords <> prettyValues scopes roots
    GrinStoreUnchecked node -> "store-unchecked" <+> prettyNode scopes node
    GrinStoreRec bindings body ->
      prettyStoreRec scopes "store-rec" bindings body
    GrinStoreRecUnchecked bindings body ->
      prettyStoreRec scopes "store-rec-unchecked" bindings body
    GrinUpdate pointer value ->
      "update" <+> prettyValue scopes pointer <+> prettyValue scopes value
    GrinUpdateBlackhole pointer value ->
      "update-blackhole" <+> prettyValue scopes pointer <+> prettyValue scopes value
    GrinEval runtimeRep value ->
      "eval" <+> "@" <> prettyRuntimeRepArgument runtimeRep <+> prettyValue scopes value
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      "cps-eval"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> hsep (map (prettyValue scopes) [value, continuation, updateContinuation])
    GrinCall runtimeRep functionName arguments ->
      "call"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyFunctionName functionName
        <> prettyValues scopes arguments
    GrinPrimitiveCall runtimeRep name arguments ->
      "primitive-call"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyName scopes name
        <> prettyValues scopes arguments
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      "cps-primitive-call"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyName scopes name
        <> prettyValues scopes arguments
        <+> "->"
        <+> prettyValue scopes continuation
    GrinApply runtimeRep function arguments ->
      "apply"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyValue scopes function
        <> prettyArgument scopes arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      "cps-apply"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyValue scopes function
        <> prettyArgument scopes arguments
        <+> "->"
        <+> prettyValue scopes continuation
    GrinContinue continuation values ->
      "continue" <+> prettyValue scopes continuation <> prettyArgument scopes values
    GrinCpsRaise exception continuation ->
      "raise-cps" <+> prettyValue scopes exception <+> prettyValue scopes continuation
    GrinHalt values -> "halt" <> prettyValues scopes values
    GrinExit status -> "exit" <+> prettyValue scopes status
    GrinCase scrutinee binder alternatives ->
      "case"
        <+> prettyValue scopes scrutinee
        <+> "as"
        <+> prettyVar binder
        <+> "of"
        <> hardline
        <> case alternatives of
          [] -> mempty
          _ -> indent 2 (vsep (map (prettyAlt scopes) alternatives))
    GrinThrow exception -> "throw" <+> prettyValue scopes exception
    GrinCatch runtimeRep action handler state ->
      "catch"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> hsep (map (prettyValue scopes) [action, handler])
        <> prettyValues scopes state
    GrinForeignCallExpr foreignCall arguments ->
      "foreign-call"
        <+> prettyForeignCall scopes foreignCall
        <+> "with"
        <> prettyValues scopes arguments

prettyStoreRec :: Scopes -> Doc ann -> [(GrinVar, GrinNode)] -> GrinExpr -> Doc ann
prettyStoreRec scopes name bindings body =
  name
    <> hardline
    <> indent 2 (vsep (map prettyBinding bindings))
    <> hardline
    <> prettyExprWith scopes body
  where
    prettyBinding (var, node) = prettyVar var <+> "=" <+> prettyNode scopes node

prettyValues :: Scopes -> [GrinValue] -> Doc ann
prettyValues scopes = foldMap ((space <>) . prettyValue scopes)

prettyArgument :: Scopes -> [GrinValue] -> Doc ann
prettyArgument scopes values =
  space
    <> case values of
      [] -> "()"
      [value] -> prettyValue scopes value
      _ -> parens (hsep (map (prettyValue scopes) values))

prettyBinders :: [GrinVar] -> Doc ann
prettyBinders vars =
  case vars of
    [] -> "()"
    _ -> hsep (punctuate comma (map prettyVar vars))

prettyAlt :: Scopes -> GrinAlt -> Doc ann
prettyAlt scopes alt =
  prettyAltCon scopes (grinAltCon alt)
    <> foldMap ((space <>) . prettyVarAtom) (grinAltBinders alt)
    <+> "->"
    <> hardline
    <> indent 2 (prettyExprWith scopes (grinAltRhs alt))

prettyAltCon :: Scopes -> GrinAltCon -> Doc ann
prettyAltCon scopes altCon =
  case altCon of
    GrinDataAlt name -> "data" <+> prettyName scopes name
    GrinLitAlt literal -> prettyLiteral literal
    GrinDefaultAlt -> "_"

prettyValue :: Scopes -> GrinValue -> Doc ann
prettyValue scopes value =
  case value of
    GrinVarValue var -> prettyVarAtom var
    GrinGlobalValue name -> "global-ref" <+> prettyName scopes name
    GrinLitValue literal -> prettyLiteral literal

prettyNode :: Scopes -> GrinNode -> Doc ann
prettyNode scopes node =
  parens
    ( prettyNodeTag scopes (grinNodeTag node)
        <> foldMap ((space <>) . prettyValue scopes) (grinNodeFields node)
    )

prettyNodeTag :: Scopes -> GrinNodeTag -> Doc ann
prettyNodeTag scopes nodeTag =
  case nodeTag of
    GrinConstructor name remaining ->
      "C" <> prettyName scopes name <> if remaining == 0 then mempty else "/" <> pretty remaining
    GrinClosure functionName argumentLayouts ->
      "P"
        <> prettyFunctionName functionName
        <> "/"
        <> if all (== [BoxedRep Lifted]) argumentLayouts
          then pretty (length argumentLayouts)
          else prettyLayouts argumentLayouts
    GrinThunk functionName -> "F" <> prettyFunctionName functionName

prettyLiteral :: GrinLiteral -> Doc ann
prettyLiteral literal =
  case literal of
    GrinLitInt runtimeRep value -> parens (pretty value <+> "::" <+> prettyShow runtimeRep)
    GrinLitChar runtimeRep value -> parens (pretty (show value) <+> "::" <+> prettyShow runtimeRep)
    GrinLitString value -> pretty (show (T.unpack value))
    GrinLitAddr value -> pretty (show (map (chr . fromIntegral) (BS.unpack value))) <> "#"

-- | A variable's number only disambiguates same-named binders, so the common
-- case of a single binder for a name prints without one.
prettyVar :: GrinVar -> Doc ann
prettyVar var =
  prettyBareName (grinVarName var)
    <> prettyNumber
    <+> "::"
    <+> prettyShow (grinVarRuntimeRep var)
  where
    number = grinVarUnique var
    prettyNumber
      | number == 0 && not (grinVarNameNeedsNumber (grinVarName var)) = mempty
      | otherwise = "%" <> pretty number

prettyVarAtom :: GrinVar -> Doc ann
prettyVarAtom = parens . prettyVar

prettyRuntimeRepArgument :: GrinRep -> Doc ann
prettyRuntimeRepArgument runtimeRep =
  case runtimeRep of
    VecRep {} -> parenthesized
    TupleRep {} -> parenthesized
    SumRep {} -> parenthesized
    BoxedRep {} -> parenthesized
    _ -> prettyShow runtimeRep
  where
    parenthesized = parens (prettyShow runtimeRep)

prettyLayouts :: [[GrinRep]] -> Doc ann
prettyLayouts layouts =
  "[" <> hsep (punctuate comma (map prettyLayout layouts)) <> "]"
  where
    prettyLayout layout = "[" <> hsep (punctuate comma (map prettyShow layout)) <> "]"

prettyForeignCall :: Scopes -> GrinForeignCall -> Doc ann
prettyForeignCall scopes foreignCall =
  prettyName scopes (grinForeignCallName foreignCall)
    <+> "="
    <+> prettyForeignTarget (grinForeignCallTarget foreignCall)
    <> pretty (show (T.unpack (grinForeignCallSymbol foreignCall)))
    <+> "::"
    <+> prettyForeignSignature (grinForeignCallSignature foreignCall)

prettyForeignTarget :: GrinForeignTarget -> Doc ann
prettyForeignTarget target =
  case target of
    GrinForeignFunction -> mempty
    GrinForeignAddress -> "address "

prettyForeignSignature :: GrinForeignSignature -> Doc ann
prettyForeignSignature signature =
  parens (hsep (punctuate comma (map prettyForeignType (grinForeignArgumentTypes signature))))
    <+> "->"
    <+> prettyForeignType (grinForeignResultType signature)
    <+> "!"
    <+> case grinForeignEffect signature of
      GrinForeignPure -> "pure"
      GrinForeignRealWorld -> "real-world"

prettyForeignType :: GrinForeignType -> Doc ann
prettyForeignType foreignType =
  case foreignType of
    GrinForeignInt -> "int"
    GrinForeignInt8 -> "int8"
    GrinForeignInt16 -> "int16"
    GrinForeignInt32 -> "int32"
    GrinForeignInt64 -> "int64"
    GrinForeignWord -> "word"
    GrinForeignWord8 -> "word8"
    GrinForeignWord16 -> "word16"
    GrinForeignWord32 -> "word32"
    GrinForeignWord64 -> "word64"
    GrinForeignFloat -> "float"
    GrinForeignDouble -> "double"
    GrinForeignAddr -> "addr"
    GrinForeignVoid -> "void"

prettyFunctionName :: FunctionName -> Doc ann
prettyFunctionName = prettyBareName . unFunctionName

-- | Print one top-level name. A name whose scope has a number prints as
-- @number.name@. Every other name prints in full.
prettyName :: Scopes -> T.Text -> Doc ann
prettyName (Scopes numbers) name =
  case grinNameScope name of
    Just (scope, baseName)
      | Just number <- Map.lookup scope numbers ->
          pretty number <> "." <> prettyBareName baseName
    _ -> prettyBareName name

prettyBareName :: T.Text -> Doc ann
prettyBareName name
  | not (T.null name) && T.all isBareNameCharacter name = pretty name
  | otherwise = prettyQuoted name
  where
    isBareNameCharacter character =
      isPrint character
        && not (isSpace character)
        && character `notElem` ['"', '(', ')', '[', ']', ',', '=', '/', '%']

prettyQuoted :: T.Text -> Doc ann
prettyQuoted = pretty . show . T.unpack

prettyShow :: (Show value) => value -> Doc ann
prettyShow = pretty . show
