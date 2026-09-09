module Aihc.Cli.ResolveArtifact
  ( ResolveArtifact (..),
    decodeResolveArtifact,
    encodeResolveArtifact,
    encodeResolveArtifactParts,
    encodeResolveScope,
  )
where

import Aihc.Cli.Cbor (cborArray, cborText, cborWord, getArrayLength, getText, getWord)
import Aihc.Parser.Syntax (FixityAssoc (..), Name (..), NameType (..), UnqualifiedName (..))
import Aihc.Resolve (OperatorFixity (..), PackageId (..), ResolvedName (..), Scope (..))
import Control.Monad (replicateM, when)
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)

data ResolveArtifact = ResolveArtifact
  { resolveArtifactModuleName :: !Text,
    resolveArtifactScope :: !Scope
  }
  deriving (Eq)

encodeResolveArtifact :: ResolveArtifact -> BL.ByteString
encodeResolveArtifact = fst . encodeResolveArtifactParts

-- | The artifact bytes together with the bytes of the scope inside them, so
-- that the writer can take the scope digest from bytes it encodes anyway.
encodeResolveArtifactParts :: ResolveArtifact -> (BL.ByteString, BL.ByteString)
encodeResolveArtifactParts artifact =
  ( Builder.toLazyByteString $
      cborArray 3
        <> cborText "aihc-resolve"
        <> cborText (resolveArtifactModuleName artifact)
        <> Builder.lazyByteString scopeBytes,
    scopeBytes
  )
  where
    scopeBytes = encodeResolveScope (resolveArtifactScope artifact)

encodeResolveScope :: Scope -> BL.ByteString
encodeResolveScope = Builder.toLazyByteString . encodeScope

decodeResolveArtifact :: BS.ByteString -> Either String ResolveArtifact
decodeResolveArtifact bytes =
  case Get.runGetOrFail getArtifact (BL.fromStrict bytes) of
    Left (_, _, message) -> Left message
    Right (remaining, _, artifact)
      | BL.null remaining -> Right artifact
      | otherwise -> Left "invalid trailing data"

getArtifact :: Get.Get ResolveArtifact
getArtifact = do
  3 <- getArrayLength
  "aihc-resolve" <- getText
  resolveArtifactModuleName <- getText
  resolveArtifactScope <- getScope
  pure ResolveArtifact {resolveArtifactModuleName, resolveArtifactScope}

encodeScope :: Scope -> Builder.Builder
encodeScope scope =
  cborArray 8
    <> encodeResolvedMap (scopeTerms scope)
    <> encodeResolvedMap (scopeTypes scope)
    <> encodeTextListMap (scopeConstructors scope)
    <> encodeTextListMap (scopeRecordFields scope)
    <> encodeTextListMap (scopeMethods scope)
    <> encodeFixities (scopeFixities scope)
    <> encodeTextListMap (scopeAssociatedTypes scope)
    <> encodeScopeMap (scopeQualifiedModules scope)

getScope :: Get.Get Scope
getScope = do
  length' <- getArrayLength
  when (length' < 6 || length' > 8) $ fail "unsupported resolve scope layout"
  scopeTerms <- getResolvedMap
  scopeTypes <- getResolvedMap
  scopeConstructors <- getTextListMap
  scopeRecordFields <- getTextListMap
  scopeMethods <- getTextListMap
  scopeFixities <- getFixities
  -- Artifacts written before associated type families have six entries.
  scopeAssociatedTypes <- if length' >= 7 then getTextListMap else pure Map.empty
  -- Artifacts written before the qualified modules of a scope have seven entries.
  scopeQualifiedModules <- if length' >= 8 then getScopeMap else pure Map.empty
  pure Scope {scopeTerms, scopeTypes, scopeConstructors, scopeRecordFields, scopeMethods, scopeAssociatedTypes, scopeFixities, scopeQualifiedModules}

-- | The scope of each qualified import alias. An exported scope has no
-- alias, but the encoder does not depend on that property.
encodeScopeMap :: Map.Map Text Scope -> Builder.Builder
encodeScopeMap entries = cborArray (Map.size entries) <> foldMap encodeEntry (Map.toAscList entries)
  where
    encodeEntry (alias, scope) = cborArray 2 <> cborText alias <> encodeScope scope

getScopeMap :: Get.Get (Map.Map Text Scope)
getScopeMap = do
  count <- getArrayLength
  Map.fromList <$> replicateM count getEntry
  where
    getEntry = do
      2 <- getArrayLength
      (,) <$> getText <*> getScope

encodeResolvedMap :: Map.Map Text ResolvedName -> Builder.Builder
encodeResolvedMap entries = cborArray (Map.size entries) <> foldMap encodeEntry (Map.toAscList entries)
  where
    encodeEntry (name, resolved) = cborArray 2 <> cborText name <> encodeResolvedName resolved

getResolvedMap :: Get.Get (Map.Map Text ResolvedName)
getResolvedMap = do
  count <- getArrayLength
  Map.fromList <$> replicateM count getEntry
  where
    getEntry = do
      2 <- getArrayLength
      (,) <$> getText <*> getResolvedName

encodeResolvedName :: ResolvedName -> Builder.Builder
encodeResolvedName resolved =
  case resolved of
    ResolvedTopLevel (PackageId packageId) name ->
      cborArray 5 <> cborWord 0 <> cborText packageId <> cborText (fromMaybe "" (nameQualifier name)) <> cborWord (nameTypeTag (nameType name)) <> cborText (nameText name)
    ResolvedSyntax -> cborArray 1 <> cborWord 1
    ResolvedLocal unique name ->
      cborArray 4 <> cborWord 2 <> cborWord (fromIntegral unique) <> cborWord (nameTypeTag (unqualifiedNameType name)) <> cborText (unqualifiedNameText name)
    ResolvedError message -> cborArray 2 <> cborWord 3 <> cborText (T.pack message)

getResolvedName :: Get.Get ResolvedName
getResolvedName = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (5, 0) -> do
      packageId <- PackageId <$> getText
      qualifierText <- getText
      nameType' <- getNameType
      text <- getText
      let qualifier = if T.null qualifierText then Nothing else Just qualifierText
      pure (ResolvedTopLevel packageId (Name qualifier nameType' text []))
    (1, 1) -> pure ResolvedSyntax
    (4, 2) -> do
      unique <- getWord
      nameType' <- getNameType
      text <- getText
      pure (ResolvedLocal (fromIntegral unique) (UnqualifiedName nameType' text []))
    (2, 3) -> ResolvedError . T.unpack <$> getText
    _ -> fail "unsupported resolved name"

nameTypeTag :: NameType -> Word64
nameTypeTag nameType' = case nameType' of
  NameVarId -> 0
  NameConId -> 1
  NameVarSym -> 2
  NameConSym -> 3

getNameType :: Get.Get NameType
getNameType = do
  tag <- getWord
  case tag of
    0 -> pure NameVarId
    1 -> pure NameConId
    2 -> pure NameVarSym
    3 -> pure NameConSym
    _ -> fail "unsupported name type"

encodeTextListMap :: Map.Map Text [Text] -> Builder.Builder
encodeTextListMap entries = cborArray (Map.size entries) <> foldMap encodeEntry (Map.toAscList entries)
  where
    encodeEntry (name, values) = cborArray 2 <> cborText name <> cborArray (length values) <> foldMap cborText values

getTextListMap :: Get.Get (Map.Map Text [Text])
getTextListMap = do
  count <- getArrayLength
  Map.fromList <$> replicateM count getEntry
  where
    getEntry = do
      2 <- getArrayLength
      name <- getText
      valueCount <- getArrayLength
      values <- replicateM valueCount getText
      pure (name, values)

encodeFixities :: Map.Map Text OperatorFixity -> Builder.Builder
encodeFixities entries = cborArray (Map.size entries) <> foldMap encodeEntry (Map.toAscList entries)
  where
    encodeEntry (name, OperatorFixity association precedence) = cborArray 3 <> cborText name <> cborWord (fixityTag association) <> cborWord (fromIntegral precedence)
    fixityTag Infix = 0
    fixityTag InfixL = 1
    fixityTag InfixR = 2

getFixities :: Get.Get (Map.Map Text OperatorFixity)
getFixities = do
  count <- getArrayLength
  Map.fromList <$> replicateM count getEntry
  where
    getEntry = do
      3 <- getArrayLength
      name <- getText
      association <- getFixityAssoc
      precedence <- fromIntegral <$> getWord
      pure (name, OperatorFixity association precedence)

getFixityAssoc :: Get.Get FixityAssoc
getFixityAssoc = do
  tag <- getWord
  case tag of
    0 -> pure Infix
    1 -> pure InfixL
    2 -> pure InfixR
    _ -> fail "unsupported fixity association"
