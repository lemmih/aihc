module Aihc.Cli.ResolveArtifact
  ( ResolveArtifact (..),
    decodeResolveArtifact,
    encodeResolveArtifact,
    encodeResolveArtifactParts,
    encodeResolveScope,
  )
where

import Aihc.Parser.Syntax (FixityAssoc (..), Name (..), NameType (..), renderUnqualifiedName)
import Aihc.Resolve (OperatorFixity (..), PackageId (..), ResolvedName (..), Scope (..))
import Control.Monad (replicateM, unless, when)
import Data.Binary.Get qualified as Get
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)

data ResolveArtifact = ResolveArtifact
  { resolveArtifactModuleName :: !Text,
    resolveArtifactInputHashes :: ![(Text, Text)],
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
      cborArray 4
        <> cborText "aihc-resolve"
        <> cborText (resolveArtifactModuleName artifact)
        <> encodeHashes (resolveArtifactInputHashes artifact)
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
  4 <- getArrayLength
  "aihc-resolve" <- getText
  resolveArtifactModuleName <- getText
  resolveArtifactInputHashes <- getHashes
  resolveArtifactScope <- getScope
  pure ResolveArtifact {resolveArtifactModuleName, resolveArtifactInputHashes, resolveArtifactScope}

encodeHashes :: [(Text, Text)] -> Builder.Builder
encodeHashes hashes = cborArray (length hashes) <> foldMap pair hashes
  where
    pair (path, digest) = cborArray 2 <> cborText path <> cborText digest

getHashes :: Get.Get [(Text, Text)]
getHashes = do
  count <- getArrayLength
  replicateM count getPair
  where
    getPair = do
      2 <- getArrayLength
      (,) <$> getText <*> getText

encodeScope :: Scope -> Builder.Builder
encodeScope scope =
  cborArray 7
    <> encodeResolvedMap (scopeTerms scope)
    <> encodeResolvedMap (scopeTypes scope)
    <> encodeTextListMap (scopeConstructors scope)
    <> encodeTextListMap (scopeRecordFields scope)
    <> encodeTextListMap (scopeMethods scope)
    <> encodeFixities (scopeFixities scope)
    <> encodeTextListMap (scopeAssociatedTypes scope)

getScope :: Get.Get Scope
getScope = do
  length' <- getArrayLength
  when (length' < 6 || length' > 7) $ fail "unsupported resolve scope layout"
  scopeTerms <- getResolvedMap
  scopeTypes <- getResolvedMap
  scopeConstructors <- getTextListMap
  scopeRecordFields <- getTextListMap
  scopeMethods <- getTextListMap
  scopeFixities <- getFixities
  -- Artifacts written before associated type families have six entries.
  scopeAssociatedTypes <- if length' >= 7 then getTextListMap else pure Map.empty
  pure Scope {scopeTerms, scopeTypes, scopeConstructors, scopeRecordFields, scopeMethods, scopeAssociatedTypes, scopeFixities, scopeQualifiedModules = Map.empty}

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
    ResolvedLocal unique name -> cborArray 3 <> cborWord 2 <> cborWord (fromIntegral unique) <> cborText (renderUnqualifiedName name)
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

cborArray :: Int -> Builder.Builder
cborArray = cborMajor 4 . fromIntegral

cborText :: Text -> Builder.Builder
cborText value = cborMajor 3 (fromIntegral (BS.length bytes)) <> Builder.byteString bytes
  where
    bytes = TE.encodeUtf8 value

cborWord :: Word64 -> Builder.Builder
cborWord = cborMajor 0

cborMajor :: Word8 -> Word64 -> Builder.Builder
cborMajor major value
  | value < 24 = Builder.word8 (major * 32 + fromIntegral value)
  | value <= 255 = Builder.word8 (major * 32 + 24) <> Builder.word8 (fromIntegral value)
  | value <= 65535 = Builder.word8 (major * 32 + 25) <> Builder.word16BE (fromIntegral value)
  | value <= 4294967295 = Builder.word8 (major * 32 + 26) <> Builder.word32BE (fromIntegral value)
  | otherwise = Builder.word8 (major * 32 + 27) <> Builder.word64BE value

getArrayLength :: Get.Get Int
getArrayLength = fromIntegral <$> getMajor 4

getText :: Get.Get Text
getText = do
  length' <- getMajor 3
  TE.decodeUtf8 <$> Get.getByteString (fromIntegral length')

getWord :: Get.Get Word64
getWord = getMajor 0

getMajor :: Word8 -> Get.Get Word64
getMajor expected = do
  initial <- Get.getWord8
  let major = initial `shiftR` 5
      info = initial `mod` 32
  unless (major == expected) (fail "unexpected CBOR major type")
  case info of
    value | value < 24 -> pure (fromIntegral value)
    24 -> fromIntegral <$> Get.getWord8
    25 -> fromIntegral <$> Get.getWord16be
    26 -> fromIntegral <$> Get.getWord32be
    27 -> Get.getWord64be
    _ -> fail "unsupported CBOR length"
