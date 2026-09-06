{-# LANGUAGE OverloadedStrings #-}

-- | Shared data for direct native object generation.
--
-- A 'Draft' accumulates the bytes of every section in a
-- 'Data.ByteString.Builder.Builder' as the assembler walks the instruction
-- stream, together with the offset of each label and each fixup. Nothing is
-- kept per instruction, so the assembler allocates a few bytes rather than a
-- boxed item for every machine word it emits.
module Aihc.Native.Object
  ( Draft (..),
    Fixup (..),
    FixupKind (..),
    Image (..),
    ImageSection (..),
    Item (..),
    ObjectError (..),
    Relocation (..),
    SectionDraft (..),
    SectionRole (..),
    Symbol (..),
    addGlobal,
    addItem,
    emptyDraft,
    layoutDraft,
    selectSection,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)

data SectionRole
  = TextSection
  | TextConstantsSection
  | ReadOnlySection
  | DataSection
  | NoExecuteStackSection
  deriving (Eq, Ord, Show)

data FixupKind
  = Arm64Branch26
  | Arm64Branch19
  | Arm64Adr21
  | Arm64Page21
  | Arm64PageOffset12
  | Absolute64
  | X86Pc32
  | X86Plt32
  deriving (Eq, Show)

-- | A place in a section whose bytes depend on the address of a symbol. The
-- width is the number of bytes the fixup occupies and the word is the value
-- written there before the address is known: the encoded instruction for a
-- branch, and zero for an absolute slot.
data Fixup = Fixup
  { fixupKind :: !FixupKind,
    fixupTarget :: !Text,
    fixupAddend :: !Int64,
    fixupWidth :: !Int,
    fixupWord :: !Word64
  }
  deriving (Eq, Show)

data Item
  = Bytes !ByteString
  | -- | A little-endian word of the given byte width.
    Word !Int !Word64
  | Align !Int !ByteString
  | Label !Text
  | Apply !Fixup
  deriving (Eq, Show)

-- | The bytes of one section and the offsets recorded inside them.
data SectionDraft = SectionDraft
  { sectionSize :: !Word64,
    sectionAlignment :: !Int,
    sectionBytes :: !Builder.Builder,
    sectionLabelsRev :: ![(Text, Word64)],
    sectionFixupsRev :: ![(Word64, Fixup)]
  }

data Draft = Draft
  { draftCurrentSection :: !(Maybe SectionRole),
    draftSectionOrder :: ![SectionRole],
    draftSections :: !(Map SectionRole SectionDraft),
    draftGlobals :: !(Set Text)
  }

data Symbol = Symbol
  { symbolName :: !Text,
    symbolGlobal :: !Bool,
    symbolSection :: !(Maybe SectionRole),
    symbolOffset :: !Word64
  }
  deriving (Eq, Show)

-- | A place whose bytes the linker fills in. The symbol is a position in
-- 'imageSymbols', so an object writer never looks a name up again.
data Relocation = Relocation
  { relocationOffset :: !Word64,
    relocationKind :: !FixupKind,
    relocationSymbol :: !Int,
    relocationAddend :: !Int64
  }
  deriving (Eq, Show)

data ImageSection = ImageSection
  { imageSectionRole :: !SectionRole,
    imageSectionAlignment :: !Int,
    imageSectionBytes :: !BL.ByteString,
    imageSectionRelocations :: ![Relocation]
  }
  deriving (Eq, Show)

-- | The symbols are in ascending name order, and every 'relocationSymbol' is
-- a position in that list.
data Image = Image
  { imageSections :: ![ImageSection],
    imageSymbols :: ![Symbol]
  }
  deriving (Eq, Show)

data ObjectError
  = ObjectNoSection
  | ObjectDuplicateSymbol !Text
  | ObjectMissingSymbol !Text
  | ObjectInvalidAlignment !Int
  | ObjectDisplacementOutOfRange !Text
  | ObjectInvalidFixup !FixupKind
  | ObjectInvalidInput !Text
  | ObjectSizeOverflow !Text
  deriving (Eq, Show)

emptyDraft :: Draft
emptyDraft = Draft Nothing [] Map.empty Set.empty

emptySection :: SectionDraft
emptySection = SectionDraft 0 0 mempty [] []

selectSection :: SectionRole -> Draft -> Draft
selectSection role draft =
  draft
    { draftCurrentSection = Just role,
      draftSectionOrder =
        if role `elem` draftSectionOrder draft
          then draftSectionOrder draft
          else draftSectionOrder draft <> [role],
      draftSections = Map.insertWith (\_ existing -> existing) role emptySection (draftSections draft)
    }

addGlobal :: Text -> Draft -> Draft
addGlobal name draft = draft {draftGlobals = Set.insert name (draftGlobals draft)}

-- | Append one item to the current section.
addItem :: Item -> Draft -> Either ObjectError Draft
addItem item draft =
  case draftCurrentSection draft of
    Nothing -> Left ObjectNoSection
    Just role -> do
      let section = Map.findWithDefault emptySection role (draftSections draft)
      next <- appendItem item section
      pure draft {draftSections = Map.insert role next (draftSections draft)}

appendItem :: Item -> SectionDraft -> Either ObjectError SectionDraft
appendItem item section =
  case item of
    Bytes value -> pure (appendBytes (fromIntegral (BS.length value)) (Builder.byteString value) section)
    Word width value -> pure (appendBytes (fromIntegral width) (littleEndian width value) section)
    Label name -> pure section {sectionLabelsRev = (name, sectionSize section) : sectionLabelsRev section}
    Apply fixup ->
      pure
        ( appendBytes
            (fromIntegral (fixupWidth fixup))
            (littleEndian (fixupWidth fixup) (fixupWord fixup))
            section {sectionFixupsRev = (sectionSize section, fixup) : sectionFixupsRev section}
        )
    Align alignmentPower fill
      | alignmentPower < 0 || alignmentPower > 30 -> Left (ObjectInvalidAlignment alignmentPower)
      | BS.null fill -> Left (ObjectInvalidInput "empty alignment fill")
      | otherwise ->
          let boundary = (1 `shiftL` alignmentPower) :: Word64
              padding = fromIntegral ((boundary - sectionSize section `mod` boundary) `mod` boundary)
              (fillCount, fillRemainder) = padding `divMod` BS.length fill
              paddingBytes =
                mconcat (replicate fillCount (Builder.byteString fill))
                  <> Builder.byteString (BS.take fillRemainder fill)
           in pure
                ( appendBytes
                    (fromIntegral padding)
                    paddingBytes
                    section {sectionAlignment = max (sectionAlignment section) alignmentPower}
                )

appendBytes :: Word64 -> Builder.Builder -> SectionDraft -> SectionDraft
appendBytes width bytes section =
  section
    { sectionSize = sectionSize section + width,
      sectionBytes = sectionBytes section <> bytes
    }

littleEndian :: Int -> Word64 -> Builder.Builder
littleEndian width value =
  case width of
    1 -> Builder.word8 (fromIntegral value)
    2 -> Builder.word16LE (fromIntegral value)
    4 -> Builder.word32LE (fromIntegral value)
    8 -> Builder.word64LE value
    _ -> mconcat [Builder.word8 (byteAt index) | index <- [0 .. width - 1]]
  where
    byteAt index = fromIntegral (value `shiftR` (8 * index)) :: Word8

layoutDraft :: Draft -> Either ObjectError Image
layoutDraft draft = do
  let firstPass = map layoutSection (draftSectionOrder draft)
  definitions <- collectDefinitions firstPass
  let globals = draftGlobals draft
      -- Only a name that the linker needs becomes a symbol: a global one, or
      -- one that a relocation names. A label that this object resolves on its
      -- own, such as a branch target inside one function, needs no symbol.
      -- Generated code has many of these, and each one would otherwise cost a
      -- symbol table entry and its name.
      relocated =
        Set.fromList
          [ fixupTarget fixup
          | section <- firstPass,
            (_, fixup) <- laidFixups section,
            not (isLocalPatch globals definitions (laidRole section) fixup)
          ]
      kept = Map.keysSet (Map.filterWithKey (\name _ -> name `Set.member` globals || name `Set.member` relocated) definitions)
      names = Set.toAscList (kept <> relocated <> globals)
      symbols = map (makeSymbol definitions) names
      table = Map.fromDistinctAscList (zip names [0 ..])
  sections <- mapM (resolveSection globals definitions table) firstPass
  pure Image {imageSections = sections, imageSymbols = symbols}
  where
    layoutSection role =
      let section = Map.findWithDefault emptySection role (draftSections draft)
       in LaidSection
            { laidRole = role,
              laidAlignment = sectionAlignment section,
              laidBytes = BL.toStrict (Builder.toLazyByteString (sectionBytes section)),
              laidLabels = reverse (sectionLabelsRev section),
              laidFixups = reverse (sectionFixupsRev section)
            }
    makeSymbol definitions name =
      case Map.lookup name definitions of
        Just (role, offset) -> Symbol name (name `Set.member` draftGlobals draft) (Just role) offset
        Nothing -> Symbol name True Nothing 0

data LaidSection = LaidSection
  { laidRole :: !SectionRole,
    laidAlignment :: !Int,
    laidBytes :: !ByteString,
    laidLabels :: ![(Text, Word64)],
    laidFixups :: ![(Word64, Fixup)]
  }

collectDefinitions :: [LaidSection] -> Either ObjectError (Map Text (SectionRole, Word64))
collectDefinitions = foldl' addSection (Right Map.empty)
  where
    addSection result section = do
      definitions <- result
      foldl' (addLabel (laidRole section)) (Right definitions) (laidLabels section)
    addLabel role result (name, offset) = do
      definitions <- result
      if Map.member name definitions
        then Left (ObjectDuplicateSymbol name)
        else pure (Map.insert name (role, offset) definitions)

-- | Whether this object can fill a fixup in without the linker. The target
-- must sit in the same section, be private to this object, and have a kind
-- that 'patchLocal' handles.
isLocalPatch :: Set Text -> Map Text (SectionRole, Word64) -> SectionRole -> Fixup -> Bool
isLocalPatch globals definitions role fixup =
  canResolve (fixupKind fixup)
    && fixupTarget fixup `Set.notMember` globals
    && case Map.lookup (fixupTarget fixup) definitions of
      Just (targetRole, _) -> targetRole == role
      Nothing -> False

resolveSection :: Set Text -> Map Text (SectionRole, Word64) -> Map Text Int -> LaidSection -> Either ObjectError ImageSection
resolveSection globals definitions table section = do
  (patches, relocations) <- foldl' resolve (Right ([], [])) (laidFixups section)
  bytes <- applyPatches (laidBytes section) (reverse patches)
  pure
    ImageSection
      { imageSectionRole = laidRole section,
        imageSectionAlignment = laidAlignment section,
        imageSectionBytes = bytes,
        imageSectionRelocations = reverse relocations
      }
  where
    resolve result (offset, fixup) = do
      (patches, relocations) <- result
      if isLocalPatch globals definitions (laidRole section) fixup
        then case Map.lookup (fixupTarget fixup) definitions of
          Nothing -> Left (ObjectMissingSymbol (fixupTarget fixup))
          Just (_, targetOffset) -> do
            patched <- patchLocal offset targetOffset fixup
            pure ((offset, patched) : patches, relocations)
        else case Map.lookup (fixupTarget fixup) table of
          Nothing -> Left (ObjectMissingSymbol (fixupTarget fixup))
          Just index ->
            pure
              ( patches,
                Relocation offset (fixupKind fixup) index (fixupAddend fixup) : relocations
              )

canResolve :: FixupKind -> Bool
canResolve kind =
  case kind of
    Arm64Branch26 -> True
    Arm64Branch19 -> True
    Arm64Adr21 -> True
    X86Pc32 -> True
    X86Plt32 -> True
    _ -> False

patchLocal :: Word64 -> Word64 -> Fixup -> Either ObjectError Word32
patchLocal offset target fixup =
  case fixupKind fixup of
    Arm64Branch26 ->
      if displacement `mod` 4 /= 0 || not (fitsSigned 28 displacement)
        then Left (ObjectDisplacementOutOfRange (fixupTarget fixup))
        else pure (instruction .|. fromIntegral ((displacement `shiftR` 2) .&. 0x03ffffff))
    Arm64Branch19 ->
      if displacement `mod` 4 /= 0 || not (fitsSigned 21 displacement)
        then Left (ObjectDisplacementOutOfRange (fixupTarget fixup))
        else pure (instruction .|. fromIntegral (((displacement `shiftR` 2) .&. 0x7ffff) `shiftL` 5))
    Arm64Adr21 ->
      if not (fitsSigned 21 displacement)
        then Left (ObjectDisplacementOutOfRange (fixupTarget fixup))
        else
          let immediate = displacement .&. 0x1fffff
              low = fromIntegral ((immediate .&. 3) `shiftL` 29)
              high = fromIntegral (((immediate `shiftR` 2) .&. 0x7ffff) `shiftL` 5)
           in pure (instruction .|. low .|. high)
    X86Pc32 -> patchX86
    X86Plt32 -> patchX86
    kind -> Left (ObjectInvalidFixup kind)
  where
    instruction = fromIntegral (fixupWord fixup) :: Word32
    displacement = signedDifference target offset + fixupAddend fixup
    patchX86 =
      if fitsSigned 32 displacement
        then pure (fromIntegral displacement)
        else Left (ObjectDisplacementOutOfRange (fixupTarget fixup))

applyPatches :: ByteString -> [(Word64, Word32)] -> Either ObjectError BL.ByteString
applyPatches bytes = fmap Builder.toLazyByteString . go 0
  where
    size = BS.length bytes
    go start patches =
      case patches of
        [] -> pure (Builder.byteString (BS.drop start bytes))
        (offset, value) : rest -> do
          let index = fromIntegral offset
          if index < start || index + 4 > size
            then Left (ObjectSizeOverflow "fixup offset")
            else do
              suffix <- go (index + 4) rest
              pure (Builder.byteString (BS.take (index - start) (BS.drop start bytes)) <> Builder.word32LE value <> suffix)

signedDifference :: Word64 -> Word64 -> Int64
signedDifference left right = fromIntegral left - fromIntegral right

fitsSigned :: Int -> Int64 -> Bool
fitsSigned bits value = value >= negate (1 `shiftL` (bits - 1)) && value < (1 `shiftL` (bits - 1))
