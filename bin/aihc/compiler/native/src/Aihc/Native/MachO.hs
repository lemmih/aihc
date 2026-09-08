{-# LANGUAGE OverloadedStrings #-}

-- | Write ARM64 Mach-O relocatable objects.
module Aihc.Native.MachO
  ( writeArm64MachO,
  )
where

import Aihc.Native.Object
import Control.Monad (replicateM_, zipWithM_)
import Data.Binary.Get (getWord64le, runGet)
import Data.Binary.Put
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (mapAccumL, sortOn)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text.Encoding qualified as Text
import Data.Word (Word32, Word64)

writeArm64MachO :: Image -> Either ObjectError BL.ByteString
writeArm64MachO image = do
  sectionDescriptions <- mapM describeSection (imageSections image)
  let sectionCount = length sectionDescriptions
      segmentCommandSize = 72 + 80 * sectionCount
      commandsSize = segmentCommandSize + 24 + 24 + 80
      headerSize = 32 + commandsSize
      (contentEnd, placedSections) = mapAccumL placeSection (fromIntegral headerSize, 0) sectionDescriptions
      relocationStart = alignUp 8 (fst contentEnd)
  (relocationEnd, sectionsWithRelocations) <- placeRelocations relocationStart placedSections
  let localTargets =
        IntMap.fromList
          [ (index, (fromIntegral ordinal, placedAddress section + symbolOffset symbol))
          | (index, symbol) <- zip [0 ..] (imageSymbols image),
            not (symbolGlobal symbol),
            Just role <- [symbolSection symbol],
            Just (ordinal, section) <- [findSection role placedSections]
          ]
      requiredSymbols =
        IntSet.fromList
          [ relocationSymbol relocation
          | section <- imageSections image,
            relocation <- imageSectionRelocations section,
            isNothing (localRelocationTarget localTargets relocation)
          ]
      placedSymbols =
        filter
          (\(index, symbol) -> symbolGlobal symbol || index `IntSet.member` requiredSymbols)
          (orderSymbols (imageSymbols image))
      orderedSymbols = map snd placedSymbols
      symbolIndexes = IntMap.fromList [(source, index) | (index, (source, _)) <- zip [0 :: Word32 ..] placedSymbols]
      symbolTableOffset = alignUp 8 relocationEnd
      (stringOffsets, stringTable) = buildStringTable orderedSymbols
      stringOffset = symbolTableOffset + fromIntegral (length orderedSymbols * 16)
      stringSize = BS.length stringTable
      locals = length (filter (not . symbolGlobal) orderedSymbols)
      definitions = length (filter isExternalDefinition orderedSymbols)
      undefinedCount = length orderedSymbols - locals - definitions
      segmentFileOffset = case sectionsWithRelocations of
        [] -> 0
        section : _ -> placedFileOffset section
      segmentFileSize = fst contentEnd - segmentFileOffset
      segmentVmSize = snd contentEnd
  pure . runPut $ do
    putHeader commandsSize
    putSegment segmentCommandSize segmentFileOffset segmentFileSize segmentVmSize sectionsWithRelocations
    putBuildVersion
    putSymbolCommand symbolTableOffset (length orderedSymbols) stringOffset stringSize
    putDynamicSymbolCommand locals definitions undefinedCount
    _ <- putSectionContents localTargets (fromIntegral headerSize) sectionsWithRelocations
    putPadding (relocationStart - fst contentEnd)
    mapM_ (putSectionRelocations localTargets symbolIndexes) sectionsWithRelocations
    putPadding (symbolTableOffset - relocationEnd)
    zipWithM_ (putSymbol placedSections) stringOffsets orderedSymbols
    putByteString stringTable

data SectionDescription = SectionDescription
  { descriptionImageSection :: !ImageSection,
    descriptionSegmentName :: !ByteString,
    descriptionSectionName :: !ByteString,
    descriptionFlags :: !Word32
  }

data PlacedSection = PlacedSection
  { placedDescription :: !SectionDescription,
    placedFileOffset :: !Word64,
    placedAddress :: !Word64,
    placedRelocationOffset :: !Word64
  }

placedImageSection :: PlacedSection -> ImageSection
placedImageSection = descriptionImageSection . placedDescription

describeSection :: ImageSection -> Either ObjectError SectionDescription
describeSection section =
  case imageSectionRole section of
    TextSection -> description "__TEXT" "__text" 0x80000400
    TextConstantsSection -> description "__TEXT" "__const" 0
    ReadOnlySection -> description "__DATA" "__const" 0
    DataSection -> description "__DATA" "__data" 0
    NoExecuteStackSection -> Left (ObjectInvalidInput "Mach-O stack section")
  where
    description segment name flags = pure (SectionDescription section segment name flags)

placeSection :: (Word64, Word64) -> SectionDescription -> ((Word64, Word64), PlacedSection)
placeSection (fileOffset, address) description =
  let section = descriptionImageSection description
      alignment = 1 `shiftL` imageSectionAlignment section
      placedFile = alignUp alignment fileOffset
      placedAddress' = alignUp alignment address
      size = fromIntegral (BL.length (imageSectionBytes section))
   in ( (placedFile + size, placedAddress' + size),
        PlacedSection description placedFile placedAddress' 0
      )

placeRelocations :: Word64 -> [PlacedSection] -> Either ObjectError (Word64, [PlacedSection])
placeRelocations = mapAccumM place
  where
    place offset section = do
      count <- relocationRecordCount (imageSectionRelocations (placedImageSection section))
      let relocationOffset = if count == 0 then 0 else offset
      pure (offset + fromIntegral (count * 8), section {placedRelocationOffset = relocationOffset})

relocationRecordCount :: [Relocation] -> Either ObjectError Int
relocationRecordCount = fmap sum . mapM count
  where
    count relocation =
      case relocationKind relocation of
        Absolute64 -> pure 1
        Arm64Branch26 -> pure (if relocationAddend relocation == 0 then 1 else 2)
        Arm64Page21 -> pure (if relocationAddend relocation == 0 then 1 else 2)
        Arm64PageOffset12 -> pure (if relocationAddend relocation == 0 then 1 else 2)
        kind -> Left (ObjectInvalidFixup kind)

putHeader :: Int -> Put
putHeader commandsSize = do
  putWord32le 0xfeedfacf
  putWord32le 0x0100000c
  putWord32le 0
  putWord32le 1
  putWord32le 4
  putWord32le (fromIntegral commandsSize)
  putWord32le 0
  putWord32le 0

putSegment :: Int -> Word64 -> Word64 -> Word64 -> [PlacedSection] -> Put
putSegment commandSize fileOffset fileSize vmSize sections = do
  putWord32le 0x19
  putWord32le (fromIntegral commandSize)
  putFixedName ""
  putWord64le 0
  putWord64le vmSize
  putWord64le fileOffset
  putWord64le fileSize
  putWord32le 7
  putWord32le 7
  putWord32le (fromIntegral (length sections))
  putWord32le 0
  mapM_ putSection sections

putSection :: PlacedSection -> Put
putSection section = do
  let description = placedDescription section
      imageSection = descriptionImageSection description
      relocations = imageSectionRelocations imageSection
  putFixedName (descriptionSectionName description)
  putFixedName (descriptionSegmentName description)
  putWord64le (placedAddress section)
  putWord64le (fromIntegral (BL.length (imageSectionBytes imageSection)))
  putWord32le (fromIntegral (placedFileOffset section))
  putWord32le (fromIntegral (imageSectionAlignment imageSection))
  putWord32le (fromIntegral (placedRelocationOffset section))
  putWord32le (fromIntegral (sum (map relocationEntries relocations) :: Int))
  putWord32le (descriptionFlags description)
  putWord32le 0
  putWord32le 0
  putWord32le 0
  where
    relocationEntries relocation
      | relocationKind relocation `elem` [Arm64Branch26, Arm64Page21, Arm64PageOffset12], relocationAddend relocation /= 0 = 2
      | otherwise = 1

putBuildVersion :: Put
putBuildVersion = do
  putWord32le 0x32
  putWord32le 24
  putWord32le 1
  putWord32le 0x000b0000
  putWord32le 0
  putWord32le 0

putSymbolCommand :: Word64 -> Int -> Word64 -> Int -> Put
putSymbolCommand symbolOffset symbolCount stringOffset stringSize = do
  putWord32le 0x2
  putWord32le 24
  putWord32le (fromIntegral symbolOffset)
  putWord32le (fromIntegral symbolCount)
  putWord32le (fromIntegral stringOffset)
  putWord32le (fromIntegral stringSize)

putDynamicSymbolCommand :: Int -> Int -> Int -> Put
putDynamicSymbolCommand locals definitions undefinedCount = do
  putWord32le 0xb
  putWord32le 80
  putWord32le 0
  putWord32le (fromIntegral locals)
  putWord32le (fromIntegral locals)
  putWord32le (fromIntegral definitions)
  putWord32le (fromIntegral (locals + definitions))
  putWord32le (fromIntegral undefinedCount)
  replicateM_ 12 (putWord32le 0)

-- | A local absolute address needs a section ordinal, not a symbol index.
-- Instruction relocations still need their target symbols.
localRelocationTarget :: IntMap (Word32, Word64) -> Relocation -> Maybe (Word32, Word64)
localRelocationTarget targets relocation
  | relocationKind relocation == Absolute64 = IntMap.lookup (relocationSymbol relocation) targets
  | otherwise = Nothing

putSectionRelocations :: IntMap (Word32, Word64) -> IntMap Word32 -> PlacedSection -> Put
putSectionRelocations localTargets indexes section =
  mapM_ putRelocation (sortOn (Down . relocationOffset) (imageSectionRelocations (placedImageSection section)))
  where
    putRelocation relocation = do
      let symbolIndex = indexes IntMap.! relocationSymbol relocation
          addend = relocationAddend relocation
      case relocationKind relocation of
        Absolute64 -> case localRelocationTarget localTargets relocation of
          Just (ordinal, _) -> putRecord relocation ordinal False False 3 0
          Nothing -> putRecord relocation symbolIndex True False 3 0
        Arm64Branch26 -> putArmInstruction relocation symbolIndex True 2 2 addend
        Arm64Page21 -> putArmInstruction relocation symbolIndex True 2 3 addend
        Arm64PageOffset12 -> putArmInstruction relocation symbolIndex False 2 4 addend
        _ -> pure ()
    putArmInstruction :: Relocation -> Word32 -> Bool -> Word32 -> Word32 -> Int64 -> Put
    putArmInstruction relocation symbolIndex pcRelative lengthValue typeValue addend = do
      if addend == 0
        then pure ()
        else putRecord relocation (fromIntegral addend .&. 0x00ffffff) True False 2 10
      putRecord relocation symbolIndex True pcRelative lengthValue typeValue
    putRecord :: Relocation -> Word32 -> Bool -> Bool -> Word32 -> Word32 -> Put
    putRecord relocation symbolIndex external pcRelative lengthValue typeValue = do
      putWord32le (fromIntegral (relocationOffset relocation))
      putWord32le
        ( (symbolIndex .&. 0x00ffffff)
            .|. (if pcRelative then 1 `shiftL` 24 else 0)
            .|. lengthValue `shiftL` 25
            .|. (if external then 1 `shiftL` 27 else 0)
            .|. typeValue `shiftL` 28
        )

putSymbol :: [PlacedSection] -> Word32 -> Symbol -> Put
putSymbol sections stringOffset symbol = do
  putWord32le stringOffset
  case symbolSection symbol of
    Nothing -> do
      putWord8 0x01
      putWord8 0
      putWord16le 0
      putWord64le 0
    Just role -> do
      case findSection role sections of
        Just (ordinal, section) -> do
          putWord8 (if symbolGlobal symbol then 0x0f else 0x0e)
          putWord8 (fromIntegral ordinal)
          putWord16le 0
          putWord64le (placedAddress section + symbolOffset symbol)
        Nothing -> do
          putWord8 0x01
          putWord8 0
          putWord16le 0
          putWord64le 0

findSection :: SectionRole -> [PlacedSection] -> Maybe (Int, PlacedSection)
findSection role sections =
  case [ (index, section)
       | (index, section) <- zip [1 :: Int ..] sections,
         imageSectionRole (placedImageSection section) == role
       ] of
    value : _ -> Just value
    [] -> Nothing

-- | Mach-O wants the local symbols, then the external definitions, then the
-- undefined ones, each group in name order. 'imageSymbols' already ascends by
-- name, so a partition keeps every group in order. Each symbol carries the
-- position it had, which is what a relocation names.
orderSymbols :: [Symbol] -> [(Int, Symbol)]
orderSymbols symbols =
  filter (not . symbolGlobal . snd) placed
    <> filter (isExternalDefinition . snd) placed
    <> filter ((== Nothing) . symbolSection . snd) placed
  where
    placed = zip [0 ..] symbols

isExternalDefinition :: Symbol -> Bool
isExternalDefinition symbol = symbolGlobal symbol && isJust (symbolSection symbol)

-- | The string table, and the offset of the name of each symbol in the order
-- the symbols are written.
buildStringTable :: [Symbol] -> ([Word32], ByteString)
buildStringTable symbols =
  let (size, entries) = mapAccumL add 1 symbols
      table = BS.cons 0 (BS.concat [bytes <> BS.singleton 0 | (_, bytes) <- entries])
      paddedSize = fromIntegral (alignUp 4 (fromIntegral size))
   in (map fst entries, table <> BS.replicate (paddedSize - BS.length table) 0)
  where
    add offset symbol =
      let bytes = Text.encodeUtf8 (symbolName symbol)
       in (offset + BS.length bytes + 1, (fromIntegral offset, bytes))

putFixedName :: ByteString -> Put
putFixedName name = putByteString (BS.take 16 name) >> replicateM_ (16 - min 16 (BS.length name)) (putWord8 0)

putSectionContents :: IntMap (Word32, Word64) -> Word64 -> [PlacedSection] -> PutM Word64
putSectionContents localTargets offset sections =
  case sections of
    [] -> pure offset
    section : rest -> do
      putPadding (placedFileOffset section - offset)
      let imageSection = placedImageSection section
          bytes = imageSectionBytes imageSection
          next = placedFileOffset section + fromIntegral (BL.length bytes)
          patches =
            sortOn
              fst
              [ (relocationOffset relocation, address)
              | relocation <- imageSectionRelocations imageSection,
                Just (_, address) <- [localRelocationTarget localTargets relocation]
              ]
      putLocalAddresses 0 bytes patches
      putSectionContents localTargets next rest

-- | A section relocation stores the original target address plus its addend.
-- The linker adjusts this address when it places the target section.
putLocalAddresses :: Word64 -> BL.ByteString -> [(Word64, Word64)] -> Put
putLocalAddresses start bytes patches =
  case patches of
    [] -> putLazyByteString bytes
    (offset, address) : rest -> do
      let (prefix, suffix) = BL.splitAt (fromIntegral (offset - start)) bytes
      putLazyByteString prefix
      putWord64le (address + runGet getWord64le suffix)
      putLocalAddresses (offset + 8) (BL.drop 8 suffix) rest

putPadding :: Word64 -> Put
putPadding count = replicateM_ (fromIntegral count) (putWord8 0)

alignUp :: Word64 -> Word64 -> Word64
alignUp alignment value = (value + alignment - 1) .&. complementMask
  where
    complementMask = maxBound - (alignment - 1)

mapAccumM :: (state -> value -> Either error (state, result)) -> state -> [value] -> Either error (state, [result])
mapAccumM function = go
  where
    go state values =
      case values of
        [] -> pure (state, [])
        value : rest -> do
          (next, result) <- function state value
          (final, results) <- go next rest
          pure (final, result : results)
