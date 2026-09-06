{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral support shared by backend code generators.
module Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    WasmSysroot (..),
    backendArchiver,
    backendCompiler,
    buildAddrLiteralPool,
    executableEntryName,
    hostNativeTarget,
    nativeTargetTriple,
    nativeTargetStoreDirectory,
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    parseNativeTarget,
    renderLinkedFunctionSymbol,
    renderLinkedConstructorInfoSymbol,
    renderLinkedGlobalSymbol,
    renderNativeTarget,
    runtimePlan,
    supportedNativePrimitiveNames,
    wasmSysroot,
  )
where

import Aihc.Grin.Syntax
import Control.Monad (filterM)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate, intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Paths_aihc (getDataFileName)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Info qualified as System

-- | The fixed linked global that starts each executable.
executableEntryName :: Text
executableEntryName = T.intercalate "\0" ["exe", "Aihc.Entry", "entry"]

-- | A complete backend and executable target.
-- Every target consumes Lir. See @docs/lir.md@.
data NativeTarget
  = AppleArm64
  | LinuxAmd64
  | Llvm
  | Wasm32Wasip3
  deriving (Bounded, Enum, Eq, Ord, Show)

data RuntimeGarbageCollector
  = RuntimeGcSemispace
  deriving (Eq, Ord, Show)

data RuntimePlan = RuntimePlan
  { runtimeSources :: ![FilePath],
    -- | Runtime units written in Lir. Every target compiles them with its own
    -- Lir backend instead of a C compiler. See @docs/lir.md@.
    runtimeLirSources :: ![FilePath],
    runtimeIncludeDirectories :: ![FilePath]
  }
  deriving (Eq, Show)

renderNativeTarget :: NativeTarget -> String
renderNativeTarget target =
  case target of
    AppleArm64 -> "apple-arm64"
    LinuxAmd64 -> "linux-amd64"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip3"

parseNativeTarget :: String -> Either String NativeTarget
parseNativeTarget value =
  case value of
    "apple-arm64" -> Right AppleArm64
    "arm64-apple-darwin" -> Right AppleArm64
    "linux-amd64" -> Right LinuxAmd64
    "x86_64-unknown-linux-gnu" -> Right LinuxAmd64
    "llvm" -> Right Llvm
    "wasm32-wasip3" -> Right Wasm32Wasip3
    "wasip3" -> Right Wasm32Wasip3
    _ -> Left "target must be apple-arm64, linux-amd64, llvm, or wasm32-wasip3"

-- | Render a NUL-separated logical linker identity as a readable, reversible
-- object symbol. ASCII letters and digits stay intact, components use a single
-- underscore separator, and only literal underscores or unsafe UTF-8 bytes
-- are escaped.
renderLinkedFunctionSymbol :: Text -> Text
renderLinkedFunctionSymbol logicalName =
  Text.decodeUtf8 (BL.toStrict (Builder.toLazyByteString rendered))
  where
    rendered =
      case BS.split 0 (Text.encodeUtf8 logicalName) of
        [unstructured] -> Builder.string7 "aihc_entry_" <> renderComponent unstructured
        components -> mconcat (intersperse (Builder.word8 underscore) (map renderComponent components))
    -- Copy the run of bytes that stay intact, then escape the one that stops
    -- it. Almost every name is one such run.
    renderComponent bytes =
      case BS.span asciiAlphaNumeric bytes of
        (intact, rest) ->
          Builder.byteString intact <> case BS.uncons rest of
            Nothing -> mempty
            Just (byte, remaining) -> renderByte byte <> renderComponent remaining
    renderByte byte
      | byte == underscore = Builder.string7 "__u"
      | otherwise = Builder.string7 "__x" <> Builder.word8 (hexDigit (byte `shiftR` 4)) <> Builder.word8 (hexDigit (byte .&. 0x0f))
    hexDigit nibble
      | nibble < 10 = 48 + nibble
      | otherwise = 87 + nibble
    asciiAlphaNumeric byte =
      (byte >= 48 && byte <= 57)
        || (byte >= 65 && byte <= 90)
        || (byte >= 97 && byte <= 122)
    underscore = 95

-- | Render the object symbol for one static Haskell value.
renderLinkedGlobalSymbol :: Text -> Text
renderLinkedGlobalSymbol = renderLinkedFunctionSymbol

-- | Render the object symbol for one constructor application stage.
renderLinkedConstructorInfoSymbol :: Text -> Int -> Text
renderLinkedConstructorInfoSymbol name remaining =
  "aihc_constructor_" <> renderLinkedFunctionSymbol name <> "_" <> T.pack (show remaining)

hostNativeTarget :: Maybe NativeTarget
hostNativeTarget
  | System.os == "darwin" && System.arch `elem` ["aarch64", "arm64"] = Just AppleArm64
  | System.os == "linux" && System.arch == "x86_64" = Just LinuxAmd64
  | otherwise = Nothing

-- | The Clang triple of one target. It selects the C ABI and the libc the
-- objects are compiled against, which is not the same question as the
-- interface a finished program speaks.
--
-- The two differ on WebAssembly, where the triple is one version behind the
-- target name. Preview 3 is not a property of the compilation: the objects
-- are ordinary wasm32 code, and the preview 3 interface comes from the WIT
-- bindings and from the component "wasm-tools" encodes around the linked
-- module. Clang has no preview 3 triple to offer either, and no use for one.
-- What the triple does decide is which libc the runtime agrees with, and the
-- wasi-libc it links was built as @wasm32-wasip1@.
nativeTargetTriple :: NativeTarget -> String
nativeTargetTriple target =
  case target of
    AppleArm64 -> "arm64-apple-darwin"
    LinuxAmd64 -> "x86_64-unknown-linux-gnu"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip1"

-- | Render the stable store directory for one compilation target.
nativeTargetStoreDirectory :: NativeTarget -> FilePath
nativeTargetStoreDirectory target =
  case target of
    AppleArm64 -> "arm64-macos-apple"
    LinuxAmd64 -> "amd64-linux-gnu"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip3"

-- | Select the compiler driver and target arguments.
backendCompiler :: NativeTarget -> IO (FilePath, [String])
backendCompiler target =
  case target of
    Llvm -> pure ("clang", ["-Wno-override-module", "-O2"])
    Wasm32Wasip3 -> do
      compiler <- fromMaybe "clang" <$> lookupEnv "AIHC_WASM_CLANG"
      pure
        ( compiler,
          [ "--target=" <> nativeTargetTriple target,
            "-mtail-call",
            "-mmultivalue",
            "-mreference-types",
            "-msign-ext"
          ]
        )
    AppleArm64 -> nativeCompiler
    LinuxAmd64 -> nativeCompiler
  where
    nativeCompiler = pure ("clang", ["--target=" <> nativeTargetTriple target])

-- | The WASI sysroot that supplies libc to the WebAssembly target. The
-- runtime allocates, copies memory, and aborts through libc like every other
-- target, so a sysroot is required rather than optional.
--
-- The header and archive directories are recorded separately. Their names
-- follow the triple the sysroot was built for, wasi-libc renamed that from
-- @wasm32-wasi@ to @wasm32-wasip1@, and an installation can carry one name
-- for its headers and the other for its archives. Reading both from the
-- directory itself keeps every installation usable without a version test.
data WasmSysroot = WasmSysroot
  { wasmSysrootInclude :: !FilePath,
    wasmSysrootLibc :: !FilePath
  }
  deriving (Eq, Show)

-- | Locate the WASI sysroot. @AIHC_WASM_SYSROOT@ names one directly.
-- Otherwise the well-known installation prefixes are searched, so an
-- ordinary Homebrew or wasi-sdk installation needs no configuration.
wasmSysroot :: IO WasmSysroot
wasmSysroot = do
  override <- lookupEnv "AIHC_WASM_SYSROOT"
  case override of
    Just root -> do
      found <- readWasmSysroot root
      maybe (ioError (userError (missingWasmSysrootMessage (Just root)))) pure found
    Nothing -> do
      found <- traverse readWasmSysroot wasmSysrootCandidates
      case catMaybes found of
        sysroot : _ -> pure sysroot
        [] -> ioError (userError (missingWasmSysrootMessage Nothing))

-- | Read one candidate directory, which is a sysroot when it holds both the
-- headers and the libc archive of a supported target directory.
readWasmSysroot :: FilePath -> IO (Maybe WasmSysroot)
readWasmSysroot root = do
  includes <- filterM (\directory -> doesFileExist (directory </> "stdlib.h")) [root </> "include" </> name | name <- wasmSysrootTargetNames]
  archives <- filterM doesFileExist [root </> "lib" </> name </> "libc.a" | name <- wasmSysrootTargetNames]
  pure $ case (includes, archives) of
    (include : _, archive : _) -> Just WasmSysroot {wasmSysrootInclude = include, wasmSysrootLibc = archive}
    _ -> Nothing

-- | The target directory names wasi-libc has used, newest first.
wasmSysrootTargetNames :: [FilePath]
wasmSysrootTargetNames = ["wasm32-wasip1", "wasm32-wasi"]

-- | The installation prefixes searched when the environment names none.
wasmSysrootCandidates :: [FilePath]
wasmSysrootCandidates =
  [ "/opt/homebrew/opt/wasi-libc/share/wasi-sysroot",
    "/usr/local/opt/wasi-libc/share/wasi-sysroot",
    "/home/linuxbrew/.linuxbrew/opt/wasi-libc/share/wasi-sysroot",
    "/opt/wasi-sdk/share/wasi-sysroot",
    "/usr/local/share/wasi-sysroot",
    "/usr/share/wasi-sysroot"
  ]

missingWasmSysrootMessage :: Maybe FilePath -> String
missingWasmSysrootMessage rejected =
  unlines
    ( introduction
        <> [ "",
             "Install one and, when it is outside a standard prefix, set",
             "AIHC_WASM_SYSROOT to the directory holding include/<target> and",
             "lib/<target>/libc.a:",
             "",
             "  brew install wasi-libc",
             "  https://github.com/WebAssembly/wasi-sdk/releases",
             "",
             "The searched prefixes are:"
           ]
        <> ["  " <> candidate | candidate <- wasmSysrootCandidates]
    )
  where
    introduction =
      case rejected of
        Just root ->
          [ "AIHC_WASM_SYSROOT does not name a WASI sysroot: " <> root,
            "It holds no " <> intercalate " or " [name <> "/libc.a" | name <- wasmSysrootTargetNames] <> " under lib."
          ]
        Nothing -> ["The wasm32-wasip3 target requires a WASI sysroot and none was found."]

-- | Select an archive tool that keeps object files for the selected target.
backendArchiver :: NativeTarget -> IO FilePath
backendArchiver target = do
  override <- lookupEnv "AIHC_LLVM_AR"
  case override of
    Just archiver -> pure archiver
    Nothing -> do
      llvmArchiver <- findExecutable "llvm-ar"
      case llvmArchiver of
        Just archiver -> pure archiver
        Nothing -> do
          archiver <- fromMaybe "ar" <$> findExecutable "ar"
          if System.os == "darwin" && target `elem` [LinuxAmd64, Wasm32Wasip3] && archiver == "/usr/bin/ar"
            then ioError (userError "The selected target requires LLVM ar. Set AIHC_LLVM_AR to its path.")
            else pure archiver

-- | Deduplicate address literals and assign short, unit-local assembly labels.
buildAddrLiteralPool :: GrinProgram -> [(ByteString, Text)]
buildAddrLiteralPool program =
  [ (value, ".Laihc_addr_" <> T.pack (show index))
  | (index, value) <- zip [0 :: Int ..] values
  ]
  where
    values = Set.toAscList (Set.fromList [value | GrinLitAddr value <- grinProgramLiterals program])

runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "compiler/native/runtime/aihc_runtime.c"

runtimePlan :: NativeTarget -> RuntimeGarbageCollector -> IO RuntimePlan
runtimePlan target garbageCollector = do
  core <- runtimeSourcePath
  runtimeOptions <- getDataFileName "compiler/native/runtime/aihc_runtime_options.c"
  collector <-
    getDataFileName $ case garbageCollector of
      RuntimeGcSemispace -> "compiler/native/runtime/aihc_gc_semispace.c"
  host <-
    getDataFileName $ case target of
      Wasm32Wasip3 -> "compiler/native/runtime/aihc_host_wasip3.c"
      _ -> "compiler/native/runtime/aihc_host_posix.c"
  lirUnits <-
    traverse
      (getDataFileName . ("compiler/native/runtime/" <>))
      ["aihc_array.lir", "aihc_byte_array.lir", "aihc_mutvar.lir", "aihc_stable_name.lir"]
  pure
    RuntimePlan
      { runtimeSources = [core, runtimeOptions, collector, host],
        runtimeLirSources = lirUnits,
        runtimeIncludeDirectories = [takeDirectory core]
      }

-- | Primitive operations implemented directly by every native backend or by
-- the shared runtime ABI.
supportedNativePrimitiveNames :: [Text]
supportedNativePrimitiveNames =
  [ "+#",
    "-#",
    "*#",
    "compareInt#",
    "<#",
    "==#",
    "ord#",
    "chr#",
    "addIntC#",
    "subIntC#",
    "plusWord#",
    "minusWord#",
    "timesWord#",
    "addWordC#",
    "subWordC#",
    "timesWord2#",
    "quotWord#",
    "remWord#",
    "quotRemWord#",
    "quotRemWord2#",
    "and#",
    "or#",
    "xor#",
    "not#",
    "uncheckedShiftL#",
    "uncheckedShiftRL#",
    "int2Word#",
    "word2Int#",
    "word8ToWord#",
    "word32ToWord#",
    "word64ToWord#",
    "wordToWord64#",
    "word16ToWord#",
    ">#",
    ">=#",
    "<=#",
    "/=#",
    "eqWord64#",
    "neWord64#",
    "ltWord64#",
    "leWord64#",
    "gtWord64#",
    "geWord64#",
    "eqWord#",
    "neWord#",
    "ltWord#",
    "leWord#",
    "gtWord#",
    "geWord#",
    "nullAddr#",
    "realWorld#",
    "unsafeFreezeArray#",
    "unsafeThawArray#",
    "unsafeFreezeByteArray#",
    "unsafeThawByteArray#",
    "castFloatToWord32#",
    "castWord32ToFloat#",
    "castDoubleToWord64#",
    "castWord64ToDouble#",
    "timesInt2#",
    -- Address arithmetic, address memory access, and C string length.
    "plusAddr#",
    "minusAddr#",
    "eqAddr#",
    "reallyUnsafePtrEquality#",
    "neAddr#",
    "ltAddr#",
    "leAddr#",
    "gtAddr#",
    "geAddr#",
    "addr2Int#",
    "int2Addr#",
    "cstringLength#",
    "touch#",
    "indexWord8OffAddr#",
    "indexWord16OffAddr#",
    "indexWord32OffAddr#",
    "indexWord64OffAddr#",
    "readWord8OffAddr#",
    "readWord16OffAddr#",
    "readWord32OffAddr#",
    "readWord64OffAddr#",
    "writeWord8OffAddr#",
    "writeWord16OffAddr#",
    "writeWord32OffAddr#",
    "writeWord64OffAddr#",
    "indexWord8OffAddrAsWord16#",
    "indexWord8OffAddrAsWord32#",
    "indexWord8OffAddrAsWord64#",
    "readWord8OffAddrAsWord16#",
    "readWord8OffAddrAsWord32#",
    "readWord8OffAddrAsWord64#",
    "writeWord8OffAddrAsWord16#",
    "writeWord8OffAddrAsWord32#",
    "writeWord8OffAddrAsWord64#",
    "indexWord8OffAddrAsFloat#",
    "indexWord8OffAddrAsDouble#",
    "readWord8OffAddrAsFloat#",
    "readWord8OffAddrAsDouble#",
    "writeWord8OffAddrAsFloat#",
    "writeWord8OffAddrAsDouble#",
    -- Sized integer conversions and byte swaps.
    "wordToWord8#",
    "wordToWord16#",
    "wordToWord32#",
    "intToInt8#",
    "int8ToInt#",
    "intToInt16#",
    "int16ToInt#",
    "intToInt32#",
    "int32ToInt#",
    "intToInt64#",
    "int64ToInt#",
    "byteSwap#",
    "byteSwap16#",
    "byteSwap32#",
    "byteSwap64#",
    -- Float and double arithmetic on the IEEE 754 bit patterns.
    "plusFloat#",
    "minusFloat#",
    "timesFloat#",
    "negateFloat#",
    "fabsFloat#",
    "int2Float#",
    "float2Int#",
    "gtFloat#",
    "ltFloat#",
    "eqFloat#",
    "float2Double#",
    "double2Float#",
    "+##",
    "-##",
    "*##",
    "/##",
    "divideFloat#",
    "sqrtDouble#",
    "sqrtFloat#",
    "negateDouble#",
    "fabsDouble#",
    "int2Double#",
    "double2Int#",
    ">##",
    "<##",
    "==##",
    -- The bit counts are Lir operations, so they need no runtime call.
    "clz#",
    "ctz#",
    "popCnt#"
  ]
    <> map fst nativeCpsPrimitiveCalls
    <> map fst nativeRuntimePrimitiveCalls

-- | Control transfer performed after a native CPS runtime call returns.
data NativeCpsTransfer
  = NativeCpsEnterContinuation
  | NativeCpsResumeScheduler
  deriving (Eq, Show)

-- | Architecture-neutral native ABI description for a CPS primitive.
data NativeCpsCall = NativeCpsCall
  { nativeCpsCallSymbol :: !Text,
    nativeCpsCallOperandCount :: !Int,
    nativeCpsCallPassContinuation :: !Bool,
    nativeCpsCallTransfer :: !NativeCpsTransfer
  }
  deriving (Eq, Show)

-- | Architecture-neutral native ABI description for a direct runtime
-- primitive. The machine is an implicit runtime argument rather than a GRIN
-- operand, and the result count describes the logical GRIN result independently
-- of the C function's return type.
data NativeRuntimeCall = NativeRuntimeCall
  { nativeRuntimeCallForeignCall :: !GrinForeignCall,
    nativeRuntimeCallPassMachine :: !Bool,
    nativeRuntimeCallResultCount :: !Int
  }
  deriving (Eq, Show)

nativeCpsPrimitiveCall :: Text -> Maybe NativeCpsCall
nativeCpsPrimitiveCall name = lookup name nativeCpsPrimitiveCalls

nativeCpsPrimitiveCalls :: [(Text, NativeCpsCall)]
nativeCpsPrimitiveCalls =
  [ enters "fork#" "aihc_fork" 1,
    enters "newMVar#" "aihc_mvar_new" 0,
    resumes "readMVar#" "aihc_mvar_read" 1,
    resumes "takeMVar#" "aihc_mvar_take" 1,
    resumes "putMVar#" "aihc_mvar_put" 2,
    resumes "yield#" "aihc_yield" 0,
    resumes "awaitIO#" "aihc_await_io" 1
  ]
  where
    enters primitive symbol operands =
      (primitive, NativeCpsCall symbol operands False NativeCpsEnterContinuation)
    resumes primitive symbol operands =
      (primitive, NativeCpsCall symbol operands True NativeCpsResumeScheduler)

-- | Runtime calls shared by native backends. Representation-preserving
-- primitives such as freeze and thaw deliberately have no entry here.
nativeRuntimePrimitiveCall :: Text -> Maybe NativeRuntimeCall
nativeRuntimePrimitiveCall name = lookup name nativeRuntimePrimitiveCalls

nativeRuntimePrimitiveCalls :: [(Text, NativeRuntimeCall)]
nativeRuntimePrimitiveCalls =
  [ machineCall "newArray#" "aihc_array_new" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "newMutVar#" "aihc_mutvar_new" [GrinForeignWord64] GrinForeignAddr,
    machineCall "makeStableName#" "aihc_stable_name_make" [GrinForeignAddr] GrinForeignAddr,
    call "readMutVar#" "aihc_mutvar_read" [GrinForeignAddr] GrinForeignWord64,
    procedure "writeMutVar#" "aihc_mutvar_write" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    pairCall "casMutVar#" "aihc_mutvar_compare_and_swap" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "sameMutVar#" "aihc_mutvar_same" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "eqStableName#" "aihc_stable_name_equal" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "stableNameToInt#" "aihc_stable_name_hash" [GrinForeignAddr] GrinForeignWord64,
    call "indexArray#" "aihc_array_index" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readArray#" "aihc_array_index" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeArray#" "aihc_array_write" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "sameMutableArray#" "aihc_array_same" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "newByteArray#" "aihc_byte_array_new" [GrinForeignWord64] GrinForeignAddr,
    call "newPinnedByteArray#" "aihc_byte_array_new_pinned" [GrinForeignWord64] GrinForeignAddr,
    call "newAlignedPinnedByteArray#" "aihc_byte_array_new_aligned_pinned" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    call "isMutableByteArrayPinned#" "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64,
    call "isByteArrayPinned#" "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64,
    call "byteArrayContents#" "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr,
    call "mutableByteArrayContents#" "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr,
    procedure "shrinkMutableByteArray#" "aihc_byte_array_shrink" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "resizeMutableByteArray#" "aihc_byte_array_resize" [GrinForeignAddr, GrinForeignWord64] GrinForeignAddr,
    call "sizeofByteArray#" "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64,
    call "getSizeofMutableByteArray#" "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64,
    procedure "copyAddrToByteArray#" "aihc_byte_array_copy_from_addr" [GrinForeignAddr, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "indexWordArray#" "aihc_byte_array_index_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWordArray#" "aihc_byte_array_read_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWordArray#" "aihc_byte_array_write_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyByteArrayToAddr#" "aihc_byte_array_copy_to_addr" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArrayToAddr#" "aihc_byte_array_copy_to_addr" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "compareByteArrays#" "aihc_byte_array_compare" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "expDouble#" "aihc_double_exp" [GrinForeignWord64] GrinForeignWord64,
    call "logDouble#" "aihc_double_log" [GrinForeignWord64] GrinForeignWord64,
    call "sinDouble#" "aihc_double_sin" [GrinForeignWord64] GrinForeignWord64,
    call "cosDouble#" "aihc_double_cos" [GrinForeignWord64] GrinForeignWord64,
    call "tanDouble#" "aihc_double_tan" [GrinForeignWord64] GrinForeignWord64,
    call "asinDouble#" "aihc_double_asin" [GrinForeignWord64] GrinForeignWord64,
    call "acosDouble#" "aihc_double_acos" [GrinForeignWord64] GrinForeignWord64,
    call "atanDouble#" "aihc_double_atan" [GrinForeignWord64] GrinForeignWord64,
    call "sinhDouble#" "aihc_double_sinh" [GrinForeignWord64] GrinForeignWord64,
    call "coshDouble#" "aihc_double_cosh" [GrinForeignWord64] GrinForeignWord64,
    call "tanhDouble#" "aihc_double_tanh" [GrinForeignWord64] GrinForeignWord64,
    call "asinhDouble#" "aihc_double_asinh" [GrinForeignWord64] GrinForeignWord64,
    call "acoshDouble#" "aihc_double_acosh" [GrinForeignWord64] GrinForeignWord64,
    call "atanhDouble#" "aihc_double_atanh" [GrinForeignWord64] GrinForeignWord64,
    call "**##" "aihc_double_pow" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "expFloat#" "aihc_float_exp" [GrinForeignWord64] GrinForeignWord64,
    call "logFloat#" "aihc_float_log" [GrinForeignWord64] GrinForeignWord64,
    call "sinFloat#" "aihc_float_sin" [GrinForeignWord64] GrinForeignWord64,
    call "cosFloat#" "aihc_float_cos" [GrinForeignWord64] GrinForeignWord64,
    call "tanFloat#" "aihc_float_tan" [GrinForeignWord64] GrinForeignWord64,
    call "asinFloat#" "aihc_float_asin" [GrinForeignWord64] GrinForeignWord64,
    call "acosFloat#" "aihc_float_acos" [GrinForeignWord64] GrinForeignWord64,
    call "atanFloat#" "aihc_float_atan" [GrinForeignWord64] GrinForeignWord64,
    call "sinhFloat#" "aihc_float_sinh" [GrinForeignWord64] GrinForeignWord64,
    call "coshFloat#" "aihc_float_cosh" [GrinForeignWord64] GrinForeignWord64,
    call "tanhFloat#" "aihc_float_tanh" [GrinForeignWord64] GrinForeignWord64,
    call "asinhFloat#" "aihc_float_asinh" [GrinForeignWord64] GrinForeignWord64,
    call "acoshFloat#" "aihc_float_acosh" [GrinForeignWord64] GrinForeignWord64,
    call "atanhFloat#" "aihc_float_atanh" [GrinForeignWord64] GrinForeignWord64,
    call "powerFloat#" "aihc_float_pow" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "indexCharArray#" "aihc_byte_array_index_byte_word8" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8ArrayAsWord16#" "aihc_byte_array_index_byte_word16" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8ArrayAsWord32#" "aihc_byte_array_index_byte_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8ArrayAsWord64#" "aihc_byte_array_index_byte_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64
  ]
  where
    call = runtimeCall False 1
    -- casMutVar# returns a failure flag and the final contents. The runtime
    -- function returns only the flag. Runtime calls do not yield, so each
    -- backend reads the final contents with readMutVar# directly after the swap.
    pairCall = runtimeCall False 2
    procedure = runtimeCall False 0
    machineCall = runtimeCall True 1

-- | Describe one runtime call in the shared native ABI.
runtimeCall :: Bool -> Int -> Text -> Text -> [GrinForeignType] -> GrinForeignType -> (Text, NativeRuntimeCall)
runtimeCall passMachine resultCount primitive symbol arguments result =
  ( primitive,
    NativeRuntimeCall
      { nativeRuntimeCallForeignCall =
          GrinForeignCall
            { grinForeignCallName = "$runtime$" <> symbol,
              grinForeignCallSymbol = symbol,
              grinForeignCallTarget = GrinForeignFunction,
              grinForeignCallSignature =
                GrinForeignSignature
                  { grinForeignArgumentTypes = arguments,
                    grinForeignResultType = result,
                    grinForeignEffect = GrinForeignPure
                  }
            },
        nativeRuntimeCallPassMachine = passMachine,
        nativeRuntimeCallResultCount = resultCount
      }
  )
