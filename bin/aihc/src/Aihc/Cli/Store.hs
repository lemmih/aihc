-- | Filesystem layout shared by installed runtimes, libraries, and the
-- compiler that consumes them.
module Aihc.Cli.Store
  ( defaultStoreRoot,
    installedEntryArchivePath,
    installedRuntimeArchivePath,
  )
where

import Aihc.Native (NativeTarget, RuntimeGarbageCollector (..), nativeTargetStoreDirectory)
import System.Directory (XdgDirectory (XdgCache), getXdgDirectory)
import System.FilePath ((</>))

defaultStoreRoot :: IO FilePath
defaultStoreRoot = do
  cacheDirectory <- getXdgDirectory XdgCache "aihc"
  pure (cacheDirectory </> "store")

installedEntryArchivePath :: FilePath -> NativeTarget -> FilePath
installedEntryArchivePath storeRoot target =
  storeRoot
    </> "targets"
    </> nativeTargetStoreDirectory target
    </> "entry.a"

installedRuntimeArchivePath :: FilePath -> NativeTarget -> RuntimeGarbageCollector -> FilePath
installedRuntimeArchivePath storeRoot target garbageCollector =
  storeRoot
    </> "runtimes"
    </> nativeTargetStoreDirectory target
    </> renderGarbageCollector garbageCollector
    </> "runtime.a"

renderGarbageCollector :: RuntimeGarbageCollector -> FilePath
renderGarbageCollector garbageCollector =
  case garbageCollector of
    RuntimeGcSemispace -> "semispace"
