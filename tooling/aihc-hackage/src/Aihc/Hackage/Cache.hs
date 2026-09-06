-- | XDG cache layout for downloaded Hackage packages.
module Aihc.Hackage.Cache
  ( getHackageCacheDir,
    hackageIndexCacheFile,
  )
where

import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    getXdgDirectory,
  )
import System.FilePath ((</>))

-- | XDG cache directory for downloaded Hackage packages.
--
-- @~\/.cache\/aihc\/hackage@
getHackageCacheDir :: IO FilePath
getHackageCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")

-- | Cache file for Hackage's package index.
--
-- @~\/.cache\/aihc\/hackage\/01-index.tar.gz@
hackageIndexCacheFile :: IO FilePath
hackageIndexCacheFile = do
  dir <- getHackageCacheDir
  createDirectoryIfMissing True dir
  pure (dir </> "01-index.tar.gz")
