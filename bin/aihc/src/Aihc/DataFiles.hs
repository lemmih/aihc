-- | Access to the package data files, insulated from @Paths_aihc@.
--
-- Cabal bakes the configured @datadir@ into the generated @Paths_aihc@, so that
-- module's interface changes whenever the install prefix does. The package
-- derivation and the check derivation that reuses its intermediates have
-- different store paths, so importing @Paths_aihc@ directly makes every
-- transitive importer recompile in the check derivation even though nothing in
-- them changed.
--
-- Re-exporting through a @NOINLINE@ binding keeps the store path out of this
-- module's interface, so the recompilation stops here. The pragma is what makes
-- that work: without it GHC exposes an unfolding that carries the path along,
-- the ABI hash moves with the prefix, and the cascade continues.
module Aihc.DataFiles
  ( getDataFileName,
  )
where

import Paths_aihc qualified

-- | Resolve a path under the package data directory.
--
-- The @aihc_datadir@ environment variable overrides the baked-in location, which
-- is how @cabal test@ points the suites at the in-tree runtime sources.
getDataFileName :: FilePath -> IO FilePath
getDataFileName = Paths_aihc.getDataFileName
{-# NOINLINE getDataFileName #-}
