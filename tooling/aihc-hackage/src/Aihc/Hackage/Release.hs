-- | The GHC release aihc pretends to be.
--
-- Hackage packages observe the compiler version in several places: the
-- @__GLASGOW_HASKELL__@ family of CPP macros, the @MIN_VERSION_*@ macros for
-- their dependencies, the @impl(ghc ...)@ conditions in their @.cabal@ files,
-- and the versions of the boot libraries they are compiled against. A package
-- that sees a different answer in two of those places takes two different
-- branches at once, so every one of them must read this one record.
--
-- The version here is independent of the GHC that compiled aihc itself.
module Aihc.Hackage.Release
  ( GhcRelease (..),
    BootLibrary (..),
    emulatedGhc,
    releaseVersionText,
    showVersionBranch,
    lookupBootLibrary,
    lookupBootLibraryByStandin,
    bootLibraryVersions,
  )
where

import Data.List (intercalate)

-- | A boot library that ships with the emulated release.
data BootLibrary = BootLibrary
  { -- | The package name as Hackage packages spell it in @build-depends@.
    bootLibraryName :: String,
    -- | The aihc package under @core-libs@ that stands in for it, when one
    -- exists. Libraries without a standin are still part of the release so
    -- that snapshot constraints of the form @foo installed@ resolve.
    bootLibraryStandin :: Maybe String,
    bootLibraryVersion :: [Int]
  }
  deriving (Eq, Show)

data GhcRelease = GhcRelease
  { releaseCompilerVersion :: [Int],
    releaseBootLibraries :: [BootLibrary]
  }
  deriving (Eq, Show)

-- | aihc presents itself as GHC 9.12.4. The standins under @core-libs@ mirror
-- the boot libraries of that release, and the versions below are the ones
-- that shipped with it.
emulatedGhc :: GhcRelease
emulatedGhc =
  GhcRelease
    { releaseCompilerVersion = [9, 12, 4],
      releaseBootLibraries =
        [ BootLibrary "array" Nothing [0, 5, 8, 0],
          BootLibrary "base" (Just "aihc-base") [4, 21, 2, 0],
          BootLibrary "binary" Nothing [0, 8, 9, 3],
          BootLibrary "bytestring" Nothing [0, 12, 2, 0],
          BootLibrary "Cabal-syntax" Nothing [3, 14, 2, 0],
          BootLibrary "Cabal" Nothing [3, 14, 2, 0],
          BootLibrary "containers" Nothing [0, 7],
          BootLibrary "deepseq" Nothing [1, 5, 1, 0],
          BootLibrary "directory" Nothing [1, 3, 10, 1],
          BootLibrary "exceptions" Nothing [0, 10, 12],
          BootLibrary "filepath" Nothing [1, 5, 5, 0],
          BootLibrary "ghc" Nothing [9, 12, 4],
          BootLibrary "ghc-internal" (Just "aihc-internal") [9, 1204, 0],
          BootLibrary "ghc-prim" (Just "aihc-prim") [0, 13, 0],
          BootLibrary "haskeline" Nothing [0, 8, 4, 1],
          BootLibrary "mtl" Nothing [2, 3, 2],
          BootLibrary "os-string" Nothing [2, 0, 10],
          BootLibrary "parsec" Nothing [3, 1, 18, 0],
          BootLibrary "pretty" Nothing [1, 1, 3, 6],
          BootLibrary "process" Nothing [1, 6, 26, 1],
          BootLibrary "semaphore-compat" Nothing [1, 0, 0],
          BootLibrary "stm" Nothing [2, 5, 3, 1],
          BootLibrary "system-cxx-std-lib" (Just "system-cxx-std-lib") [1, 0],
          BootLibrary "template-haskell" (Just "aihc-template-haskell") [2, 23, 0, 0],
          BootLibrary "terminfo" Nothing [0, 4, 1, 7],
          BootLibrary "text" Nothing [2, 1, 4],
          BootLibrary "time" Nothing [1, 14],
          BootLibrary "transformers" Nothing [0, 6, 3, 0],
          BootLibrary "unix" Nothing [2, 8, 8, 0],
          BootLibrary "xhtml" Nothing [3000, 2, 2, 1]
        ]
    }

showVersionBranch :: [Int] -> String
showVersionBranch = intercalate "." . map show

-- | The compiler version as GHC prints it, for example @9.12.4@.
releaseVersionText :: GhcRelease -> String
releaseVersionText = showVersionBranch . releaseCompilerVersion

lookupBootLibrary :: String -> GhcRelease -> Maybe BootLibrary
lookupBootLibrary name release =
  case [library | library <- releaseBootLibraries release, bootLibraryName library == name] of
    library : _ -> Just library
    [] -> Nothing

lookupBootLibraryByStandin :: String -> GhcRelease -> Maybe BootLibrary
lookupBootLibraryByStandin standin release =
  case [library | library <- releaseBootLibraries release, bootLibraryStandin library == Just standin] of
    library : _ -> Just library
    [] -> Nothing

-- | Every boot library with its version, keyed by Hackage name.
bootLibraryVersions :: GhcRelease -> [(String, [Int])]
bootLibraryVersions release =
  [(bootLibraryName library, bootLibraryVersion library) | library <- releaseBootLibraries release]
