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

-- | A boot library of the emulated release that aihc provides through a
-- standin under @core-libs@.
--
-- Only the libraries aihc actually ships belong here. A Hackage package that
-- depends on anything else, @deepseq@ or @filepath@ say, gets it from
-- Hackage like any other dependency.
data BootLibrary = BootLibrary
  { -- | The package name as Hackage packages spell it in @build-depends@.
    bootLibraryName :: String,
    -- | The aihc package under @core-libs@ that stands in for it.
    bootLibraryStandin :: String,
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
        [ BootLibrary "base" "aihc-base" [4, 21, 2, 0],
          BootLibrary "ghc-internal" "aihc-internal" [9, 1204, 0],
          BootLibrary "ghc-prim" "aihc-prim" [0, 13, 0],
          BootLibrary "system-cxx-std-lib" "system-cxx-std-lib" [1, 0],
          BootLibrary "template-haskell" "aihc-template-haskell" [2, 23, 0, 0]
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
  case [library | library <- releaseBootLibraries release, bootLibraryStandin library == standin] of
    library : _ -> Just library
    [] -> Nothing

-- | Every provided boot library with its version, keyed by Hackage name.
bootLibraryVersions :: GhcRelease -> [(String, [Int])]
bootLibraryVersions release =
  [(bootLibraryName library, bootLibraryVersion library) | library <- releaseBootLibraries release]
