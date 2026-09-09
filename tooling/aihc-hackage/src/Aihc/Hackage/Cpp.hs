{-# LANGUAGE OverloadedStrings #-}

-- | Shared CPP macro machinery for Haskell source preprocessing.
--
-- Compiler progress tools use this module to set identical CPP preprocessor
-- flags (GHC version macros,
-- @MIN_VERSION_*@ macros, and @-D@\/@-U@ options from @.cabal@ files).
--
-- Every version that a macro reports comes from 'emulatedGhc' or from the
-- versions of the packages the file is compiled against, never from the
-- compiler that built aihc.
module Aihc.Hackage.Cpp
  ( emulatedGhcVersion,
    builtinCppMacros,
    compilerCppHeader,
    cppMacrosFromOptions,
    DependencyVersions,
    minVersionMacroName,
    injectSyntheticCppMacros,
  )
where

import Aihc.Hackage.Release (GhcRelease (..), emulatedGhc, releaseVersionText)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

-- | The GHC version aihc presents to packages.
--
-- Both the CPP macros and the @impl(ghc ...)@ conditions in @.cabal@ files
-- must agree on this, or a package can take one branch in its source and the
-- other in its @build-depends@.
emulatedGhcVersion :: [Int]
emulatedGhcVersion = [9, 6, 7]

-- | GHC version macros that every preprocessed file sees.
-- Mirrors what GHC itself defines when invoking @cpp@.
builtinCppMacros :: Map Text Text
builtinCppMacros =
  M.fromList
    [ ("__GLASGOW_HASKELL__", T.pack (show (major * 100 + minor))),
      ("__GLASGOW_HASKELL_FULL_VERSION__", T.pack (show (releaseVersionText emulatedGhc))),
      ("__GLASGOW_HASKELL_PATCHLEVEL1__", T.pack (show patch1)),
      ("__GLASGOW_HASKELL_PATCHLEVEL2__", T.pack (show patch2))
    ]
    `M.union` M.restrictKeys machineCppMacros (S.fromList ["WORD_SIZE_IN_BITS", "WORD_SIZE_IN_BITS_FLOAT", "SIZEOF_HSWORD", "SIZEOF_HSDOUBLE", "SIZEOF_HSFLOAT"])
  where
    (major, minor, patch1, patch2) = compilerVersionComponents

-- | Compiler headers for Haskell source use the same 64-bit model as CPP macros.
-- Native C compilation uses its separate target ABI headers.
compilerCppHeader :: FilePath -> Maybe Text
compilerCppHeader path = case path of
  -- Haskell source does not receive host configuration features.
  "ghcautoconf.h" -> Just (header "GHCAUTOCONF_H" [] [])
  "MachDeps.h" -> Just (header "MACHDEPS_H" ["#include \"ghcplatform.h\""] (M.toList machineCppMacros))
  "ghcplatform.h" ->
    Just
      ( header
          "GHCPLATFORM_H"
          []
          [ ("SIZEOF_VOID_P", wordBytes),
            ("SIZEOF_UNSIGNED_LONG", wordBytes),
            ("SIZEOF_UNSIGNED_LONG_LONG", "8")
          ]
      )
  _ -> Nothing
  where
    wordBytes = T.pack (show haskellWordBytes)
    header guard includes definitions =
      T.unlines
        (["#ifndef " <> guard, "#define " <> guard] <> includes <> map define definitions <> ["#endif"])
    define (name, value) = "#define " <> name <> " " <> value

haskellWordBytes :: Int
haskellWordBytes = 8

machineCppMacros :: Map Text Text
machineCppMacros =
  M.fromList
    ( [("WORD_SIZE_IN_BITS", bits), ("WORD_SIZE_IN_BITS_FLOAT", bits <> ".0")]
        <> concatMap
          sizeAndAlignment
          [ ("HSCHAR", 4),
            ("HSINT", haskellWordBytes),
            ("HSWORD", haskellWordBytes),
            ("HSFLOAT", 4),
            ("HSDOUBLE", 8),
            ("WORD16", 2),
            ("WORD32", 4),
            ("WORD64", 8)
          ]
    )
  where
    bits = T.pack (show (haskellWordBytes * 8))
    sizeAndAlignment (name, bytes) =
      [("SIZEOF_" <> name, T.pack (show bytes)), ("ALIGNMENT_" <> name, T.pack (show bytes))]

-- | The four components GHC exposes through its version macros.
compilerVersionComponents :: (Int, Int, Int, Int)
compilerVersionComponents =
  case releaseCompilerVersion emulatedGhc of
    [a, b] -> (a, b, 0, 0)
    [a, b, c] -> (a, b, c, 0)
    a : b : c : d : _ -> (a, b, c, d)
    _ -> error "emulatedGhc must have at least a major and a minor version"

-- | Build the macro map for the CPP config from a list of @cpp-options@ strings.
--
-- Starts from 'builtinCppMacros' and applies any @-D@ and @-U@ flags found
-- in the option list.
cppMacrosFromOptions :: [String] -> Map Text Text
cppMacrosFromOptions cppOptions =
  foldl apply builtinCppMacros (mapMaybe parseCppMacroOption cppOptions)
  where
    apply macros option =
      case option of
        CppDefine name value -> M.insert name value macros
        CppUndef name -> M.delete name macros

-- | The resolved version of every package a file may depend on, keyed by the
-- name the file's @build-depends@ uses.
type DependencyVersions = Map Text [Int]

-- | The @MIN_VERSION_<pkg>@ macro name for a package.
minVersionMacroName :: Text -> Text
minVersionMacroName pkg = "MIN_VERSION_" <> sanitizePkgName pkg

sanitizePkgName :: Text -> Text
sanitizePkgName = T.map sanitizePkgChar
  where
    sanitizePkgChar '-' = '_'
    sanitizePkgChar c = c

-- | Prepend synthetic @#define@ lines for the @MIN_VERSION_*@ and
-- @VERSION_*@ macros of the dependencies to the source, skipping any names
-- that are already explicitly @-D@\/@-U@'d.
--
-- This is what Cabal's autogenerated @cabal_macros.h@ provides. A dependency
-- with a known version gets the same comparison Cabal generates; a
-- dependency whose version is unknown is treated as newer than anything the
-- file could ask for, which is the only answer that keeps the file
-- compiling.
--
-- The header is followed by a @#line 1@ directive naming the file, so that
-- the lines the preprocessor reports are the lines of the original source
-- rather than the lines of the source plus this header.
injectSyntheticCppMacros :: FilePath -> [String] -> DependencyVersions -> [Text] -> Text -> Text
injectSyntheticCppMacros path cppOptions versions dependencies source =
  let existingFromOptions = cppDefinedOrUndefinedFromOptions cppOptions
      shouldDefine name = not (name `S.member` existingFromOptions)
      compilerVersion = releaseCompilerVersion emulatedGhc
      compilerMacroLines =
        [ minVersionDefine "MIN_VERSION_ghc" compilerVersion
        | shouldDefine "MIN_VERSION_ghc"
        ]
          ++ [ "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) " <> atLeast ["ma", "mi", "pl1", "pl2"] compilerVersion
             | shouldDefine "MIN_VERSION_GLASGOW_HASKELL"
             ]
      dependencyLines = concatMap dependencyMacros (S.toAscList (S.fromList dependencies))
      dependencyMacros pkg =
        let name = minVersionMacroName pkg
            versionName = "VERSION_" <> sanitizePkgName pkg
         in if pkg == "ghc" || not (shouldDefine name)
              then []
              else case M.lookup pkg versions of
                Just version ->
                  [ "#define " <> versionName <> " " <> T.pack (show (intercalate "." (map show version)))
                  | shouldDefine versionName
                  ]
                    ++ [minVersionDefine name version]
                Nothing -> ["#define " <> name <> "(major1,major2,minor) 1"]
      macroLines = compilerMacroLines ++ dependencyLines
      header =
        if null macroLines
          then ""
          else T.unlines (macroLines ++ [resetLineDirective path])
   in if T.null header then source else header <> source

-- | The @#line@ directive that makes the next line count as line 1 of @path@.
resetLineDirective :: FilePath -> Text
resetLineDirective path = "#line 1 \"" <> escapeLineDirectivePath (T.pack path) <> "\""

-- | Escape the characters a @#line@ directive's quoted path cannot carry
-- literally.
escapeLineDirectivePath :: Text -> Text
escapeLineDirectivePath = T.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = T.singleton c

minVersionDefine :: Text -> [Int] -> Text
minVersionDefine name version =
  "#define " <> name <> "(major1,major2,minor) " <> atLeast ["major1", "major2", "minor"] version

-- | The condition @(arguments) <= version@ in lexicographic order, written
-- the way @cabal_macros.h@ writes it. Missing components of the version
-- count as zero.
atLeast :: [Text] -> [Int] -> Text
atLeast arguments version =
  "(" <> T.intercalate " || " (zipWith clause [1 ..] pairs) <> ")"
  where
    padded = take (length arguments) (version ++ repeat 0)
    pairs = zip arguments padded
    clause n (argument, component) =
      let prefix = take (n - 1) pairs
          equalities = ["(" <> a <> ") == " <> T.pack (show c) | (a, c) <- prefix]
          comparison = "(" <> argument <> ") " <> (if n == length arguments then "<= " else "< ") <> T.pack (show component)
       in T.intercalate " && " (equalities ++ [comparison])

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

data CppMacroOption
  = CppDefine Text Text
  | CppUndef Text

parseCppMacroOption :: String -> Maybe CppMacroOption
parseCppMacroOption raw =
  let opt = T.strip (stripWrappingQuotes (T.pack raw))
   in case T.stripPrefix "-D" opt of
        Just rest ->
          case T.breakOn "=" rest of
            (name, "") | validMacroName name -> Just (CppDefine name "1")
            (name, value) | validMacroName name -> Just (CppDefine name (T.drop 1 value))
            _ -> Nothing
        Nothing ->
          case T.stripPrefix "-U" opt of
            Just name | validMacroName name -> Just (CppUndef name)
            _ -> Nothing

validMacroName :: Text -> Bool
validMacroName = not . T.null . T.strip

stripWrappingQuotes :: Text -> Text
stripWrappingQuotes txt =
  if T.length txt >= 2 && T.head txt == '"' && T.last txt == '"'
    then T.dropEnd 1 (T.drop 1 txt)
    else txt

cppDefinedOrUndefinedFromOptions :: [String] -> Set Text
cppDefinedOrUndefinedFromOptions =
  foldl addName S.empty . mapMaybe parseCppMacroOption
  where
    addName acc option =
      case option of
        CppDefine name _ -> S.insert name acc
        CppUndef name -> S.insert name acc
