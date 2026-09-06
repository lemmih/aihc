{-# LANGUAGE ScopedTypeVariables #-}

-- | Stackage snapshot fetching and parsing.
module Aihc.Hackage.Stackage
  ( loadStackageSnapshot,
    parseSnapshotConstraints,
  )
where

import Aihc.Hackage.Cache (snapshotCacheFile)
import Aihc.Hackage.Release (bootLibraryVersions, emulatedGhc, showVersionBranch)
import Aihc.Hackage.Types (PackageSpec (..))
import Control.Exception (SomeException, displayException, try)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Network.HTTP.Client (Manager, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)

-- | Load a Stackage snapshot, either from cache or by fetching it.
--
-- When @offline@ is 'True', only the cache is consulted.  A shared
-- 'Manager' can be provided; if 'Nothing', a fresh one is created.
loadStackageSnapshot :: Maybe Manager -> String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshot mManager snapshot offline = do
  cacheFile <- snapshotCacheFile snapshot
  hasCache <- doesFileExist cacheFile
  if hasCache
    then do
      cachedBody <- readFile cacheFile
      pure (parseSnapshotConstraints cachedBody)
    else
      if offline
        then pure (Left ("Snapshot missing from cache in offline mode: " ++ snapshot))
        else do
          manager <- case mManager of
            Just m -> pure m
            Nothing -> newManager tlsManagerSettings
          let url = "https://www.stackage.org/" ++ snapshot ++ "/cabal.config"
          fetched <- httpGetString manager url
          case fetched of
            Left err -> pure (Left err)
            Right body ->
              case parseSnapshotConstraints body of
                Left parseErr -> pure (Left parseErr)
                Right specs -> do
                  writeFile cacheFile body
                  pure (Right specs)

-- | Parse package constraints from a snapshot's @cabal.config@.
parseSnapshotConstraints :: String -> Either String [PackageSpec]
parseSnapshotConstraints content = do
  let section = constraintLines (lines content)
      entries = map trim (splitComma (concat section))
      specs = mapMaybe parseConstraint entries
  if null specs
    then Left "No package constraints found"
    else Right specs

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

constraintLines :: [String] -> [String]
constraintLines ls =
  case break (isPrefixOf "constraints:" . trimLeft) ls of
    (_, []) -> []
    (_, firstRaw : restRaw) ->
      let firstLine = trimLeft firstRaw
          start = [drop 12 firstLine]
          cont = [trimLeft line | line <- takeWhile isConstraintContinuation restRaw]
       in start <> cont

isConstraintContinuation :: String -> Bool
isConstraintContinuation line =
  case line of
    c : _ -> isSpace c
    [] -> False

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseConstraint :: String -> Maybe PackageSpec
parseConstraint entry
  | null entry = Nothing
  | "--" `isPrefixOf` trim entry = Nothing
  | otherwise =
      case breakOn "==" entry of
        Just (name, ver) -> Just (PackageSpec (trim name) (trim ver))
        Nothing ->
          let ws = words entry
           in case ws of
                -- Snapshot constraints like "base installed" refer to compiler-provided
                -- packages. We return a PackageSpec with version "installed" so that
                -- downstream code can fetch the latest version from Hackage.
                [name, "installed"] ->
                  let packageName = trim name
                      version = Map.findWithDefault "installed" packageName installedPackageVersions
                   in Just (PackageSpec packageName version)
                _ -> Nothing

-- | The versions of the packages the emulated GHC ships with.
installedPackageVersions :: Map.Map String String
installedPackageVersions =
  Map.fromList
    [ (name, showVersionBranch version)
    | (name, version) <- bootLibraryVersions emulatedGhc
    ]

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
  case findNeedle needle haystack of
    Nothing -> Nothing
    Just i ->
      let (left, right) = splitAt i haystack
       in Just (left, drop (length needle) right)

findNeedle :: String -> String -> Maybe Int
findNeedle needle = go 0
  where
    go _ [] = Nothing
    go i xs
      | needle `isPrefixOf` xs = Just i
      | otherwise = go (i + 1) (drop 1 xs)

splitComma :: String -> [String]
splitComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitComma rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Perform an HTTP GET request and return the response body as a String.
-- Uses lenient UTF-8 decoding to handle malformed sequences.
httpGetString :: Manager -> String -> IO (Either String String)
httpGetString manager url = do
  result <- try $ do
    request <- parseRequest url
    response <- httpLbs request manager
    let status = statusCode (responseStatus response)
    if status >= 200 && status < 300
      then pure (Right (TL.unpack (TLE.decodeUtf8With lenientDecode (responseBody response))))
      else pure (Left ("HTTP " ++ show status ++ " for " ++ url))
  case result of
    Left (err :: SomeException) -> pure (Left (displayException err))
    Right r -> pure r
