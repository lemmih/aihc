module Main (main) where

import Control.Monad (unless)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \pkg lbi hooks flags -> do
          generateIdentity pkg lbi
          buildHook simpleUserHooks pkg lbi hooks flags,
        replHook = \pkg lbi hooks flags args -> do
          generateIdentity pkg lbi
          replHook simpleUserHooks pkg lbi hooks flags args,
        haddockHook = \pkg lbi hooks flags -> do
          generateIdentity pkg lbi
          haddockHook simpleUserHooks pkg lbi hooks flags
      }

generateIdentity :: PackageDescription -> LocalBuildInfo -> IO ()
generateIdentity pkg lbi = do
  identity <- catchIOError gitCommit (const (pure ""))
  withLibLBI pkg lbi $ \_ clbi -> do
    let output = getSymbolicPath (autogenComponentModulesDir lbi clbi) </> "Aihc/CompilerBuildIdentity.hs"
        content = "module Aihc.CompilerBuildIdentity (compilerBuildIdentity) where\n\ncompilerBuildIdentity :: String\ncompilerBuildIdentity = " <> show identity <> "\n"
    exists <- doesFileExist output
    unchanged <- if exists then (== content) <$> readFile' output else pure False
    unless unchanged $ do
      createDirectoryIfMissing True (takeDirectory output)
      writeFile output content

gitCommit :: IO String
gitCommit = do
  (status, output, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
  pure (if status == ExitSuccess then takeWhile (/= '\n') output else "")
