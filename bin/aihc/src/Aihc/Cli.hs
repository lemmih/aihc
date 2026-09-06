module Aihc.Cli
  ( main,
    runCommand,
  )
where

import Aihc.Cli.BuildExe (runBuildExe, runLinkExe)
import Aihc.Cli.Install (runInstall)
import Aihc.Cli.Options (Command (..), parseCommandIO)
import Aihc.Cli.Runtime (runPrepareRuntime)
import Control.Exception (IOException, displayException, try)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  result <- try (parseCommandIO >>= runCommand) :: IO (Either IOException ())
  case result of
    Right () -> pure ()
    Left err -> do
      hPutStrLn stderr ("aihc: " <> displayException err)
      exitFailure

runCommand :: Command -> IO ()
runCommand (CmdBuildExe opts) = runBuildExe opts
runCommand (CmdInstall opts) = runInstall opts
runCommand (CmdLinkExe opts) = runLinkExe opts
runCommand (CmdPrepareRuntime opts) = runPrepareRuntime opts
