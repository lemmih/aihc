{-# LANGUAGE RecordWildCards #-}

-- | Keep the output of the test runner away from file descriptor 1.
--
-- The test runner writes its progress to the 'stdout' handle from its own
-- thread. A program under test writes to file descriptor 1: the GRIN
-- interpreter writes through its own handle on that descriptor, and a foreign
-- call writes through the C library. An evaluation fixture with an expected
-- @stdout@ redirects descriptor 1 to a file while the program runs. When the
-- 'stdout' handle also points at descriptor 1, the progress of other tests
-- lands in that file and the fixture fails at random.
--
-- 'detachHostStdout' moves the 'stdout' handle to a duplicate of descriptor 1.
-- The handle then writes to the same terminal or pipe as before, and a
-- redirect of descriptor 1 no longer catches it.
module Aihc.Testing.HostStdout (detachHostStdout) where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Typeable (cast)
import GHC.IO.FD (FD (..))
import GHC.IO.Handle.Types (Handle (..), Handle__ (..))
import System.IO (hFlush, stdout)
import System.Posix.IO (closeFd, dup, stdOutput)

-- | Point the 'stdout' handle at a duplicate of file descriptor 1.
--
-- Call this one time, before the test runner starts. The handle keeps its
-- buffers, its encoding and its terminal status. Only its descriptor changes.
detachHostStdout :: IO ()
detachHostStdout = do
  hFlush stdout
  duplicate <- dup stdOutput
  case stdout of
    FileHandle _ cell ->
      modifyMVar_ cell $ \Handle__ {..} ->
        case cast haDevice of
          Just device ->
            pure Handle__ {haDevice = device {fdFD = fromIntegral duplicate}, ..}
          Nothing -> do
            closeFd duplicate
            pure Handle__ {..}
    DuplexHandle {} -> closeFd duplicate
