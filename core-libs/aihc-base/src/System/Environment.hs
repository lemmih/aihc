-- | Process argument access. The scoped mutation operations update the same
-- process-global vector observed by every Haskell thread.
module System.Environment
  ( getArgs,
    getEnv,
    getProgName,
    withArgs,
    withProgName,
  )
where

import Control.Exception.Base (SomeException, catch, throwIO)
import GHC.Internal.Environment (getFullArgs, setFullArgs)
import Prelude

getEnv :: String -> IO String
getEnv name = return (error ("System.Environment.getEnv is not available: " ++ name))

-- | Return every initial or replacement argument after the program name.
getArgs :: IO [String]
getArgs = do
  arguments <- getFullArgs
  case arguments of
    [] -> return []
    _programName : rest -> return rest

-- | Return the final path component of the current program name.
getProgName :: IO String
getProgName = do
  arguments <- getFullArgs
  case arguments of
    [] -> return []
    programName : _ -> return (baseName programName)

-- | Run an action with replacement arguments, restoring the previous vector
-- after normal completion or a synchronous exception.
withArgs :: [String] -> IO a -> IO a
withArgs arguments action = do
  oldArguments <- getFullArgs
  let programName =
        case oldArguments of
          [] -> []
          name : _ -> name
  withFullArgs (programName : arguments) oldArguments action

-- | Run an action with a replacement program name, restoring it afterwards.
withProgName :: String -> IO a -> IO a
withProgName programName action = do
  oldArguments <- getFullArgs
  let arguments =
        case oldArguments of
          [] -> []
          _oldProgramName : rest -> rest
  withFullArgs (programName : arguments) oldArguments action

withFullArgs :: [String] -> [String] -> IO a -> IO a
withFullArgs newArguments oldArguments action = do
  setFullArgs newArguments
  result <- restoreOnException action (setFullArgs oldArguments)
  setFullArgs oldArguments
  return result

restoreOnException :: IO a -> IO () -> IO a
restoreOnException action cleanup = catch action handler
  where
    handler :: SomeException -> IO a
    handler exception = do
      cleanup
      throwIO exception

baseName :: String -> String
baseName = go []
  where
    go latest [] = reverseString latest
    go _ ('/' : rest) = go [] rest
    go latest (character : rest) = go (character : latest) rest

reverseString :: String -> String
reverseString = go []
  where
    go :: String -> String -> String
    go reversed [] = reversed
    go reversed (character : rest) = go (character : reversed) rest
