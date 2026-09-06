{-# LANGUAGE MagicHash #-}

-- | Low-level byte output used to bootstrap the text operations exported by
-- 'Prelude'. This module must not depend on 'Prelude', because 'Prelude'
-- supplies the public 'String' traversal and encoding policy.
module GHC.IO.Console
  ( writeOutputByte,
    writeStdout,
  )
where

import GHC.Base (Monad (return))
import GHC.IO (IO)
import GHC.IO.Runtime (IOHandle, awaitIO, raiseIOErrorRaw, stdoutHandle, submitWrite, takeResult, writeMemoryByte)
import GHC.Int (Int (..))
import GHC.Prim
  ( Addr#,
    Int#,
    MutableByteArray#,
    RealWorld,
    mutableByteArrayContents#,
    (+#),
    (-#),
    (<#),
    (==#),
  )
import GHC.Ptr (Ptr)

writeOutputByte :: MutableByteArray# RealWorld -> Int# -> Int# -> IO ()
writeOutputByte buffer offset value = do
  encodedError <- writeMemoryByte (mutableByteArrayContents# buffer) (I# offset) (I# value)
  checkOutputByteResult encodedError

checkOutputByteResult :: Int -> IO ()
checkOutputByteResult (I# encodedError) =
  case (<#) encodedError 0# of
    1# -> raiseConsoleIOError ((-#) ((-#) 0# encodedError) 1#)
    _ -> return ()

writeStdout :: MutableByteArray# RealWorld -> Int# -> IO ()
writeStdout buffer count =
  case (==#) count 0# of
    1# -> return ()
    _ -> do
      handle <- stdoutHandle
      writeStdoutLoop handle (mutableByteArrayContents# buffer) 0# count

writeStdoutLoop :: Ptr IOHandle -> Addr# -> Int# -> Int# -> IO ()
writeStdoutLoop handle buffer offset remaining = do
  request <- submitWrite handle buffer (I# offset) (I# remaining)
  awaitIO request
  transferred <- takeResult request
  finishWriteResult handle buffer offset remaining transferred

finishWriteResult :: Ptr IOHandle -> Addr# -> Int# -> Int# -> Int -> IO ()
finishWriteResult handle buffer offset remaining (I# transferred) =
  case (<#) transferred 0# of
    1# -> raiseConsoleIOError ((-#) ((-#) 0# transferred) 1#)
    _ ->
      case (==#) transferred 0# of
        1# -> raiseConsoleIOError 6#
        _ ->
          case (==#) transferred remaining of
            1# -> return ()
            _ -> writeStdoutLoop handle buffer ((+#) offset transferred) ((-#) remaining transferred)

raiseConsoleIOError :: Int# -> IO ()
raiseConsoleIOError exceptionCode = do
  nextException <- raiseIOErrorRaw (I# exceptionCode)
  case nextException of
    I# nextExceptionCode -> raiseConsoleIOError nextExceptionCode
