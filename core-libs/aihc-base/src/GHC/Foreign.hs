{-# LANGUAGE MagicHash #-}

-- | String marshalling through text encodings.
module GHC.Foreign
  ( peekCString,
    peekCStringLen,
    newCString,
    newCStringLen,
    withCString,
    withCStringLen,
  )
where

import Foreign.C.String (CString, CStringLen)
import GHC.Base (String)
import GHC.IO (IO)
import GHC.IO.Encoding (TextEncoding)
import GHC.Prim (raise#)

-- | Marshalling through text encodings needs byte access through pointers,
-- which is not available.
peekCString :: TextEncoding -> CString -> IO String
peekCString _ _ = marshalError "GHC.Foreign.peekCString: string marshalling is not available"

peekCStringLen :: TextEncoding -> CStringLen -> IO String
peekCStringLen _ _ = marshalError "GHC.Foreign.peekCStringLen: string marshalling is not available"

newCString :: TextEncoding -> String -> IO CString
newCString _ _ = marshalError "GHC.Foreign.newCString: string marshalling is not available"

newCStringLen :: TextEncoding -> String -> IO CStringLen
newCStringLen _ _ = marshalError "GHC.Foreign.newCStringLen: string marshalling is not available"

withCString :: TextEncoding -> String -> (CString -> IO a) -> IO a
withCString _ _ _ = marshalError "GHC.Foreign.withCString: string marshalling is not available"

withCStringLen :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a
withCStringLen _ _ _ = marshalError "GHC.Foreign.withCStringLen: string marshalling is not available"

marshalError :: String -> a
marshalError = raise#
