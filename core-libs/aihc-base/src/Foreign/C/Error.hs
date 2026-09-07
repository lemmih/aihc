{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | C error numbers and their IO error forms. The runtime reports POSIX
-- error numbers, so the constants use the values that Linux and macOS
-- share. The runtime has no thread-local @errno@, so the operations that
-- read it are not available.
module Foreign.C.Error
  ( Errno (..),
    eOK,
    ePERM,
    eNOENT,
    eSRCH,
    eINTR,
    eIO,
    eNXIO,
    e2BIG,
    eNOEXEC,
    eBADF,
    eCHILD,
    eNOMEM,
    eACCES,
    eFAULT,
    eBUSY,
    eEXIST,
    eXDEV,
    eNODEV,
    eNOTDIR,
    eISDIR,
    eINVAL,
    eNFILE,
    eMFILE,
    eNOTTY,
    eFBIG,
    eNOSPC,
    eSPIPE,
    eROFS,
    eMLINK,
    ePIPE,
    eDOM,
    eRANGE,
    isValidErrno,
    errnoToIOError,
  )
where

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Foreign.C.Types (CInt)
import GHC.Base (String, (++))
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (Handle, IOError, IOErrorType (..), IOException (..))
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import GHC.Show (Show (..))

newtype Errno = Errno CInt
  deriving newtype (Eq)

eOK, ePERM, eNOENT, eSRCH, eINTR, eIO, eNXIO, e2BIG, eNOEXEC, eBADF, eCHILD, eNOMEM, eACCES, eFAULT, eBUSY, eEXIST, eXDEV, eNODEV, eNOTDIR, eISDIR, eINVAL, eNFILE, eMFILE, eNOTTY, eFBIG, eNOSPC, eSPIPE, eROFS, eMLINK, ePIPE, eDOM, eRANGE :: Errno
eOK = Errno 0
ePERM = Errno 1
eNOENT = Errno 2
eSRCH = Errno 3
eINTR = Errno 4
eIO = Errno 5
eNXIO = Errno 6
e2BIG = Errno 7
eNOEXEC = Errno 8
eBADF = Errno 9
eCHILD = Errno 10
eNOMEM = Errno 12
eACCES = Errno 13
eFAULT = Errno 14
eBUSY = Errno 16
eEXIST = Errno 17
eXDEV = Errno 18
eNODEV = Errno 19
eNOTDIR = Errno 20
eISDIR = Errno 21
eINVAL = Errno 22
eNFILE = Errno 23
eMFILE = Errno 24
eNOTTY = Errno 25
eFBIG = Errno 27
eNOSPC = Errno 28
eSPIPE = Errno 29
eROFS = Errno 30
eMLINK = Errno 31
ePIPE = Errno 32
eDOM = Errno 33
eRANGE = Errno 34

isValidErrno :: Errno -> Bool
isValidErrno (Errno value) = value /= negate 1

-- | The IO error for an error number. The description is the C library
-- text for the error number.
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError location errno@(Errno code) handle name =
  case errnoDetails errno of
    (errorType, description) -> IOError handle errorType location description (Just code) name

errnoDetails :: Errno -> (IOErrorType, String)
errnoDetails errno@(Errno code)
  | errno == ePERM = (PermissionDenied, "Operation not permitted")
  | errno == eNOENT = (NoSuchThing, "No such file or directory")
  | errno == eSRCH = (NoSuchThing, "No such process")
  | errno == eINTR = (Interrupted, "Interrupted system call")
  | errno == eIO = (HardwareFault, "Input/output error")
  | errno == eNXIO = (NoSuchThing, "Device not configured")
  | errno == e2BIG = (ResourceExhausted, "Argument list too long")
  | errno == eNOEXEC = (InvalidArgument, "Exec format error")
  | errno == eBADF = (InvalidArgument, "Bad file descriptor")
  | errno == eCHILD = (NoSuchThing, "No child processes")
  | errno == eNOMEM = (ResourceExhausted, "Cannot allocate memory")
  | errno == eACCES = (PermissionDenied, "Permission denied")
  | errno == eFAULT = (OtherError, "Bad address")
  | errno == eBUSY = (ResourceBusy, "Resource busy")
  | errno == eEXIST = (AlreadyExists, "File exists")
  | errno == eXDEV = (UnsupportedOperation, "Cross-device link")
  | errno == eNODEV = (UnsupportedOperation, "Operation not supported by device")
  | errno == eNOTDIR = (InappropriateType, "Not a directory")
  | errno == eISDIR = (InappropriateType, "Is a directory")
  | errno == eINVAL = (InvalidArgument, "Invalid argument")
  | errno == eNFILE = (ResourceExhausted, "Too many open files in system")
  | errno == eMFILE = (ResourceExhausted, "Too many open files")
  | errno == eNOTTY = (IllegalOperation, "Inappropriate ioctl for device")
  | errno == eFBIG = (PermissionDenied, "File too large")
  | errno == eNOSPC = (ResourceExhausted, "No space left on device")
  | errno == eSPIPE = (UnsupportedOperation, "Illegal seek")
  | errno == eROFS = (PermissionDenied, "Read-only file system")
  | errno == eMLINK = (ResourceExhausted, "Too many links")
  | errno == ePIPE = (ResourceVanished, "Broken pipe")
  | errno == eDOM = (InvalidArgument, "Numerical argument out of domain")
  | errno == eRANGE = (UnsupportedOperation, "Result too large")
  | otherwise = (OtherError, "errno " ++ show (fromIntegral code :: Int))
  where
    otherwise = True
