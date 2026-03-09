{-# LANGUAGE CPP #-}
#ifdef WINDOWS
module System.OsPath.Windows where
#elif defined(POSIX)
module System.OsPath.Posix where
#else
module System.OsPath where
#endif
