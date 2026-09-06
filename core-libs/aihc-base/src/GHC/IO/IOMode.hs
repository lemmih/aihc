module GHC.IO.IOMode
  ( IOMode (..),
  )
where

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode
