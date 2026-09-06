module Foreign.C.Types
  ( CBool (..),
    CChar (..),
    CClock (..),
    CDouble (..),
    CFile,
    CFloat (..),
    CFpos,
    CInt (..),
    CIntMax (..),
    CIntPtr (..),
    CJmpBuf,
    CLLong (..),
    CLong (..),
    CPtrdiff (..),
    CSChar (..),
    CSUSeconds (..),
    CShort (..),
    CSigAtomic (..),
    CSize (..),
    CTime (..),
    CUChar (..),
    CUInt (..),
    CUIntMax (..),
    CUIntPtr (..),
    CULLong (..),
    CULong (..),
    CUSeconds (..),
    CUShort (..),
    CWchar (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types.Repr
  ( CIntPtrRep,
    CLongRep,
    CPtrdiffRep,
    CSizeRep,
    CUIntPtrRep,
    CULongRep,
  )
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Float ()
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Real (Integral (..), Real (..))
import GHC.Types (Double, Float)

newtype CBool = CBool Word8

newtype CChar = CChar Int8

newtype CClock = CClock Word64

newtype CDouble = CDouble Double

data CFile = CFile

newtype CFloat = CFloat Float

data CFpos = CFpos

newtype CInt = CInt Int32

newtype CIntMax = CIntMax Int64

newtype CIntPtr = CIntPtr CIntPtrRep

data CJmpBuf = CJmpBuf

newtype CLLong = CLLong Int64

newtype CLong = CLong CLongRep

newtype CPtrdiff = CPtrdiff CPtrdiffRep

newtype CSChar = CSChar Int8

newtype CSUSeconds = CSUSeconds Int32

newtype CShort = CShort Int16

newtype CSigAtomic = CSigAtomic Int32

newtype CSize = CSize CSizeRep

newtype CTime = CTime Int64

newtype CUChar = CUChar Word8

newtype CUInt = CUInt Word32

newtype CUIntMax = CUIntMax Word64

newtype CUIntPtr = CUIntPtr CUIntPtrRep

newtype CULLong = CULLong Word64

newtype CULong = CULong CULongRep

newtype CUSeconds = CUSeconds Word32

newtype CUShort = CUShort Word16

newtype CWchar = CWchar Int32

-- | A local list map keeps this module free of a Data.List import.
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList convert (value : values) = convert value : mapList convert values

-- Each C integer type is a newtype over the Haskell type that matches its
-- platform width, so every class below simply forwards to that type.

instance Eq CBool where
  CBool left == CBool right = left == right

instance Ord CBool where
  compare (CBool left) (CBool right) = compare left right

instance Enum CBool where
  succ (CBool value) = CBool (succ value)
  pred (CBool value) = CBool (pred value)
  toEnum value = CBool (toEnum value)
  fromEnum (CBool value) = fromEnum value
  enumFrom (CBool value) = mapList CBool (enumFrom value)
  enumFromThen (CBool first) (CBool second) = mapList CBool (enumFromThen first second)
  enumFromTo (CBool first) (CBool final) = mapList CBool (enumFromTo first final)
  enumFromThenTo (CBool first) (CBool second) (CBool final) = mapList CBool (enumFromThenTo first second final)

instance Bounded CBool where
  minBound = CBool minBound
  maxBound = CBool maxBound

instance Num CBool where
  CBool left + CBool right = CBool (left + right)
  CBool left - CBool right = CBool (left - right)
  CBool left * CBool right = CBool (left * right)
  negate (CBool value) = CBool (negate value)
  abs (CBool value) = CBool (abs value)
  signum (CBool value) = CBool (signum value)
  fromInteger value = CBool (fromInteger value)

instance Real CBool where
  toRational (CBool value) = toRational value

instance Integral CBool where
  quot (CBool left) (CBool right) = CBool (quot left right)
  rem (CBool left) (CBool right) = CBool (rem left right)
  div (CBool left) (CBool right) = CBool (div left right)
  mod (CBool left) (CBool right) = CBool (mod left right)
  quotRem (CBool left) (CBool right) = case quotRem left right of
    (quotient, remainder) -> (CBool quotient, CBool remainder)
  divMod (CBool left) (CBool right) = case divMod left right of
    (quotient, modulus) -> (CBool quotient, CBool modulus)
  toInteger (CBool value) = toInteger value

instance Eq CChar where
  CChar left == CChar right = left == right

instance Ord CChar where
  compare (CChar left) (CChar right) = compare left right

instance Enum CChar where
  succ (CChar value) = CChar (succ value)
  pred (CChar value) = CChar (pred value)
  toEnum value = CChar (toEnum value)
  fromEnum (CChar value) = fromEnum value
  enumFrom (CChar value) = mapList CChar (enumFrom value)
  enumFromThen (CChar first) (CChar second) = mapList CChar (enumFromThen first second)
  enumFromTo (CChar first) (CChar final) = mapList CChar (enumFromTo first final)
  enumFromThenTo (CChar first) (CChar second) (CChar final) = mapList CChar (enumFromThenTo first second final)

instance Bounded CChar where
  minBound = CChar minBound
  maxBound = CChar maxBound

instance Num CChar where
  CChar left + CChar right = CChar (left + right)
  CChar left - CChar right = CChar (left - right)
  CChar left * CChar right = CChar (left * right)
  negate (CChar value) = CChar (negate value)
  abs (CChar value) = CChar (abs value)
  signum (CChar value) = CChar (signum value)
  fromInteger value = CChar (fromInteger value)

instance Real CChar where
  toRational (CChar value) = toRational value

instance Integral CChar where
  quot (CChar left) (CChar right) = CChar (quot left right)
  rem (CChar left) (CChar right) = CChar (rem left right)
  div (CChar left) (CChar right) = CChar (div left right)
  mod (CChar left) (CChar right) = CChar (mod left right)
  quotRem (CChar left) (CChar right) = case quotRem left right of
    (quotient, remainder) -> (CChar quotient, CChar remainder)
  divMod (CChar left) (CChar right) = case divMod left right of
    (quotient, modulus) -> (CChar quotient, CChar modulus)
  toInteger (CChar value) = toInteger value

instance Eq CClock where
  CClock left == CClock right = left == right

instance Ord CClock where
  compare (CClock left) (CClock right) = compare left right

instance Enum CClock where
  succ (CClock value) = CClock (succ value)
  pred (CClock value) = CClock (pred value)
  toEnum value = CClock (toEnum value)
  fromEnum (CClock value) = fromEnum value
  enumFrom (CClock value) = mapList CClock (enumFrom value)
  enumFromThen (CClock first) (CClock second) = mapList CClock (enumFromThen first second)
  enumFromTo (CClock first) (CClock final) = mapList CClock (enumFromTo first final)
  enumFromThenTo (CClock first) (CClock second) (CClock final) = mapList CClock (enumFromThenTo first second final)

instance Bounded CClock where
  minBound = CClock minBound
  maxBound = CClock maxBound

instance Num CClock where
  CClock left + CClock right = CClock (left + right)
  CClock left - CClock right = CClock (left - right)
  CClock left * CClock right = CClock (left * right)
  negate (CClock value) = CClock (negate value)
  abs (CClock value) = CClock (abs value)
  signum (CClock value) = CClock (signum value)
  fromInteger value = CClock (fromInteger value)

instance Real CClock where
  toRational (CClock value) = toRational value

instance Integral CClock where
  quot (CClock left) (CClock right) = CClock (quot left right)
  rem (CClock left) (CClock right) = CClock (rem left right)
  div (CClock left) (CClock right) = CClock (div left right)
  mod (CClock left) (CClock right) = CClock (mod left right)
  quotRem (CClock left) (CClock right) = case quotRem left right of
    (quotient, remainder) -> (CClock quotient, CClock remainder)
  divMod (CClock left) (CClock right) = case divMod left right of
    (quotient, modulus) -> (CClock quotient, CClock modulus)
  toInteger (CClock value) = toInteger value

instance Eq CInt where
  CInt left == CInt right = left == right

instance Ord CInt where
  compare (CInt left) (CInt right) = compare left right

instance Enum CInt where
  succ (CInt value) = CInt (succ value)
  pred (CInt value) = CInt (pred value)
  toEnum value = CInt (toEnum value)
  fromEnum (CInt value) = fromEnum value
  enumFrom (CInt value) = mapList CInt (enumFrom value)
  enumFromThen (CInt first) (CInt second) = mapList CInt (enumFromThen first second)
  enumFromTo (CInt first) (CInt final) = mapList CInt (enumFromTo first final)
  enumFromThenTo (CInt first) (CInt second) (CInt final) = mapList CInt (enumFromThenTo first second final)

instance Bounded CInt where
  minBound = CInt minBound
  maxBound = CInt maxBound

instance Num CInt where
  CInt left + CInt right = CInt (left + right)
  CInt left - CInt right = CInt (left - right)
  CInt left * CInt right = CInt (left * right)
  negate (CInt value) = CInt (negate value)
  abs (CInt value) = CInt (abs value)
  signum (CInt value) = CInt (signum value)
  fromInteger value = CInt (fromInteger value)

instance Real CInt where
  toRational (CInt value) = toRational value

instance Integral CInt where
  quot (CInt left) (CInt right) = CInt (quot left right)
  rem (CInt left) (CInt right) = CInt (rem left right)
  div (CInt left) (CInt right) = CInt (div left right)
  mod (CInt left) (CInt right) = CInt (mod left right)
  quotRem (CInt left) (CInt right) = case quotRem left right of
    (quotient, remainder) -> (CInt quotient, CInt remainder)
  divMod (CInt left) (CInt right) = case divMod left right of
    (quotient, modulus) -> (CInt quotient, CInt modulus)
  toInteger (CInt value) = toInteger value

instance Eq CIntMax where
  CIntMax left == CIntMax right = left == right

instance Ord CIntMax where
  compare (CIntMax left) (CIntMax right) = compare left right

instance Enum CIntMax where
  succ (CIntMax value) = CIntMax (succ value)
  pred (CIntMax value) = CIntMax (pred value)
  toEnum value = CIntMax (toEnum value)
  fromEnum (CIntMax value) = fromEnum value
  enumFrom (CIntMax value) = mapList CIntMax (enumFrom value)
  enumFromThen (CIntMax first) (CIntMax second) = mapList CIntMax (enumFromThen first second)
  enumFromTo (CIntMax first) (CIntMax final) = mapList CIntMax (enumFromTo first final)
  enumFromThenTo (CIntMax first) (CIntMax second) (CIntMax final) = mapList CIntMax (enumFromThenTo first second final)

instance Bounded CIntMax where
  minBound = CIntMax minBound
  maxBound = CIntMax maxBound

instance Num CIntMax where
  CIntMax left + CIntMax right = CIntMax (left + right)
  CIntMax left - CIntMax right = CIntMax (left - right)
  CIntMax left * CIntMax right = CIntMax (left * right)
  negate (CIntMax value) = CIntMax (negate value)
  abs (CIntMax value) = CIntMax (abs value)
  signum (CIntMax value) = CIntMax (signum value)
  fromInteger value = CIntMax (fromInteger value)

instance Real CIntMax where
  toRational (CIntMax value) = toRational value

instance Integral CIntMax where
  quot (CIntMax left) (CIntMax right) = CIntMax (quot left right)
  rem (CIntMax left) (CIntMax right) = CIntMax (rem left right)
  div (CIntMax left) (CIntMax right) = CIntMax (div left right)
  mod (CIntMax left) (CIntMax right) = CIntMax (mod left right)
  quotRem (CIntMax left) (CIntMax right) = case quotRem left right of
    (quotient, remainder) -> (CIntMax quotient, CIntMax remainder)
  divMod (CIntMax left) (CIntMax right) = case divMod left right of
    (quotient, modulus) -> (CIntMax quotient, CIntMax modulus)
  toInteger (CIntMax value) = toInteger value

instance Eq CIntPtr where
  CIntPtr left == CIntPtr right = left == right

instance Ord CIntPtr where
  compare (CIntPtr left) (CIntPtr right) = compare left right

instance Enum CIntPtr where
  succ (CIntPtr value) = CIntPtr (succ value)
  pred (CIntPtr value) = CIntPtr (pred value)
  toEnum value = CIntPtr (toEnum value)
  fromEnum (CIntPtr value) = fromEnum value
  enumFrom (CIntPtr value) = mapList CIntPtr (enumFrom value)
  enumFromThen (CIntPtr first) (CIntPtr second) = mapList CIntPtr (enumFromThen first second)
  enumFromTo (CIntPtr first) (CIntPtr final) = mapList CIntPtr (enumFromTo first final)
  enumFromThenTo (CIntPtr first) (CIntPtr second) (CIntPtr final) = mapList CIntPtr (enumFromThenTo first second final)

instance Bounded CIntPtr where
  minBound = CIntPtr minBound
  maxBound = CIntPtr maxBound

instance Num CIntPtr where
  CIntPtr left + CIntPtr right = CIntPtr (left + right)
  CIntPtr left - CIntPtr right = CIntPtr (left - right)
  CIntPtr left * CIntPtr right = CIntPtr (left * right)
  negate (CIntPtr value) = CIntPtr (negate value)
  abs (CIntPtr value) = CIntPtr (abs value)
  signum (CIntPtr value) = CIntPtr (signum value)
  fromInteger value = CIntPtr (fromInteger value)

instance Real CIntPtr where
  toRational (CIntPtr value) = toRational value

instance Integral CIntPtr where
  quot (CIntPtr left) (CIntPtr right) = CIntPtr (quot left right)
  rem (CIntPtr left) (CIntPtr right) = CIntPtr (rem left right)
  div (CIntPtr left) (CIntPtr right) = CIntPtr (div left right)
  mod (CIntPtr left) (CIntPtr right) = CIntPtr (mod left right)
  quotRem (CIntPtr left) (CIntPtr right) = case quotRem left right of
    (quotient, remainder) -> (CIntPtr quotient, CIntPtr remainder)
  divMod (CIntPtr left) (CIntPtr right) = case divMod left right of
    (quotient, modulus) -> (CIntPtr quotient, CIntPtr modulus)
  toInteger (CIntPtr value) = toInteger value

instance Eq CLLong where
  CLLong left == CLLong right = left == right

instance Ord CLLong where
  compare (CLLong left) (CLLong right) = compare left right

instance Enum CLLong where
  succ (CLLong value) = CLLong (succ value)
  pred (CLLong value) = CLLong (pred value)
  toEnum value = CLLong (toEnum value)
  fromEnum (CLLong value) = fromEnum value
  enumFrom (CLLong value) = mapList CLLong (enumFrom value)
  enumFromThen (CLLong first) (CLLong second) = mapList CLLong (enumFromThen first second)
  enumFromTo (CLLong first) (CLLong final) = mapList CLLong (enumFromTo first final)
  enumFromThenTo (CLLong first) (CLLong second) (CLLong final) = mapList CLLong (enumFromThenTo first second final)

instance Bounded CLLong where
  minBound = CLLong minBound
  maxBound = CLLong maxBound

instance Num CLLong where
  CLLong left + CLLong right = CLLong (left + right)
  CLLong left - CLLong right = CLLong (left - right)
  CLLong left * CLLong right = CLLong (left * right)
  negate (CLLong value) = CLLong (negate value)
  abs (CLLong value) = CLLong (abs value)
  signum (CLLong value) = CLLong (signum value)
  fromInteger value = CLLong (fromInteger value)

instance Real CLLong where
  toRational (CLLong value) = toRational value

instance Integral CLLong where
  quot (CLLong left) (CLLong right) = CLLong (quot left right)
  rem (CLLong left) (CLLong right) = CLLong (rem left right)
  div (CLLong left) (CLLong right) = CLLong (div left right)
  mod (CLLong left) (CLLong right) = CLLong (mod left right)
  quotRem (CLLong left) (CLLong right) = case quotRem left right of
    (quotient, remainder) -> (CLLong quotient, CLLong remainder)
  divMod (CLLong left) (CLLong right) = case divMod left right of
    (quotient, modulus) -> (CLLong quotient, CLLong modulus)
  toInteger (CLLong value) = toInteger value

instance Eq CLong where
  CLong left == CLong right = left == right

instance Ord CLong where
  compare (CLong left) (CLong right) = compare left right

instance Enum CLong where
  succ (CLong value) = CLong (succ value)
  pred (CLong value) = CLong (pred value)
  toEnum value = CLong (toEnum value)
  fromEnum (CLong value) = fromEnum value
  enumFrom (CLong value) = mapList CLong (enumFrom value)
  enumFromThen (CLong first) (CLong second) = mapList CLong (enumFromThen first second)
  enumFromTo (CLong first) (CLong final) = mapList CLong (enumFromTo first final)
  enumFromThenTo (CLong first) (CLong second) (CLong final) = mapList CLong (enumFromThenTo first second final)

instance Bounded CLong where
  minBound = CLong minBound
  maxBound = CLong maxBound

instance Num CLong where
  CLong left + CLong right = CLong (left + right)
  CLong left - CLong right = CLong (left - right)
  CLong left * CLong right = CLong (left * right)
  negate (CLong value) = CLong (negate value)
  abs (CLong value) = CLong (abs value)
  signum (CLong value) = CLong (signum value)
  fromInteger value = CLong (fromInteger value)

instance Real CLong where
  toRational (CLong value) = toRational value

instance Integral CLong where
  quot (CLong left) (CLong right) = CLong (quot left right)
  rem (CLong left) (CLong right) = CLong (rem left right)
  div (CLong left) (CLong right) = CLong (div left right)
  mod (CLong left) (CLong right) = CLong (mod left right)
  quotRem (CLong left) (CLong right) = case quotRem left right of
    (quotient, remainder) -> (CLong quotient, CLong remainder)
  divMod (CLong left) (CLong right) = case divMod left right of
    (quotient, modulus) -> (CLong quotient, CLong modulus)
  toInteger (CLong value) = toInteger value

instance Eq CPtrdiff where
  CPtrdiff left == CPtrdiff right = left == right

instance Ord CPtrdiff where
  compare (CPtrdiff left) (CPtrdiff right) = compare left right

instance Enum CPtrdiff where
  succ (CPtrdiff value) = CPtrdiff (succ value)
  pred (CPtrdiff value) = CPtrdiff (pred value)
  toEnum value = CPtrdiff (toEnum value)
  fromEnum (CPtrdiff value) = fromEnum value
  enumFrom (CPtrdiff value) = mapList CPtrdiff (enumFrom value)
  enumFromThen (CPtrdiff first) (CPtrdiff second) = mapList CPtrdiff (enumFromThen first second)
  enumFromTo (CPtrdiff first) (CPtrdiff final) = mapList CPtrdiff (enumFromTo first final)
  enumFromThenTo (CPtrdiff first) (CPtrdiff second) (CPtrdiff final) = mapList CPtrdiff (enumFromThenTo first second final)

instance Bounded CPtrdiff where
  minBound = CPtrdiff minBound
  maxBound = CPtrdiff maxBound

instance Num CPtrdiff where
  CPtrdiff left + CPtrdiff right = CPtrdiff (left + right)
  CPtrdiff left - CPtrdiff right = CPtrdiff (left - right)
  CPtrdiff left * CPtrdiff right = CPtrdiff (left * right)
  negate (CPtrdiff value) = CPtrdiff (negate value)
  abs (CPtrdiff value) = CPtrdiff (abs value)
  signum (CPtrdiff value) = CPtrdiff (signum value)
  fromInteger value = CPtrdiff (fromInteger value)

instance Real CPtrdiff where
  toRational (CPtrdiff value) = toRational value

instance Integral CPtrdiff where
  quot (CPtrdiff left) (CPtrdiff right) = CPtrdiff (quot left right)
  rem (CPtrdiff left) (CPtrdiff right) = CPtrdiff (rem left right)
  div (CPtrdiff left) (CPtrdiff right) = CPtrdiff (div left right)
  mod (CPtrdiff left) (CPtrdiff right) = CPtrdiff (mod left right)
  quotRem (CPtrdiff left) (CPtrdiff right) = case quotRem left right of
    (quotient, remainder) -> (CPtrdiff quotient, CPtrdiff remainder)
  divMod (CPtrdiff left) (CPtrdiff right) = case divMod left right of
    (quotient, modulus) -> (CPtrdiff quotient, CPtrdiff modulus)
  toInteger (CPtrdiff value) = toInteger value

instance Eq CSChar where
  CSChar left == CSChar right = left == right

instance Ord CSChar where
  compare (CSChar left) (CSChar right) = compare left right

instance Enum CSChar where
  succ (CSChar value) = CSChar (succ value)
  pred (CSChar value) = CSChar (pred value)
  toEnum value = CSChar (toEnum value)
  fromEnum (CSChar value) = fromEnum value
  enumFrom (CSChar value) = mapList CSChar (enumFrom value)
  enumFromThen (CSChar first) (CSChar second) = mapList CSChar (enumFromThen first second)
  enumFromTo (CSChar first) (CSChar final) = mapList CSChar (enumFromTo first final)
  enumFromThenTo (CSChar first) (CSChar second) (CSChar final) = mapList CSChar (enumFromThenTo first second final)

instance Bounded CSChar where
  minBound = CSChar minBound
  maxBound = CSChar maxBound

instance Num CSChar where
  CSChar left + CSChar right = CSChar (left + right)
  CSChar left - CSChar right = CSChar (left - right)
  CSChar left * CSChar right = CSChar (left * right)
  negate (CSChar value) = CSChar (negate value)
  abs (CSChar value) = CSChar (abs value)
  signum (CSChar value) = CSChar (signum value)
  fromInteger value = CSChar (fromInteger value)

instance Real CSChar where
  toRational (CSChar value) = toRational value

instance Integral CSChar where
  quot (CSChar left) (CSChar right) = CSChar (quot left right)
  rem (CSChar left) (CSChar right) = CSChar (rem left right)
  div (CSChar left) (CSChar right) = CSChar (div left right)
  mod (CSChar left) (CSChar right) = CSChar (mod left right)
  quotRem (CSChar left) (CSChar right) = case quotRem left right of
    (quotient, remainder) -> (CSChar quotient, CSChar remainder)
  divMod (CSChar left) (CSChar right) = case divMod left right of
    (quotient, modulus) -> (CSChar quotient, CSChar modulus)
  toInteger (CSChar value) = toInteger value

instance Eq CSUSeconds where
  CSUSeconds left == CSUSeconds right = left == right

instance Ord CSUSeconds where
  compare (CSUSeconds left) (CSUSeconds right) = compare left right

instance Enum CSUSeconds where
  succ (CSUSeconds value) = CSUSeconds (succ value)
  pred (CSUSeconds value) = CSUSeconds (pred value)
  toEnum value = CSUSeconds (toEnum value)
  fromEnum (CSUSeconds value) = fromEnum value
  enumFrom (CSUSeconds value) = mapList CSUSeconds (enumFrom value)
  enumFromThen (CSUSeconds first) (CSUSeconds second) = mapList CSUSeconds (enumFromThen first second)
  enumFromTo (CSUSeconds first) (CSUSeconds final) = mapList CSUSeconds (enumFromTo first final)
  enumFromThenTo (CSUSeconds first) (CSUSeconds second) (CSUSeconds final) = mapList CSUSeconds (enumFromThenTo first second final)

instance Bounded CSUSeconds where
  minBound = CSUSeconds minBound
  maxBound = CSUSeconds maxBound

instance Num CSUSeconds where
  CSUSeconds left + CSUSeconds right = CSUSeconds (left + right)
  CSUSeconds left - CSUSeconds right = CSUSeconds (left - right)
  CSUSeconds left * CSUSeconds right = CSUSeconds (left * right)
  negate (CSUSeconds value) = CSUSeconds (negate value)
  abs (CSUSeconds value) = CSUSeconds (abs value)
  signum (CSUSeconds value) = CSUSeconds (signum value)
  fromInteger value = CSUSeconds (fromInteger value)

instance Real CSUSeconds where
  toRational (CSUSeconds value) = toRational value

instance Integral CSUSeconds where
  quot (CSUSeconds left) (CSUSeconds right) = CSUSeconds (quot left right)
  rem (CSUSeconds left) (CSUSeconds right) = CSUSeconds (rem left right)
  div (CSUSeconds left) (CSUSeconds right) = CSUSeconds (div left right)
  mod (CSUSeconds left) (CSUSeconds right) = CSUSeconds (mod left right)
  quotRem (CSUSeconds left) (CSUSeconds right) = case quotRem left right of
    (quotient, remainder) -> (CSUSeconds quotient, CSUSeconds remainder)
  divMod (CSUSeconds left) (CSUSeconds right) = case divMod left right of
    (quotient, modulus) -> (CSUSeconds quotient, CSUSeconds modulus)
  toInteger (CSUSeconds value) = toInteger value

instance Eq CShort where
  CShort left == CShort right = left == right

instance Ord CShort where
  compare (CShort left) (CShort right) = compare left right

instance Enum CShort where
  succ (CShort value) = CShort (succ value)
  pred (CShort value) = CShort (pred value)
  toEnum value = CShort (toEnum value)
  fromEnum (CShort value) = fromEnum value
  enumFrom (CShort value) = mapList CShort (enumFrom value)
  enumFromThen (CShort first) (CShort second) = mapList CShort (enumFromThen first second)
  enumFromTo (CShort first) (CShort final) = mapList CShort (enumFromTo first final)
  enumFromThenTo (CShort first) (CShort second) (CShort final) = mapList CShort (enumFromThenTo first second final)

instance Bounded CShort where
  minBound = CShort minBound
  maxBound = CShort maxBound

instance Num CShort where
  CShort left + CShort right = CShort (left + right)
  CShort left - CShort right = CShort (left - right)
  CShort left * CShort right = CShort (left * right)
  negate (CShort value) = CShort (negate value)
  abs (CShort value) = CShort (abs value)
  signum (CShort value) = CShort (signum value)
  fromInteger value = CShort (fromInteger value)

instance Real CShort where
  toRational (CShort value) = toRational value

instance Integral CShort where
  quot (CShort left) (CShort right) = CShort (quot left right)
  rem (CShort left) (CShort right) = CShort (rem left right)
  div (CShort left) (CShort right) = CShort (div left right)
  mod (CShort left) (CShort right) = CShort (mod left right)
  quotRem (CShort left) (CShort right) = case quotRem left right of
    (quotient, remainder) -> (CShort quotient, CShort remainder)
  divMod (CShort left) (CShort right) = case divMod left right of
    (quotient, modulus) -> (CShort quotient, CShort modulus)
  toInteger (CShort value) = toInteger value

instance Eq CSigAtomic where
  CSigAtomic left == CSigAtomic right = left == right

instance Ord CSigAtomic where
  compare (CSigAtomic left) (CSigAtomic right) = compare left right

instance Enum CSigAtomic where
  succ (CSigAtomic value) = CSigAtomic (succ value)
  pred (CSigAtomic value) = CSigAtomic (pred value)
  toEnum value = CSigAtomic (toEnum value)
  fromEnum (CSigAtomic value) = fromEnum value
  enumFrom (CSigAtomic value) = mapList CSigAtomic (enumFrom value)
  enumFromThen (CSigAtomic first) (CSigAtomic second) = mapList CSigAtomic (enumFromThen first second)
  enumFromTo (CSigAtomic first) (CSigAtomic final) = mapList CSigAtomic (enumFromTo first final)
  enumFromThenTo (CSigAtomic first) (CSigAtomic second) (CSigAtomic final) = mapList CSigAtomic (enumFromThenTo first second final)

instance Bounded CSigAtomic where
  minBound = CSigAtomic minBound
  maxBound = CSigAtomic maxBound

instance Num CSigAtomic where
  CSigAtomic left + CSigAtomic right = CSigAtomic (left + right)
  CSigAtomic left - CSigAtomic right = CSigAtomic (left - right)
  CSigAtomic left * CSigAtomic right = CSigAtomic (left * right)
  negate (CSigAtomic value) = CSigAtomic (negate value)
  abs (CSigAtomic value) = CSigAtomic (abs value)
  signum (CSigAtomic value) = CSigAtomic (signum value)
  fromInteger value = CSigAtomic (fromInteger value)

instance Real CSigAtomic where
  toRational (CSigAtomic value) = toRational value

instance Integral CSigAtomic where
  quot (CSigAtomic left) (CSigAtomic right) = CSigAtomic (quot left right)
  rem (CSigAtomic left) (CSigAtomic right) = CSigAtomic (rem left right)
  div (CSigAtomic left) (CSigAtomic right) = CSigAtomic (div left right)
  mod (CSigAtomic left) (CSigAtomic right) = CSigAtomic (mod left right)
  quotRem (CSigAtomic left) (CSigAtomic right) = case quotRem left right of
    (quotient, remainder) -> (CSigAtomic quotient, CSigAtomic remainder)
  divMod (CSigAtomic left) (CSigAtomic right) = case divMod left right of
    (quotient, modulus) -> (CSigAtomic quotient, CSigAtomic modulus)
  toInteger (CSigAtomic value) = toInteger value

instance Eq CSize where
  CSize left == CSize right = left == right

instance Ord CSize where
  compare (CSize left) (CSize right) = compare left right

instance Enum CSize where
  succ (CSize value) = CSize (succ value)
  pred (CSize value) = CSize (pred value)
  toEnum value = CSize (toEnum value)
  fromEnum (CSize value) = fromEnum value
  enumFrom (CSize value) = mapList CSize (enumFrom value)
  enumFromThen (CSize first) (CSize second) = mapList CSize (enumFromThen first second)
  enumFromTo (CSize first) (CSize final) = mapList CSize (enumFromTo first final)
  enumFromThenTo (CSize first) (CSize second) (CSize final) = mapList CSize (enumFromThenTo first second final)

instance Bounded CSize where
  minBound = CSize minBound
  maxBound = CSize maxBound

instance Num CSize where
  CSize left + CSize right = CSize (left + right)
  CSize left - CSize right = CSize (left - right)
  CSize left * CSize right = CSize (left * right)
  negate (CSize value) = CSize (negate value)
  abs (CSize value) = CSize (abs value)
  signum (CSize value) = CSize (signum value)
  fromInteger value = CSize (fromInteger value)

instance Real CSize where
  toRational (CSize value) = toRational value

instance Integral CSize where
  quot (CSize left) (CSize right) = CSize (quot left right)
  rem (CSize left) (CSize right) = CSize (rem left right)
  div (CSize left) (CSize right) = CSize (div left right)
  mod (CSize left) (CSize right) = CSize (mod left right)
  quotRem (CSize left) (CSize right) = case quotRem left right of
    (quotient, remainder) -> (CSize quotient, CSize remainder)
  divMod (CSize left) (CSize right) = case divMod left right of
    (quotient, modulus) -> (CSize quotient, CSize modulus)
  toInteger (CSize value) = toInteger value

instance Eq CTime where
  CTime left == CTime right = left == right

instance Ord CTime where
  compare (CTime left) (CTime right) = compare left right

instance Enum CTime where
  succ (CTime value) = CTime (succ value)
  pred (CTime value) = CTime (pred value)
  toEnum value = CTime (toEnum value)
  fromEnum (CTime value) = fromEnum value
  enumFrom (CTime value) = mapList CTime (enumFrom value)
  enumFromThen (CTime first) (CTime second) = mapList CTime (enumFromThen first second)
  enumFromTo (CTime first) (CTime final) = mapList CTime (enumFromTo first final)
  enumFromThenTo (CTime first) (CTime second) (CTime final) = mapList CTime (enumFromThenTo first second final)

instance Bounded CTime where
  minBound = CTime minBound
  maxBound = CTime maxBound

instance Num CTime where
  CTime left + CTime right = CTime (left + right)
  CTime left - CTime right = CTime (left - right)
  CTime left * CTime right = CTime (left * right)
  negate (CTime value) = CTime (negate value)
  abs (CTime value) = CTime (abs value)
  signum (CTime value) = CTime (signum value)
  fromInteger value = CTime (fromInteger value)

instance Real CTime where
  toRational (CTime value) = toRational value

instance Integral CTime where
  quot (CTime left) (CTime right) = CTime (quot left right)
  rem (CTime left) (CTime right) = CTime (rem left right)
  div (CTime left) (CTime right) = CTime (div left right)
  mod (CTime left) (CTime right) = CTime (mod left right)
  quotRem (CTime left) (CTime right) = case quotRem left right of
    (quotient, remainder) -> (CTime quotient, CTime remainder)
  divMod (CTime left) (CTime right) = case divMod left right of
    (quotient, modulus) -> (CTime quotient, CTime modulus)
  toInteger (CTime value) = toInteger value

instance Eq CUChar where
  CUChar left == CUChar right = left == right

instance Ord CUChar where
  compare (CUChar left) (CUChar right) = compare left right

instance Enum CUChar where
  succ (CUChar value) = CUChar (succ value)
  pred (CUChar value) = CUChar (pred value)
  toEnum value = CUChar (toEnum value)
  fromEnum (CUChar value) = fromEnum value
  enumFrom (CUChar value) = mapList CUChar (enumFrom value)
  enumFromThen (CUChar first) (CUChar second) = mapList CUChar (enumFromThen first second)
  enumFromTo (CUChar first) (CUChar final) = mapList CUChar (enumFromTo first final)
  enumFromThenTo (CUChar first) (CUChar second) (CUChar final) = mapList CUChar (enumFromThenTo first second final)

instance Bounded CUChar where
  minBound = CUChar minBound
  maxBound = CUChar maxBound

instance Num CUChar where
  CUChar left + CUChar right = CUChar (left + right)
  CUChar left - CUChar right = CUChar (left - right)
  CUChar left * CUChar right = CUChar (left * right)
  negate (CUChar value) = CUChar (negate value)
  abs (CUChar value) = CUChar (abs value)
  signum (CUChar value) = CUChar (signum value)
  fromInteger value = CUChar (fromInteger value)

instance Real CUChar where
  toRational (CUChar value) = toRational value

instance Integral CUChar where
  quot (CUChar left) (CUChar right) = CUChar (quot left right)
  rem (CUChar left) (CUChar right) = CUChar (rem left right)
  div (CUChar left) (CUChar right) = CUChar (div left right)
  mod (CUChar left) (CUChar right) = CUChar (mod left right)
  quotRem (CUChar left) (CUChar right) = case quotRem left right of
    (quotient, remainder) -> (CUChar quotient, CUChar remainder)
  divMod (CUChar left) (CUChar right) = case divMod left right of
    (quotient, modulus) -> (CUChar quotient, CUChar modulus)
  toInteger (CUChar value) = toInteger value

instance Eq CUInt where
  CUInt left == CUInt right = left == right

instance Ord CUInt where
  compare (CUInt left) (CUInt right) = compare left right

instance Enum CUInt where
  succ (CUInt value) = CUInt (succ value)
  pred (CUInt value) = CUInt (pred value)
  toEnum value = CUInt (toEnum value)
  fromEnum (CUInt value) = fromEnum value
  enumFrom (CUInt value) = mapList CUInt (enumFrom value)
  enumFromThen (CUInt first) (CUInt second) = mapList CUInt (enumFromThen first second)
  enumFromTo (CUInt first) (CUInt final) = mapList CUInt (enumFromTo first final)
  enumFromThenTo (CUInt first) (CUInt second) (CUInt final) = mapList CUInt (enumFromThenTo first second final)

instance Bounded CUInt where
  minBound = CUInt minBound
  maxBound = CUInt maxBound

instance Num CUInt where
  CUInt left + CUInt right = CUInt (left + right)
  CUInt left - CUInt right = CUInt (left - right)
  CUInt left * CUInt right = CUInt (left * right)
  negate (CUInt value) = CUInt (negate value)
  abs (CUInt value) = CUInt (abs value)
  signum (CUInt value) = CUInt (signum value)
  fromInteger value = CUInt (fromInteger value)

instance Real CUInt where
  toRational (CUInt value) = toRational value

instance Integral CUInt where
  quot (CUInt left) (CUInt right) = CUInt (quot left right)
  rem (CUInt left) (CUInt right) = CUInt (rem left right)
  div (CUInt left) (CUInt right) = CUInt (div left right)
  mod (CUInt left) (CUInt right) = CUInt (mod left right)
  quotRem (CUInt left) (CUInt right) = case quotRem left right of
    (quotient, remainder) -> (CUInt quotient, CUInt remainder)
  divMod (CUInt left) (CUInt right) = case divMod left right of
    (quotient, modulus) -> (CUInt quotient, CUInt modulus)
  toInteger (CUInt value) = toInteger value

instance Eq CUIntMax where
  CUIntMax left == CUIntMax right = left == right

instance Ord CUIntMax where
  compare (CUIntMax left) (CUIntMax right) = compare left right

instance Enum CUIntMax where
  succ (CUIntMax value) = CUIntMax (succ value)
  pred (CUIntMax value) = CUIntMax (pred value)
  toEnum value = CUIntMax (toEnum value)
  fromEnum (CUIntMax value) = fromEnum value
  enumFrom (CUIntMax value) = mapList CUIntMax (enumFrom value)
  enumFromThen (CUIntMax first) (CUIntMax second) = mapList CUIntMax (enumFromThen first second)
  enumFromTo (CUIntMax first) (CUIntMax final) = mapList CUIntMax (enumFromTo first final)
  enumFromThenTo (CUIntMax first) (CUIntMax second) (CUIntMax final) = mapList CUIntMax (enumFromThenTo first second final)

instance Bounded CUIntMax where
  minBound = CUIntMax minBound
  maxBound = CUIntMax maxBound

instance Num CUIntMax where
  CUIntMax left + CUIntMax right = CUIntMax (left + right)
  CUIntMax left - CUIntMax right = CUIntMax (left - right)
  CUIntMax left * CUIntMax right = CUIntMax (left * right)
  negate (CUIntMax value) = CUIntMax (negate value)
  abs (CUIntMax value) = CUIntMax (abs value)
  signum (CUIntMax value) = CUIntMax (signum value)
  fromInteger value = CUIntMax (fromInteger value)

instance Real CUIntMax where
  toRational (CUIntMax value) = toRational value

instance Integral CUIntMax where
  quot (CUIntMax left) (CUIntMax right) = CUIntMax (quot left right)
  rem (CUIntMax left) (CUIntMax right) = CUIntMax (rem left right)
  div (CUIntMax left) (CUIntMax right) = CUIntMax (div left right)
  mod (CUIntMax left) (CUIntMax right) = CUIntMax (mod left right)
  quotRem (CUIntMax left) (CUIntMax right) = case quotRem left right of
    (quotient, remainder) -> (CUIntMax quotient, CUIntMax remainder)
  divMod (CUIntMax left) (CUIntMax right) = case divMod left right of
    (quotient, modulus) -> (CUIntMax quotient, CUIntMax modulus)
  toInteger (CUIntMax value) = toInteger value

instance Eq CUIntPtr where
  CUIntPtr left == CUIntPtr right = left == right

instance Ord CUIntPtr where
  compare (CUIntPtr left) (CUIntPtr right) = compare left right

instance Enum CUIntPtr where
  succ (CUIntPtr value) = CUIntPtr (succ value)
  pred (CUIntPtr value) = CUIntPtr (pred value)
  toEnum value = CUIntPtr (toEnum value)
  fromEnum (CUIntPtr value) = fromEnum value
  enumFrom (CUIntPtr value) = mapList CUIntPtr (enumFrom value)
  enumFromThen (CUIntPtr first) (CUIntPtr second) = mapList CUIntPtr (enumFromThen first second)
  enumFromTo (CUIntPtr first) (CUIntPtr final) = mapList CUIntPtr (enumFromTo first final)
  enumFromThenTo (CUIntPtr first) (CUIntPtr second) (CUIntPtr final) = mapList CUIntPtr (enumFromThenTo first second final)

instance Bounded CUIntPtr where
  minBound = CUIntPtr minBound
  maxBound = CUIntPtr maxBound

instance Num CUIntPtr where
  CUIntPtr left + CUIntPtr right = CUIntPtr (left + right)
  CUIntPtr left - CUIntPtr right = CUIntPtr (left - right)
  CUIntPtr left * CUIntPtr right = CUIntPtr (left * right)
  negate (CUIntPtr value) = CUIntPtr (negate value)
  abs (CUIntPtr value) = CUIntPtr (abs value)
  signum (CUIntPtr value) = CUIntPtr (signum value)
  fromInteger value = CUIntPtr (fromInteger value)

instance Real CUIntPtr where
  toRational (CUIntPtr value) = toRational value

instance Integral CUIntPtr where
  quot (CUIntPtr left) (CUIntPtr right) = CUIntPtr (quot left right)
  rem (CUIntPtr left) (CUIntPtr right) = CUIntPtr (rem left right)
  div (CUIntPtr left) (CUIntPtr right) = CUIntPtr (div left right)
  mod (CUIntPtr left) (CUIntPtr right) = CUIntPtr (mod left right)
  quotRem (CUIntPtr left) (CUIntPtr right) = case quotRem left right of
    (quotient, remainder) -> (CUIntPtr quotient, CUIntPtr remainder)
  divMod (CUIntPtr left) (CUIntPtr right) = case divMod left right of
    (quotient, modulus) -> (CUIntPtr quotient, CUIntPtr modulus)
  toInteger (CUIntPtr value) = toInteger value

instance Eq CULLong where
  CULLong left == CULLong right = left == right

instance Ord CULLong where
  compare (CULLong left) (CULLong right) = compare left right

instance Enum CULLong where
  succ (CULLong value) = CULLong (succ value)
  pred (CULLong value) = CULLong (pred value)
  toEnum value = CULLong (toEnum value)
  fromEnum (CULLong value) = fromEnum value
  enumFrom (CULLong value) = mapList CULLong (enumFrom value)
  enumFromThen (CULLong first) (CULLong second) = mapList CULLong (enumFromThen first second)
  enumFromTo (CULLong first) (CULLong final) = mapList CULLong (enumFromTo first final)
  enumFromThenTo (CULLong first) (CULLong second) (CULLong final) = mapList CULLong (enumFromThenTo first second final)

instance Bounded CULLong where
  minBound = CULLong minBound
  maxBound = CULLong maxBound

instance Num CULLong where
  CULLong left + CULLong right = CULLong (left + right)
  CULLong left - CULLong right = CULLong (left - right)
  CULLong left * CULLong right = CULLong (left * right)
  negate (CULLong value) = CULLong (negate value)
  abs (CULLong value) = CULLong (abs value)
  signum (CULLong value) = CULLong (signum value)
  fromInteger value = CULLong (fromInteger value)

instance Real CULLong where
  toRational (CULLong value) = toRational value

instance Integral CULLong where
  quot (CULLong left) (CULLong right) = CULLong (quot left right)
  rem (CULLong left) (CULLong right) = CULLong (rem left right)
  div (CULLong left) (CULLong right) = CULLong (div left right)
  mod (CULLong left) (CULLong right) = CULLong (mod left right)
  quotRem (CULLong left) (CULLong right) = case quotRem left right of
    (quotient, remainder) -> (CULLong quotient, CULLong remainder)
  divMod (CULLong left) (CULLong right) = case divMod left right of
    (quotient, modulus) -> (CULLong quotient, CULLong modulus)
  toInteger (CULLong value) = toInteger value

instance Eq CULong where
  CULong left == CULong right = left == right

instance Ord CULong where
  compare (CULong left) (CULong right) = compare left right

instance Enum CULong where
  succ (CULong value) = CULong (succ value)
  pred (CULong value) = CULong (pred value)
  toEnum value = CULong (toEnum value)
  fromEnum (CULong value) = fromEnum value
  enumFrom (CULong value) = mapList CULong (enumFrom value)
  enumFromThen (CULong first) (CULong second) = mapList CULong (enumFromThen first second)
  enumFromTo (CULong first) (CULong final) = mapList CULong (enumFromTo first final)
  enumFromThenTo (CULong first) (CULong second) (CULong final) = mapList CULong (enumFromThenTo first second final)

instance Bounded CULong where
  minBound = CULong minBound
  maxBound = CULong maxBound

instance Num CULong where
  CULong left + CULong right = CULong (left + right)
  CULong left - CULong right = CULong (left - right)
  CULong left * CULong right = CULong (left * right)
  negate (CULong value) = CULong (negate value)
  abs (CULong value) = CULong (abs value)
  signum (CULong value) = CULong (signum value)
  fromInteger value = CULong (fromInteger value)

instance Real CULong where
  toRational (CULong value) = toRational value

instance Integral CULong where
  quot (CULong left) (CULong right) = CULong (quot left right)
  rem (CULong left) (CULong right) = CULong (rem left right)
  div (CULong left) (CULong right) = CULong (div left right)
  mod (CULong left) (CULong right) = CULong (mod left right)
  quotRem (CULong left) (CULong right) = case quotRem left right of
    (quotient, remainder) -> (CULong quotient, CULong remainder)
  divMod (CULong left) (CULong right) = case divMod left right of
    (quotient, modulus) -> (CULong quotient, CULong modulus)
  toInteger (CULong value) = toInteger value

instance Eq CUSeconds where
  CUSeconds left == CUSeconds right = left == right

instance Ord CUSeconds where
  compare (CUSeconds left) (CUSeconds right) = compare left right

instance Enum CUSeconds where
  succ (CUSeconds value) = CUSeconds (succ value)
  pred (CUSeconds value) = CUSeconds (pred value)
  toEnum value = CUSeconds (toEnum value)
  fromEnum (CUSeconds value) = fromEnum value
  enumFrom (CUSeconds value) = mapList CUSeconds (enumFrom value)
  enumFromThen (CUSeconds first) (CUSeconds second) = mapList CUSeconds (enumFromThen first second)
  enumFromTo (CUSeconds first) (CUSeconds final) = mapList CUSeconds (enumFromTo first final)
  enumFromThenTo (CUSeconds first) (CUSeconds second) (CUSeconds final) = mapList CUSeconds (enumFromThenTo first second final)

instance Bounded CUSeconds where
  minBound = CUSeconds minBound
  maxBound = CUSeconds maxBound

instance Num CUSeconds where
  CUSeconds left + CUSeconds right = CUSeconds (left + right)
  CUSeconds left - CUSeconds right = CUSeconds (left - right)
  CUSeconds left * CUSeconds right = CUSeconds (left * right)
  negate (CUSeconds value) = CUSeconds (negate value)
  abs (CUSeconds value) = CUSeconds (abs value)
  signum (CUSeconds value) = CUSeconds (signum value)
  fromInteger value = CUSeconds (fromInteger value)

instance Real CUSeconds where
  toRational (CUSeconds value) = toRational value

instance Integral CUSeconds where
  quot (CUSeconds left) (CUSeconds right) = CUSeconds (quot left right)
  rem (CUSeconds left) (CUSeconds right) = CUSeconds (rem left right)
  div (CUSeconds left) (CUSeconds right) = CUSeconds (div left right)
  mod (CUSeconds left) (CUSeconds right) = CUSeconds (mod left right)
  quotRem (CUSeconds left) (CUSeconds right) = case quotRem left right of
    (quotient, remainder) -> (CUSeconds quotient, CUSeconds remainder)
  divMod (CUSeconds left) (CUSeconds right) = case divMod left right of
    (quotient, modulus) -> (CUSeconds quotient, CUSeconds modulus)
  toInteger (CUSeconds value) = toInteger value

instance Eq CUShort where
  CUShort left == CUShort right = left == right

instance Ord CUShort where
  compare (CUShort left) (CUShort right) = compare left right

instance Enum CUShort where
  succ (CUShort value) = CUShort (succ value)
  pred (CUShort value) = CUShort (pred value)
  toEnum value = CUShort (toEnum value)
  fromEnum (CUShort value) = fromEnum value
  enumFrom (CUShort value) = mapList CUShort (enumFrom value)
  enumFromThen (CUShort first) (CUShort second) = mapList CUShort (enumFromThen first second)
  enumFromTo (CUShort first) (CUShort final) = mapList CUShort (enumFromTo first final)
  enumFromThenTo (CUShort first) (CUShort second) (CUShort final) = mapList CUShort (enumFromThenTo first second final)

instance Bounded CUShort where
  minBound = CUShort minBound
  maxBound = CUShort maxBound

instance Num CUShort where
  CUShort left + CUShort right = CUShort (left + right)
  CUShort left - CUShort right = CUShort (left - right)
  CUShort left * CUShort right = CUShort (left * right)
  negate (CUShort value) = CUShort (negate value)
  abs (CUShort value) = CUShort (abs value)
  signum (CUShort value) = CUShort (signum value)
  fromInteger value = CUShort (fromInteger value)

instance Real CUShort where
  toRational (CUShort value) = toRational value

instance Integral CUShort where
  quot (CUShort left) (CUShort right) = CUShort (quot left right)
  rem (CUShort left) (CUShort right) = CUShort (rem left right)
  div (CUShort left) (CUShort right) = CUShort (div left right)
  mod (CUShort left) (CUShort right) = CUShort (mod left right)
  quotRem (CUShort left) (CUShort right) = case quotRem left right of
    (quotient, remainder) -> (CUShort quotient, CUShort remainder)
  divMod (CUShort left) (CUShort right) = case divMod left right of
    (quotient, modulus) -> (CUShort quotient, CUShort modulus)
  toInteger (CUShort value) = toInteger value

instance Eq CWchar where
  CWchar left == CWchar right = left == right

instance Ord CWchar where
  compare (CWchar left) (CWchar right) = compare left right

instance Enum CWchar where
  succ (CWchar value) = CWchar (succ value)
  pred (CWchar value) = CWchar (pred value)
  toEnum value = CWchar (toEnum value)
  fromEnum (CWchar value) = fromEnum value
  enumFrom (CWchar value) = mapList CWchar (enumFrom value)
  enumFromThen (CWchar first) (CWchar second) = mapList CWchar (enumFromThen first second)
  enumFromTo (CWchar first) (CWchar final) = mapList CWchar (enumFromTo first final)
  enumFromThenTo (CWchar first) (CWchar second) (CWchar final) = mapList CWchar (enumFromThenTo first second final)

instance Bounded CWchar where
  minBound = CWchar minBound
  maxBound = CWchar maxBound

instance Num CWchar where
  CWchar left + CWchar right = CWchar (left + right)
  CWchar left - CWchar right = CWchar (left - right)
  CWchar left * CWchar right = CWchar (left * right)
  negate (CWchar value) = CWchar (negate value)
  abs (CWchar value) = CWchar (abs value)
  signum (CWchar value) = CWchar (signum value)
  fromInteger value = CWchar (fromInteger value)

instance Real CWchar where
  toRational (CWchar value) = toRational value

instance Integral CWchar where
  quot (CWchar left) (CWchar right) = CWchar (quot left right)
  rem (CWchar left) (CWchar right) = CWchar (rem left right)
  div (CWchar left) (CWchar right) = CWchar (div left right)
  mod (CWchar left) (CWchar right) = CWchar (mod left right)
  quotRem (CWchar left) (CWchar right) = case quotRem left right of
    (quotient, remainder) -> (CWchar quotient, CWchar remainder)
  divMod (CWchar left) (CWchar right) = case divMod left right of
    (quotient, modulus) -> (CWchar quotient, CWchar modulus)
  toInteger (CWchar value) = toInteger value

instance Eq CFloat where
  CFloat left == CFloat right = left == right

instance Ord CFloat where
  compare (CFloat left) (CFloat right) = compare left right

instance Num CFloat where
  CFloat left + CFloat right = CFloat (left + right)
  CFloat left - CFloat right = CFloat (left - right)
  CFloat left * CFloat right = CFloat (left * right)
  negate (CFloat value) = CFloat (negate value)
  abs (CFloat value) = CFloat (abs value)
  signum (CFloat value) = CFloat (signum value)
  fromInteger value = CFloat (fromInteger value)

instance Eq CDouble where
  CDouble left == CDouble right = left == right

instance Ord CDouble where
  compare (CDouble left) (CDouble right) = compare left right

instance Num CDouble where
  CDouble left + CDouble right = CDouble (left + right)
  CDouble left - CDouble right = CDouble (left - right)
  CDouble left * CDouble right = CDouble (left * right)
  negate (CDouble value) = CDouble (negate value)
  abs (CDouble value) = CDouble (abs value)
  signum (CDouble value) = CDouble (signum value)
  fromInteger value = CDouble (fromInteger value)
