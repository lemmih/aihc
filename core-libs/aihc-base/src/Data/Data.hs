{-# LANGUAGE RankNTypes #-}

module Data.Data
  ( module Data.Typeable,
    Data (..),
    Constr,
    DataType,
    Fixity (..),
    constrFields,
    constrFixity,
    constrIndex,
    dataTypeConstrs,
    dataTypeName,
    mkConstr,
    mkDataType,
    mkNoRepType,
    showConstr,
  )
where

import Data.Maybe (Maybe (..))
import Data.Typeable
import GHC.Base (String)
import GHC.Err (errorWithoutStackTrace)
import GHC.Int (Int)
import GHC.Num ((+))
import GHC.Show (Show (..))
import GHC.Types (Bool (..), Char)
import GHC.Word (Word, Word8)

-- | Generic operations on a data type.
class (Typeable a) => Data a where
  gfoldl ::
    (forall d b. (Data d) => c (d -> b) -> d -> c b) ->
    (forall g. g -> c g) ->
    a ->
    c a
  gfoldl _ z = z
  gunfold ::
    (forall b r. (Data b) => c (b -> r) -> c r) ->
    (forall r. r -> c r) ->
    Constr ->
    c a
  toConstr :: a -> Constr
  dataTypeOf :: a -> DataType

  -- The casts carry no Typeable context: Typeable is not kind polymorphic
  -- here, and 'gcast1' and 'gcast2' need none.
  dataCast1 :: (forall d. (Data d) => c (t d)) -> Maybe (c a)
  dataCast1 _ = Nothing
  dataCast2 :: (forall d e. (Data d, Data e) => c (t d e)) -> Maybe (c a)
  dataCast2 _ = Nothing

-- | The fixity of a data constructor.
data Fixity = Prefix | Infix

-- | The description of a data type.
data DataType = DataType String [Constr]

-- | The description of one data constructor.
data Constr = Constr String [String] Fixity Int

-- | Make a data type that lists its constructors.
mkDataType :: String -> [Constr] -> DataType
mkDataType = DataType

-- | Make a data type that has no generic representation.
mkNoRepType :: String -> DataType
mkNoRepType name = DataType name []

-- | Make a constructor description.
-- The standin gives the constructor the index that comes after the
-- constructors that the data type already lists. A no-rep data type lists no
-- constructors, thus its first constructor gets index 1.
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dataType name fields fixity =
  Constr name fields fixity (countConstrs (dataTypeConstrs dataType) + 1)

countConstrs :: [Constr] -> Int
countConstrs [] = 0
countConstrs (_ : rest) = 1 + countConstrs rest

-- | Give the name of a data type.
dataTypeName :: DataType -> String
dataTypeName (DataType name _) = name

-- | Give the constructors that a data type lists.
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs (DataType _ constrs) = constrs

-- | Give the name of a constructor.
showConstr :: Constr -> String
showConstr (Constr name _ _ _) = name

-- | Give the field names of a constructor.
constrFields :: Constr -> [String]
constrFields (Constr _ fields _ _) = fields

-- | Give the fixity of a constructor.
constrFixity :: Constr -> Fixity
constrFixity (Constr _ _ fixity _) = fixity

-- | Give the index of a constructor in its data type.
constrIndex :: Constr -> Int
constrIndex (Constr _ _ _ index) = index

instance (Data a) => Data [a] where
  gfoldl _ z [] = z []
  gfoldl f z (x : xs) = z (:) `f` x `f` xs
  toConstr [] = nilConstr
  toConstr (_ : _) = consConstr
  gunfold k z c = case constrIndex c of
    1 -> z []
    2 -> k (k (z (:)))
    _ -> errorWithoutStackTrace "Data.Data.gunfold(List)"
  dataTypeOf _ = listDataType

nilConstr :: Constr
nilConstr = mkConstr listDataType "[]" [] Prefix

consConstr :: Constr
consConstr = mkConstr (DataType "Prelude.[]" [nilConstr]) "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr, consConstr]

instance Data Bool where
  toConstr False = falseConstr
  toConstr True = trueConstr
  gunfold _ z c = case constrIndex c of
    1 -> z False
    2 -> z True
    _ -> errorWithoutStackTrace "Data.Data.gunfold(Bool)"
  dataTypeOf _ = boolDataType

falseConstr :: Constr
falseConstr = mkConstr boolDataType "False" [] Prefix

trueConstr :: Constr
trueConstr = mkConstr (DataType "Prelude.Bool" [falseConstr]) "True" [] Prefix

boolDataType :: DataType
boolDataType = mkDataType "Prelude.Bool" [falseConstr, trueConstr]

-- The primitive types show their value as the constructor name. The
-- standin cannot rebuild a value from a constructor, so gunfold fails.
instance Data Char where
  toConstr x = mkConstr charType ['\'', x, '\''] [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Char)"
  dataTypeOf _ = charType

charType :: DataType
charType = mkNoRepType "Prelude.Char"

instance Data Int where
  toConstr x = mkConstr intType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Int)"
  dataTypeOf _ = intType

intType :: DataType
intType = mkNoRepType "Prelude.Int"

instance Data Word where
  toConstr x = mkConstr wordType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word)"
  dataTypeOf _ = wordType

wordType :: DataType
wordType = mkNoRepType "Prelude.Word"

instance Data Word8 where
  toConstr x = mkConstr word8Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word8)"
  dataTypeOf _ = word8Type

word8Type :: DataType
word8Type = mkNoRepType "Data.Word.Word8"

instance (Data a, Data b) => Data (a, b) where
  gfoldl f z (a, b) = z (,) `f` a `f` b
  gunfold k z c = case constrIndex c of
    1 -> k (k (z (,)))
    _ -> errorWithoutStackTrace "Data.Data.gunfold((,))"
  toConstr _ = pairConstr
  dataTypeOf _ = pairDataType

pairConstr :: Constr
pairConstr = mkConstr (mkNoRepType "Prelude.(,)") "(,)" [] Infix

pairDataType :: DataType
pairDataType = mkDataType "Prelude.(,)" [pairConstr]
