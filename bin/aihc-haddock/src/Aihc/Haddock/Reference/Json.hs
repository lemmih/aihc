{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Reference.Json
-- Description : Adapter for mainline Haddock's @--show-interface@ JSON
--
-- Reads the JSON that Haddock 2.32 writes for @--show-interface@. That
-- version writes the JSON to standard error; the caller captures the stream
-- and hands the bytes here. Names arrive as Haddock stable strings of the form
-- @$package$Module$name@.
module Aihc.Haddock.Reference.Json
  ( ReferenceInterface (..),
    ReferenceModule (..),
    decodeReferenceInterface,
    StableName (..),
    parseStableName,
    referenceModuleShortName,
  )
where

import Aihc.Haddock.Model (Fixity, MetaDoc)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data ReferenceInterface = ReferenceInterface
  { referencePackageInfo :: Text,
    referenceLinkEnv :: Map Text Text,
    referenceModules :: [ReferenceModule]
  }
  deriving (Eq, Show)

data ReferenceModule = ReferenceModule
  { -- | The module's stable name, @$package$Module@.
    referenceModuleName :: Text,
    referenceIsSignature :: Bool,
    referenceInfo :: Aeson.Value,
    referenceDocMap :: Map Text MetaDoc,
    referenceArgMap :: Map Text (Map Int MetaDoc),
    referenceExports :: [Text],
    referenceVisibleExports :: [Text],
    referenceOptions :: [Text],
    referenceFixities :: Map Text Fixity
  }
  deriving (Eq, Show)

instance FromJSON ReferenceInterface where
  parseJSON = withObject "ReferenceInterface" $ \obj ->
    ReferenceInterface
      <$> obj .: "package_info"
      <*> obj .:? "link_env" .!= Map.empty
      <*> obj .: "inst_ifaces"

instance FromJSON ReferenceModule where
  parseJSON = withObject "ReferenceModule" $ \obj -> do
    argMap <- obj .:? "arg_map" .!= Map.empty
    argMap' <- traverse parseArgPositions (argMap :: Map Text (Map Text MetaDoc))
    ReferenceModule
      <$> obj .: "module"
      <*> obj .:? "is_sig" .!= False
      <*> obj .:? "info" .!= Aeson.Null
      <*> obj .:? "doc_map" .!= Map.empty
      <*> pure argMap'
      <*> obj .:? "exports" .!= []
      <*> obj .:? "visible_exports" .!= []
      <*> obj .:? "options" .!= []
      <*> obj .:? "fix_map" .!= Map.empty
    where
      parseArgPositions positions =
        Map.fromList
          <$> traverse
            ( \(key, doc) ->
                case readMaybe (T.unpack key) of
                  Just position -> pure (position, doc)
                  Nothing -> fail ("argument position is not a number: " <> T.unpack key)
            )
            (Map.toList positions)

decodeReferenceInterface :: BL.ByteString -> Either String ReferenceInterface
decodeReferenceInterface = Aeson.eitherDecode

-- | A Haddock stable name split into its parts. The name part may itself
-- contain @$@, as operators such as @<$>@ do.
data StableName = StableName
  { stablePackage :: Text,
    stableModule :: Text,
    stableName :: Text
  }
  deriving (Eq, Ord, Show)

parseStableName :: Text -> Maybe StableName
parseStableName text = do
  rest <- T.stripPrefix "$" text
  let (package, afterPackage) = T.breakOn "$" rest
  afterPackage' <- T.stripPrefix "$" afterPackage
  let (modName, afterModule) = T.breakOn "$" afterPackage'
  name <- T.stripPrefix "$" afterModule
  if T.null package || T.null modName || T.null name
    then Nothing
    else Just (StableName package modName name)

-- | @Data.Colour@ for @$colour-2.3.7$Data.Colour@.
referenceModuleShortName :: ReferenceModule -> Text
referenceModuleShortName modu =
  case T.stripPrefix "$" (referenceModuleName modu) of
    Just rest ->
      let (_, afterPackage) = T.breakOn "$" rest
       in fromMaybe (referenceModuleName modu) (T.stripPrefix "$" afterPackage)
    Nothing -> referenceModuleName modu
