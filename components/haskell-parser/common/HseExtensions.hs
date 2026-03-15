module HseExtensions
  ( toHseExtension,
    fromHseExtension,
    toHseExtensions,
    toHseExtensionSetting,
    toHseExtensionSettings,
    fromExtensionNames,
  )
where

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.Exts as HSE
import qualified Parser.Ast as Ast
import Text.Read (readMaybe)

toHseExtension :: Ast.Extension -> Maybe HSE.Extension
toHseExtension ext =
  HSE.EnableExtension <$> toHseKnownExtension ext

fromHseExtension :: HSE.Extension -> Maybe Ast.Extension
fromHseExtension hseExt =
  case hseExt of
    HSE.EnableExtension known -> Ast.parseExtensionName (T.pack (show known))
    HSE.DisableExtension known -> Ast.parseExtensionName (T.pack (show known))
    HSE.UnknownExtension name -> Ast.parseExtensionName (T.pack name)

toHseExtensions :: [Ast.Extension] -> [HSE.Extension]
toHseExtensions = mapMaybe toHseExtension

toHseExtensionSetting :: Ast.ExtensionSetting -> Maybe HSE.Extension
toHseExtensionSetting setting =
  case setting of
    Ast.EnableExtension ext -> toHseExtension ext
    Ast.DisableExtension ext -> HSE.DisableExtension <$> toHseKnownExtension ext

toHseExtensionSettings :: [Ast.ExtensionSetting] -> [HSE.Extension]
toHseExtensionSettings = mapMaybe toHseExtensionSetting

fromExtensionNames :: [String] -> [HSE.Extension]
fromExtensionNames names =
  toHseExtensionSettings (mapMaybe (Ast.parseExtensionSettingName . T.pack) names)

toHseKnownExtension :: Ast.Extension -> Maybe HSE.KnownExtension
toHseKnownExtension ext =
  listToMaybe [known | name <- hseKnownNameCandidates ext, Just known <- [readMaybe name]]

hseKnownNameCandidates :: Ast.Extension -> [String]
hseKnownNameCandidates ext =
  case ext of
    Ast.CPP -> ["CPP", "Cpp"]
    Ast.GeneralizedNewtypeDeriving -> ["GeneralizedNewtypeDeriving", "GeneralisedNewtypeDeriving"]
    _ -> [T.unpack (Ast.extensionName ext)]
