module HseExtensions
  ( toHseExtension,
    fromHseExtension,
    toHseExtensions,
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
  HSE.EnableExtension
    <$> listToMaybe [known | name <- hseKnownNameCandidates ext, Just known <- [readMaybe name]]

fromHseExtension :: HSE.Extension -> Maybe Ast.Extension
fromHseExtension hseExt =
  case hseExt of
    HSE.EnableExtension known -> Ast.parseExtensionName (T.pack (show known))
    HSE.DisableExtension known -> Ast.parseExtensionName (T.pack (show known))
    HSE.UnknownExtension name -> Ast.parseExtensionName (T.pack name)

toHseExtensions :: [Ast.Extension] -> [HSE.Extension]
toHseExtensions = mapMaybe toHseExtension

fromExtensionNames :: [String] -> [HSE.Extension]
fromExtensionNames names =
  toHseExtensions (mapMaybe (Ast.parseExtensionName . T.pack) names)

hseKnownNameCandidates :: Ast.Extension -> [String]
hseKnownNameCandidates ext =
  case ext of
    Ast.CPP -> ["CPP", "Cpp"]
    Ast.GeneralizedNewtypeDeriving -> ["GeneralizedNewtypeDeriving", "GeneralisedNewtypeDeriving"]
    _ -> [T.unpack (Ast.extensionName ext)]
