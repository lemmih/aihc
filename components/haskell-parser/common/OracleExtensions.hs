module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import qualified Data.Text as T
import ExtensionSupport (ExtensionSpec (..))
import qualified GHC.LanguageExtensions.Type as GHC
import GhcOracle (toGhcExtension)
import Parser.Ast (parseExtensionName)

resolveOracleExtensions :: ExtensionSpec -> IO [GHC.Extension]
resolveOracleExtensions spec =
  case extName spec of
    "Haskell2010" -> pure []
    "Haskell98" -> pure []
    "FunctionalDependencies" -> resolveMany ["FunctionalDependencies", "MultiParamTypeClasses"]
    "PatternSynonyms" -> resolveMany ["PatternSynonyms", "ExplicitNamespaces"]
    name -> resolveMany [name]
  where
    resolveMany = fmap concat . mapM resolveOne
    resolveOne name =
      case parseExtensionName (T.pack name) >>= toGhcExtension of
        Just ghcExt -> pure [ghcExt]
        Nothing -> fail ("Unsupported extension mapping: " <> name)
