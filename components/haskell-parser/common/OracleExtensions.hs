module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import ExtensionSupport (ExtensionSpec (..))
import GHC.LanguageExtensions.Type (Extension (ParallelListComp, TypeApplications))

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    "TypeApplications" -> pure [TypeApplications]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
