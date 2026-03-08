module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import ExtensionSupport (ExtensionSpec (..))
import GHC.LanguageExtensions.Type (Extension (ParallelListComp, QuasiQuotes, TypeApplications))

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    "QuasiQuotes" -> pure [QuasiQuotes]
    "TypeApplications" -> pure [TypeApplications]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
