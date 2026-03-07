module Test.Oracle
  ( oracleModuleAstFingerprint,
    oracleModuleAstFingerprintWithExtensions,
    oracleParsesModule,
    oracleParsesModuleWithExtensions,
  )
where

import Data.Text (Text)
import GhcOracle
  ( oracleModuleAstFingerprintWithExtensions,
    oracleParsesModuleWithExtensions,
  )

oracleParsesModule :: Text -> Bool
oracleParsesModule = oracleParsesModuleWithExtensions []

oracleModuleAstFingerprint :: Text -> Either Text Text
oracleModuleAstFingerprint = oracleModuleAstFingerprintWithExtensions []
