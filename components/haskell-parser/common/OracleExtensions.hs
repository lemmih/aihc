module OracleExtensions
  ( resolveOracleExtensions,
  )
where

import ExtensionSupport (ExtensionSpec (..))
import GHC.LanguageExtensions.Type
  ( Extension
      ( BangPatterns,
        BinaryLiterals,
        DerivingStrategies,
        DoAndIfThenElse,
        EmptyCase,
        EmptyDataDecls,
        ExplicitForAll,
        ExplicitNamespaces,
        FunctionalDependencies,
        GADTs,
        HexFloatLiterals,
        ImportQualifiedPost,
        InstanceSigs,
        KindSignatures,
        LambdaCase,
        MultiParamTypeClasses,
        NamedFieldPuns,
        NumericUnderscores,
        PackageImports,
        ParallelListComp,
        QuasiQuotes,
        TypeApplications,
        ViewPatterns
      ),
  )

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    "QuasiQuotes" -> pure [QuasiQuotes]
    "TypeApplications" -> pure [TypeApplications]
    "LambdaCase" -> pure [LambdaCase]
    "ViewPatterns" -> pure [ViewPatterns]
    "BinaryLiterals" -> pure [BinaryLiterals]
    "HexFloatLiterals" -> pure [HexFloatLiterals]
    "NumericUnderscores" -> pure [NumericUnderscores]
    "EmptyCase" -> pure [EmptyCase]
    "DoAndIfThenElse" -> pure [DoAndIfThenElse]
    "GADTs" -> pure [GADTs]
    "PackageImports" -> pure [PackageImports]
    "ExplicitNamespaces" -> pure [ExplicitNamespaces]
    "ImportQualifiedPost" -> pure [ImportQualifiedPost]
    "BangPatterns" -> pure [BangPatterns]
    "DerivingStrategies" -> pure [DerivingStrategies]
    "EmptyDataDecls" -> pure [EmptyDataDecls]
    "ExplicitForAll" -> pure [ExplicitForAll]
    "FunctionalDependencies" -> pure [FunctionalDependencies, MultiParamTypeClasses]
    "InstanceSigs" -> pure [InstanceSigs]
    "KindSignatures" -> pure [KindSignatures]
    "MultiParamTypeClasses" -> pure [MultiParamTypeClasses]
    "NamedFieldPuns" -> pure [NamedFieldPuns]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
