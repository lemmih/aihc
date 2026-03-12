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
        ExistentialQuantification,
        ExplicitForAll,
        ExplicitLevelImports,
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
        NamedWildCards,
        NumericUnderscores,
        PackageImports,
        ParallelListComp,
        PatternGuards,
        QuasiQuotes,
        RoleAnnotations,
        StandaloneDeriving,
        StandaloneKindSignatures,
        TupleSections,
        TypeApplications,
        TypeOperators,
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
    "ExistentialQuantification" -> pure [ExistentialQuantification]
    "ExplicitForAll" -> pure [ExplicitForAll]
    "ExplicitLevelImports" -> pure [ExplicitLevelImports]
    "FunctionalDependencies" -> pure [FunctionalDependencies, MultiParamTypeClasses]
    "InstanceSigs" -> pure [InstanceSigs]
    "KindSignatures" -> pure [KindSignatures]
    "MultiParamTypeClasses" -> pure [MultiParamTypeClasses]
    "NamedFieldPuns" -> pure [NamedFieldPuns]
    "NamedWildCards" -> pure [NamedWildCards]
    "PatternGuards" -> pure [PatternGuards]
    "RoleAnnotations" -> pure [RoleAnnotations]
    "StandaloneDeriving" -> pure [StandaloneDeriving]
    "StandaloneKindSignatures" -> pure [StandaloneKindSignatures]
    "TupleSections" -> pure [TupleSections]
    "TypeOperators" -> pure [TypeOperators]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
