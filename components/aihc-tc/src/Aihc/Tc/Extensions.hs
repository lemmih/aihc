-- | Module extension evaluation for type checks.
module Aihc.Tc.Extensions (effectiveModuleExtensions) where

import Aihc.Parser.Syntax (Extension (..), ExtensionSetting (..), applyExtensionSetting, applyImpliedExtensions)

-- | Apply pragmas in source order. Each enabled extension adds its implied
-- extensions immediately. A later pragma can disable these extensions.
effectiveModuleExtensions :: [ExtensionSetting] -> [Extension]
effectiveModuleExtensions = foldl step [MonoLocalBinds, MonomorphismRestriction]
  where
    step extensions setting =
      case setting of
        EnableExtension _ -> applyImpliedExtensions (applyExtensionSetting setting extensions)
        DisableExtension _ -> applyExtensionSetting setting extensions
