{-# LANGUAGE OverloadedStrings #-}

-- | Select and validate the effective mechanism for a deriving request.
-- Keeping extension-sensitive policy here makes a checked deriving plan
-- independent of source defaults and prevents System FC from choosing a
-- Haskell-level strategy.
module Aihc.Tc.Deriving.Strategy
  ( checkDerivingStrategy,
  )
where

import Aihc.Parser.Syntax
  ( DerivingStrategy (..),
    Extension (..),
    SourceSpan,
  )
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Annotations (TcDerivingStrategy (..))
import Aihc.Tc.Deriving.References (DerivingReferences (..))
import Aihc.Tc.Env (TyConFlavor (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (TvKindEnv, checkSurfaceType)
import Aihc.Tc.Monad (TcM, emitError, emitWarning, getDerivingReferences)
import Aihc.Tc.Types (TcType)
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as T

checkDerivingStrategy :: [Extension] -> TyConFlavor -> Text -> Maybe (Text, Text) -> TvKindEnv -> TcType -> SourceSpan -> Maybe DerivingStrategy -> TcM TcDerivingStrategy
checkDerivingStrategy extensions targetFlavor className classOrigin tvEnv targetKind sourceSpan strategy = do
  stockPackages <- derivingStockPackages <$> getDerivingReferences
  let isStock = isStockClassOrigin stockPackages classOrigin
  case strategy of
    Nothing -> selectDefaultDerivingStrategy extensions targetFlavor className isStock sourceSpan
    Just DerivingStock -> do
      checkStockDeriving extensions className isStock sourceSpan
      pure TcDerivingStock
    Just DerivingAnyclass -> do
      requireDerivingExtension extensions DeriveAnyClass "anyclass deriving" sourceSpan
      pure TcDerivingAnyclass
    Just DerivingNewtype -> do
      requireDerivingExtension extensions GeneralizedNewtypeDeriving "newtype deriving" sourceSpan
      unless (targetFlavor == NewtypeTyCon) $
        emitError sourceSpan (OtherError "newtype deriving requires a newtype instance target")
      pure TcDerivingNewtype
    Just (DerivingVia viaType) -> do
      requireDerivingExtension extensions DerivingViaExtension "via deriving" sourceSpan
      TcDerivingVia <$> checkSurfaceType tvEnv viaType targetKind

selectDefaultDerivingStrategy :: [Extension] -> TyConFlavor -> Text -> Bool -> SourceSpan -> TcM TcDerivingStrategy
selectDefaultDerivingStrategy extensions targetFlavor className isStock sourceSpan =
  case (isStock, stockDerivingRequirement className) of
    (True, Just requiredExtension)
      | maybe True (`elem` extensions) requiredExtension -> pure TcDerivingStock
    _
      | DeriveAnyClass `elem` extensions -> do
          when (targetFlavor == NewtypeTyCon && GeneralizedNewtypeDeriving `elem` extensions) $
            emitWarning sourceSpan (OtherError (derivingDefaultsWarning className))
          pure TcDerivingAnyclass
      | targetFlavor == NewtypeTyCon,
        GeneralizedNewtypeDeriving `elem` extensions ->
          pure TcDerivingNewtype
      | otherwise -> do
          emitError sourceSpan (OtherError (defaultStrategyError targetFlavor className))
          pure TcDerivingStock

checkStockDeriving :: [Extension] -> Text -> Bool -> SourceSpan -> TcM ()
checkStockDeriving extensions className isStock sourceSpan
  | not isStock =
      emitError sourceSpan (OtherError "stock deriving requires a standard class")
  | otherwise =
      case stockDerivingRequirement className of
        Nothing ->
          emitError sourceSpan (OtherError ("stock deriving is not available for class " <> T.unpack className))
        Just Nothing -> pure ()
        Just (Just extension) ->
          requireDerivingExtension extensions extension ("stock deriving for " <> T.unpack className) sourceSpan

-- | Whether a class comes from a package whose classes GHC's stock
-- deriving mechanisms know about. The configuration names those packages.
isStockClassOrigin :: [PackageId] -> Maybe (Text, Text) -> Bool
isStockClassOrigin stockPackages (Just (packageId, _)) = PackageId packageId `elem` stockPackages
isStockClassOrigin _ _ = False

-- | Extensions required by GHC's stock deriving mechanisms. A @Nothing@
-- requirement denotes the six classes available for ordinary Haskell data
-- declarations without an extension.
stockDerivingRequirement :: Text -> Maybe (Maybe Extension)
stockDerivingRequirement className =
  case className of
    "Eq" -> Just Nothing
    "Ord" -> Just Nothing
    "Enum" -> Just Nothing
    "Bounded" -> Just Nothing
    "Ix" -> Just Nothing
    "Show" -> Just Nothing
    "Read" -> Just Nothing
    "Data" -> Just (Just DeriveDataTypeable)
    "Typeable" -> Just (Just DeriveDataTypeable)
    "Foldable" -> Just (Just DeriveFoldable)
    "Functor" -> Just (Just DeriveFunctor)
    "Generic" -> Just (Just DeriveGeneric)
    "Generic1" -> Just (Just DeriveGeneric)
    "Lift" -> Just (Just DeriveLift)
    "Traversable" -> Just (Just DeriveTraversable)
    _ -> Nothing

requireDerivingExtension :: [Extension] -> Extension -> String -> SourceSpan -> TcM ()
requireDerivingExtension extensions extension mechanism sourceSpan =
  unless (extension `elem` extensions) $
    emitError sourceSpan (OtherError (mechanism <> " requires " <> derivingExtensionName extension))

derivingExtensionName :: Extension -> String
derivingExtensionName DerivingViaExtension = "DerivingVia"
derivingExtensionName extension = show extension

derivingDefaultsWarning :: Text -> String
derivingDefaultsWarning className =
  "both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled; defaulting to anyclass for "
    <> T.unpack className

defaultStrategyError :: TyConFlavor -> Text -> String
defaultStrategyError targetFlavor className =
  "cannot select a deriving strategy for "
    <> T.unpack className
    <> "; enable DeriveAnyClass"
    <> if targetFlavor == NewtypeTyCon
      then " or GeneralizedNewtypeDeriving, or use an explicit strategy"
      else ", or use an explicit strategy"
