{-# LANGUAGE OverloadedStrings #-}

-- | Wired GHC.Types names for System FC.
module Aihc.Fc.Wired
  ( wiredGhcTypes,
    typeSynonym,
    typeConstructor,
    functionArrowConstructor,
    runtimeRepConstructor,
    levityConstructor,
    constraintName,
    liftedRepName,
    unliftedRepName,
    equalityRep,
    ghcTypesModule,
    primPackageFromScopes,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Data.Maybe (listToMaybe)
import Data.Text (Text)

ghcTypesModule :: Text
ghcTypesModule = "GHC.Types"

-- | The empty tuple representation erases equality evidence.
equalityRep :: PackageId -> Type
equalityRep package =
  TyApp
    (TyCon (wiredGhcTypes package "TupleRep" SortDataConstructor))
    (TyApp (TyCon (wiredGhcTypes package "[]" SortDataConstructor)) (TyCon (runtimeRepConstructor package)))

wiredGhcTypes :: PackageId -> Text -> Sort -> Name
wiredGhcTypes package name sort =
  Name name sort (OriginTop package ghcTypesModule)

typeSynonym :: PackageId -> Type
typeSynonym package =
  TyCon (wiredGhcTypes package "Type" SortSynonym)

typeConstructor :: PackageId -> Name
typeConstructor package =
  wiredGhcTypes package "TYPE" SortTypeConstructor

-- | The function arrow as a type constructor. A saturated application of
-- it is the function type.
functionArrowConstructor :: PackageId -> Name
functionArrowConstructor package =
  wiredGhcTypes package "(->)" SortTypeConstructor

runtimeRepConstructor :: PackageId -> Name
runtimeRepConstructor package =
  wiredGhcTypes package "RuntimeRep" SortTypeConstructor

levityConstructor :: PackageId -> Name
levityConstructor package =
  wiredGhcTypes package "Levity" SortTypeConstructor

constraintName :: PackageId -> Name
constraintName package =
  wiredGhcTypes package "Constraint" SortTypeConstructor

liftedRepName :: PackageId -> Name
liftedRepName package =
  wiredGhcTypes package "LiftedRep" SortSynonym

unliftedRepName :: PackageId -> Name
unliftedRepName package =
  wiredGhcTypes package "UnliftedRep" SortSynonym

-- | The package identity of the GHC.Types scope, if the table has one.
primPackageFromScopes :: ScopeTable -> Maybe PackageId
primPackageFromScopes table =
  listToMaybe
    [ package
    | (_, package, moduleName) <- scopeEntries table,
      moduleName == ghcTypesModule
    ]
