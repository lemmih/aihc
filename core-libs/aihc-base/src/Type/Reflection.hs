{-# LANGUAGE PatternSynonyms #-}

module Type.Reflection
  ( Typeable (..),
    TypeRep,
    (:~~:) (..),
    pattern Con,
    pattern Con',
    pattern App,
    pattern Fun,
    typeRepKind,
    someTypeRep,
    SomeTypeRep (..),
    TyCon (..),
    Module (..),
    eqTypeRep,
    typeOf,
    splitApps,
    typeRepTyCon,
    tyConPackage,
    tyConModule,
    tyConName,
    modulePackage,
    moduleName,
    rnfTypeRep,
    rnfSomeTypeRep,
    rnfTyCon,
    rnfModule,
  )
where

import Type.Reflection.Internal
