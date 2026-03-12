module ParserFuzz.Arbitrary.Helpers
  ( eThingWith,
    moduleNameNode,
    noNamespace,
    noWildcard,
  )
where

import Language.Haskell.Exts qualified as HSE

eThingWith :: HSE.EWildcard () -> HSE.QName () -> [HSE.CName ()] -> HSE.ExportSpec ()
eThingWith = HSE.EThingWith ()

moduleNameNode :: String -> HSE.ModuleName ()
moduleNameNode = HSE.ModuleName ()

noNamespace :: HSE.Namespace ()
noNamespace = HSE.NoNamespace ()

noWildcard :: HSE.EWildcard ()
noWildcard = HSE.NoWildcard ()
