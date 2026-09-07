{- HLINT ignore "Use camelCase" -}

-- | Precedence parsers. 'Prelude' builds its 'Read' class on this module.
--
-- The parser itself lives in "GHC.Prim.Read", so that a derived 'Read'
-- instance needs the primitive package only.
module Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    Prec,
    minPrec,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import GHC.Prim.Read
  ( Prec,
    ReadPrec,
    choice,
    get,
    look,
    minPrec,
    pfail,
    prec,
    readPrec_to_S,
    readS_to_Prec,
    reset,
    step,
    (+++),
    (<++),
  )
