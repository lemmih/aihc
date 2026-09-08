-- | Continuously runnable Hedgehog properties owned by @aihc-tc@.
module Aihc.Tc.Fuzz
  ( tcFuzzProperties,
  )
where

import Hedgehog (Property)
import Test.Tc.Properties (prop_zonkAssignedMeta, prop_zonkIdempotent)

tcFuzzProperties :: [(String, Property)]
tcFuzzProperties =
  [ ("zonking idempotent", prop_zonkIdempotent),
    ("zonk reads an assigned metavariable", prop_zonkAssignedMeta)
  ]
