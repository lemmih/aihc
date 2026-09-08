module Selected (identity, value) where

import Aihc.CompilerBuildIdentity (compilerBuildIdentity)

identity :: String
identity = compilerBuildIdentity

value :: Int
value = 1
