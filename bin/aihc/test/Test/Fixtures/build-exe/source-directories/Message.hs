module Message where

import Cycle

message :: String
message = cycleMessage

-- | A polymorphic head in front of the application operator: the checked
-- condition of the if expression must be Bool, not the head's type variable.
keep :: Bool -> a -> a
keep _ x = x

-- | A derived Read instance outside the core libraries. Its generated
-- parser calls base names that this module does not import, so the build
-- must make those names available on its own.
data Setting = Setting {label :: String, level :: Int}
  deriving (Eq, Read)

messageText :: String
messageText
  | not (keep True $ (1208925819614629174706176 :: Integer) == 1208925819614629174706176) = "large integer failed"
  | read "Setting {label = \"on\", level = 3}" /= Setting "on" 3 = "derived Read failed"
  | otherwise = "build-exe works"
