module S5ImportHidingVar where
import Data.Maybe hiding (maybe)
x = fromMaybe 0 Nothing
