module S5ImportQualifiedHidingVar where
import qualified Data.Maybe hiding (fromMaybe)
x = Data.Maybe.isJust Nothing
