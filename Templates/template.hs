import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ _
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst
