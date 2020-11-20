import Control.Arrow ((>>>))
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (sort)
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  C.interact $
  C.lines >>>
  map (C.words >>> map readInt) >>> solve >>> bool "No" "Yes" >>> C.pack
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [[Int]] -> Bool
solve ([_, w]:ps) =
  all (<= w) . scanl (+) 0 . map snd . sort . concat $
  [[(s, p), (t, -p)] | [s, t, p] <- ps]
