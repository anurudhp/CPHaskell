import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (group, sort)
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  C.interact $
  C.lines >>> (!! 1) >>> C.words >>> map readInt >>> solve >>> show >>> C.pack
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [Int] -> Int
solve xs = sum [fa * fb * (a - b) ^ 2 | (a, fa) <- fxs, (b, fb) <- fxs, a < b]
  where
    fxs = map (\vs -> (head vs, length vs)) . group . sort $ xs
