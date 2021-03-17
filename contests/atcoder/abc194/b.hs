import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ lines >>> map (words >>> map read) >>> solve >>> show
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [[Int]] -> Int
solve ([n]:abs) =
  minimum [compute as bs, compute bs as, minimum $ zipWith (+) as bs]
  where
    as = map (!! 0) abs
    bs = map (!! 1) abs
    inf = 10 ^ 9
    compute xs ys = minimum $ zipWith max xs (scanl min inf ys)
