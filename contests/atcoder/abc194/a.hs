import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  interact $ lines >>> map (words >>> map read >>> solve >>> show) >>> unlines
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [Int] -> Int
solve [a, b]
  | a + b >= 15 && b >= 8 = 1
  | a + b >= 10 && b >= 3 = 2
  | a + b >= 3 = 3
  | otherwise = 4
