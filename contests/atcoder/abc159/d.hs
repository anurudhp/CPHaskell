-- AC https://atcoder.jp/contests/abc159/submissions/17193156

import Control.Arrow ((>>>))
import qualified Data.Map as M

main :: IO ()
main =
  interact $
    lines
      >>> drop 1 -- n
      >>> head
      >>> words
      >>> map read
      >>> solve
      >>> map show
      >>> unlines

solve :: [Int] -> [Int]
solve a = map ((total -) . maybe 0 (subtract 1) . flip M.lookup ga) a
  where
    ga :: M.Map Int Int
    ga = M.fromListWith (+) [(x, 1) | x <- a]

    total = sum . map (choose2 . snd) . M.toList $ ga

choose2 :: Int -> Int
choose2 n = (n * (n - 1)) `div` 2
