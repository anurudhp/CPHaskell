-- AC https://atcoder.jp/contests/abc159/submissions/11129383

import Control.Arrow
import qualified Data.List as L
import qualified Data.Map as M

main =
  interact $
    lines >>> tail >>> head >>> words >>> map read
      >>> solve
      >>> map show
      >>> unlines

solve :: [Int] -> [Int]
solve a =
  zipWith
    (-)
    (take len $ repeat total)
    $ map
      ( \v -> case v of
          Nothing -> 0
          (Just x) -> x - 1
      )
      [M.lookup x ga | x <- a]
  where
    len = length a
    ga :: M.Map Int Int
    ga = M.fromListWith (+) [(x, 1) | x <- a]
    total = sum $ map choose2 $ map (\v -> snd v) $ M.toList ga

choose2 :: Int -> Int
choose2 n = (n * (n - 1)) `div` 2
