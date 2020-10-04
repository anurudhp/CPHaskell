-- AC https://atcoder.jp/contests/abc159/submissions/17193156

import Control.Arrow ((>>>))
import qualified Data.List as L
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
solve a =
  zipWith (-) (repeat total) $
    map
      ( \v -> case v of
          Nothing -> 0
          (Just x) -> x - 1
      )
      [M.lookup x ga | x <- a]
  where
    ga :: M.Map Int Int
    ga = M.fromListWith (+) [(x, 1) | x <- a]

    total = sum $ map (choose2 . snd) $ M.toList ga

choose2 :: Int -> Int
choose2 n = (n * (n - 1)) `div` 2
