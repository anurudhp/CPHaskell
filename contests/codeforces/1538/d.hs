import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read >>> solve >>> bool "No" "Yes") >>> unlines

type I64 = Integer

solve :: [Int] -> Bool
solve [a, b, k]
  | a == 1 && b == 1 = False
  | k == 1 = a /= b && (a `mod` b == 0 || b `mod` a == 0)
  | otherwise = k <= fac a + fac b

primes :: [Int]
primes = sieve [2 .. 4 * 10 ^ 4]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve (filter ((/= 0) . (`mod` p)) xs)

fac :: Int -> Int
fac n =
  (\(v, acc) -> acc + bool 1 0 (v == 1)) $
  foldl (\(v, acc) p -> f v p acc) (n, 0) primes
  where
    f v p acc
      | v `mod` p /= 0 = (v, acc)
      | otherwise = f (v `div` p) p (acc + 1)
