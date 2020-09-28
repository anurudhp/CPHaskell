-- AC https://codeforces.com/contest/1426/submission/94150745

import Control.Arrow ((>>>))
import qualified Data.Set as Set

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> head
      >>> words
      >>> map read
      >>> solve
      >>> show
      >>> (++ "\n")

type SetInt = Set.Set Integer

solve :: [Integer] -> Integer
solve a = compute (Set.singleton 0) 0 a

compute :: SetInt -> Integer -> [Integer] -> Integer
compute _ _ [] = 0
compute pres pre (x : a) =
  if Set.member pre' pres
    then 1 + compute (Set.fromList [x, 0]) x a
    else compute (Set.insert pre' pres) pre' a
  where
    pre' = pre + x
