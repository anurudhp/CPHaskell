{-# LANGUAGE Safe #-}

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    words
      >>> drop 1
      >>> map read
      >>> solve
      >>> map show
      >>> unwords

solve :: [Int] -> [Int]
solve = foldr f [2, 2]
  where
    f _ [-1] = [-1]
    f x [l, r]
      | l' > r' = [-1]
      | otherwise = [l', r']
      where
        l' = if l `mod` x == 0 then l else l + x - (l `mod` x)
        r' = r - (r `mod` x) + (x - 1)
