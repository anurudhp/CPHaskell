import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> map (words >>> map read >>> solve >>> show)
      >>> unlines

solve :: [Integer] -> Integer
solve [n, a, b] = 2 *% ways1 -% ways2
  where
    ways2 = row *% row
    ways1 = row *% (n - a + 1) *% (n - b + 1)
    row = len * (len + 1)
    len = max 0 (n - a - b + 1)

modV :: Integer
modV = 10 ^ 9 + 7

(*%) :: Integer -> Integer -> Integer
u *% v = (u * v) `mod` modV

infixl 7 *%

(-%) :: Integer -> Integer -> Integer
u -% v = ((u - v) `mod` modV + modV) `mod` modV

infixl 6 -%
