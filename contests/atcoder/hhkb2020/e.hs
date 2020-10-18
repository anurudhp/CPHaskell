-- VERDICT: TLE
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe qualified Data.ByteString.Lazy.Char8 as C
import safe Data.List (transpose)

main :: IO ()
main =
  C.interact $ C.lines >>> drop 1 >>> map C.unpack >>> solve >>> show >>> (++ "\n") >>> C.pack

solve :: [String] -> Int
solve xs =
  foldl1 (+%) . map (foldl1 (+%)) $
    zipWith (zipWith (\u v -> ways (u + v - 1))) (compute <$> xs) (transpose (compute <$> transpose xs))
  where
    k = length . filter (== '.') . concat $ xs
    ways (-1) = 0
    ways n = (2 ^% n -% 1) *% 2 ^% (k - n)

compute :: String -> [Int]
compute = snd . foldr f (0, [])
  where
    f '.' (len, ys) = (len + 1, len + 1 : ys)
    f '#' (_, ys) = (0, 0 : ys)

-- modular arithmetic
modV :: Int
modV = 10 ^ (9 :: Int) + 7

(*%) :: Int -> Int -> Int
u *% v = (u * v) `mod` modV

infixl 7 *%

(+%) :: Int -> Int -> Int
u +% v = (u + v) `mod` modV

infixl 6 +%

(-%) :: Int -> Int -> Int
u -% v = ((u - v) `mod` modV + modV) `mod` modV

infixl 6 -%

(^%) :: Int -> Int -> Int
u ^% v
  | v < 0 = error "invalid power!"
  | v == 0 = 1
  | odd v = u *% u ^% (v - 1)
  | even v = let u' = u ^% (v `div` 2) in u' *% u'
  | otherwise = error ""

infixl 8 ^%
