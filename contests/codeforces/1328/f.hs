{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (group, sort)

main :: IO ()
main =
  interact $
    lines >>> map (words >>> map read) >>> solve >>> show >>> (++ "\n")

solve :: [[Integer]] -> Integer
solve [[n, k], a]
  | maximum (map snd groups) >= k = 0
  | otherwise = minimum $ map (get k) [lt, rt, comb]
  where
    groups = getGroups a
    lt = compute groups
    rt = reverse . compute . reverse $ groups
    comb = zip (repeat n) $ zipWith (+) (map snd lt) (map snd rt)

type PII = (Integer, Integer)

-- merge :: [PII] -> [PII] -> [PII]
-- merge = zipWith cadd
--   where
--     cadd (l, c) (l', c') = (l + l', c + c')

getGroups :: [Integer] -> [PII]
getGroups =
  map (\u -> (head u, fromIntegral $ length u))
    . group
    . sort

inf :: Integer
inf = 10 ^ 18

get :: Integer -> [(Integer, Integer)] -> Integer
get k =
  foldl min inf
    . map ((+ k) . uncurry (flip (-)))
    . filter ((>= k) . fst)

--         [(value,   length)]  -> [(length,  cost)]
compute :: [(Integer, Integer)] -> [(Integer, Integer)]
compute [] = []
compute [(_, l)] = [(l, 0)]
compute (x : x' : xs) = (len, cost) : res
  where
    res = compute (x' : xs)
    len = snd x + fst (head res)
    cost = snd (head res) + fst (head res) * abs (fst x - fst x')
