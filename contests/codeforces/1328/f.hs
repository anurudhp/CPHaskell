-- AC https://codeforces.com/contest/1328/submission/74774748

import Control.Arrow
import Data.List

main = interact $ 
  lines >>> map (words >>> map read) >>> solve >>> show >>> (++"\n")

solve :: [[Integer]] -> Integer
solve [[n,k], a]
  | foldl1 max (map snd groups) >= k = 0
  | otherwise = foldl1 min $ map (get k) [lt, rt, comb]
  where
    groups = getgroups a
    lt = compute groups
    rt = reverse $ compute $ reverse groups
    comb = map(\(_, c) -> (n, c)) $ merge lt rt

merge a b = map (\((l, c), (l', c')) -> (l + l', c + c')) 
             $ zip a b

getgroups a = map (\u -> (head u, fromIntegral $ length u)) 
               $ group $ sort a

inf = 1000000000000000000 :: Integer
get k xs = foldl min inf
            $ map (\(l, c) -> c - l + k)
              $ filter (\(l, _) -> l >= k) xs

--         [(value,   length)]  -> [(length,  cost)]
compute :: [(Integer, Integer)] -> [(Integer, Integer)]
compute [] = []
compute [(v, l)] = [(l, 0)]
compute (a:b:rest) = (len, cost) : res
  where
    res = compute (b:rest)
    len = snd a + fst (head res)
    cost = snd (head res) + (fst (head res)) * abs (fst a - fst b)
