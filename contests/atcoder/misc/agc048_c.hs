-- PROBLEM: https://atcoder.jp/contests/agc048/tasks/agc048_c
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ lines >>> map (words >>> map read) >>> solve >>> show >>> (++ "\n")

solve :: [[Int]] -> Int
solve [[_, l], as, bs] = either id id $ compute (gaps as) (gaps bs)
  where
    diffs xs = subtract 1 <$> zipWith (-) (tail xs) xs
    gaps xs = filter ((/= 0) . fst) $ zip (diffs (0 : xs ++ [l + 1])) [0 ..]

    no = Left (-1)
    ok = Right

    compute _ [] = ok 0
    compute xs ((0, _) : ys) = compute xs ys
    compute xs ((y, i) : ys) = (+) <$> cost <*> compute (drop n' xs) ys
      where
        n' = extract y xs
        cost
          | sum (fst <$> xs') /= y = no
          | otherwise = ok (lt + rt)
          where
            xs' = take n' xs
            lt = max 0 (i - snd (head xs'))
            rt = max 0 (snd (last xs') - i)

        extract 0 _ = 0
        extract _ [] = 0
        extract y' ((z, _) : zs)
          | y' >= z = 1 + extract (y' - z) zs
          | otherwise = 0
