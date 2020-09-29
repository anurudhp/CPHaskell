-- AC https://codeforces.com/contest/1327/submission/74097686

import Control.Arrow
import qualified Data.Set as S
type SetInt = S.Set Int

main = interact $ 
  lines >>> drop 1 >>> process >>> unlines

process :: [String] -> [String]
process [] = [""]
process (ns:xs) = solve n (take n xs) : process (drop n xs)
  where n = read ns

solve :: Int -> [String] -> String
solve n ls 
  | S.null unmatched = "OPTIMAL"
  | otherwise = 
    let 
      lv = S.findMin unmatched
      rv = [ u | u <- [1..n], not $ S.member u picked ] !! 0
    in "IMPROVE\n" ++ (show lv) ++ " " ++ (show rv) 
  where
    (picked, unmatched) = 
      foldl pick (S.empty, S.empty) $ 
        zip [1..n] $ map ((map read) . (drop 1) . words) ls
    pick :: (SetInt, SetInt) -> (Int, [Int]) -> (SetInt, SetInt)
    pick (used, unmatched) (p, []) = (used, S.insert p unmatched)
    pick (used, unmatched) (p, (c:rest))
      | S.member c used = pick (used, unmatched) (p, rest)
      | otherwise = (S.insert c used, unmatched)
