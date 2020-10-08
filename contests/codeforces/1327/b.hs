-- AC https://codeforces.com/contest/1327/submission/94883247

import Control.Arrow ((>>>))
import qualified Data.Set as S

type SetInt = S.Set Int

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> process >>> unlines

process :: [String] -> [String]
process [] = []
process (ns : xs) = let n = read ns in solve n (take n xs) : process (drop n xs)

solve :: Int -> [String] -> String
solve n ls
  | S.null unmatched = "OPTIMAL"
  | otherwise =
    let lv = S.findMin unmatched
        rv = head $ filter (\u -> not $ S.member u picked) [1 .. n]
     in "IMPROVE\n" ++ show lv ++ " " ++ show rv
  where
    (picked, unmatched) =
      foldl pick (S.empty, S.empty) $
        zip [1 .. n] $ map (map read . drop 1 . words) ls
    pick :: (SetInt, SetInt) -> (Int, [Int]) -> (SetInt, SetInt)
    pick (used, unmatched) (p, cs) =
      case filter (\u -> not $ S.member u used) cs of
        [] -> (used, S.insert p unmatched)
        (c : _) -> (S.insert c used, unmatched)
