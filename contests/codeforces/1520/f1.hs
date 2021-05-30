import Control.Arrow ((>>>))
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  interact $
    lines >>>
    (\(nt:ls) -> head (words nt) : ls) >>>
    map read >>> solve >>> map show >>> unlines

data Query
  = Ask Int Int
  | Answer Int

instance Show Query where
  show (Ask l r) = "? " ++ show l ++ " " ++ show r
  show (Answer x) = "! " ++ show x

solve :: [Int] -> [Query]
solve (n:k:rs) = go 1 n rs
  where
    go lo hi xs
      | lo == hi = [Answer lo]
      | otherwise =
        let mid = (lo + hi) `div` 2
         in Ask 1 mid :
            (let (lo', hi') =
                   (if head xs + k > mid
                      then (mid + 1, hi)
                      else (lo, mid))
              in go lo' hi' (tail xs))
