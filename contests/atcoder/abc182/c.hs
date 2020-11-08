import Control.Arrow ((>>>))
import Control.Monad (filterM)

main :: IO ()
main = interact $ words >>> head >>> solve >>> show

solve :: String -> Int
solve s
  | null subs = -1
  | otherwise = length s - maximum subs
  where
    subs =
      map length .
      filter ((== 0) . (`mod` 3) . read) .
      filter (not . null) . filterM (const [False, True]) $
      s
