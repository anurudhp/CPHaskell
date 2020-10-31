import Control.Arrow ((>>>))
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  C.interact $
  C.lines >>>
  drop 1 >>>
  map (C.words >>> map readInt) >>>
  process >>> map (show >>> C.pack) >>> C.unlines
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

process :: [[Int]] -> [Int]
process [] = []
process (_:xs:xss) = solve xs : process xss

solve :: [Int] -> Int
solve = maximum . foldl bfs [0] . blocks (<) . tail
  where
    bfs (d:ds) l = ds ++ replicate l (d + 1)

blocks :: (a -> a -> Bool) -> [a] -> [Int]
blocks f xs = foldr (bool appl incl) [1] $ zipWith f xs (tail xs)
  where
    appl = (1 :)
    incl (l:ls) = 1 + l : ls
