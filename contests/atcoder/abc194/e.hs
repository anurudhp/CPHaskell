import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (groupBy, sort)
import Data.Maybe (fromMaybe)

type PII = (Int, Int)

main :: IO ()
main =
  C.interact $
  C.words >>> map readInt >>> solve >>> show >>> (++ "\n") >>> C.pack
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [Int] -> Int
solve (n:m:xs) =
  rec 0 $ groupBy (\a b -> fst a == fst b) $ sort $ zip xs [1 .. n]
  where
    rec v [] = v
    rec v (ps:pss)
      | v /= fst (head ps) = v
      | check (0 : map snd ps) = v
      | otherwise = rec (v + 1) pss
    check (p:p':ps) = p' - p > m || check (p' : ps)
    check [p] = n + 1 - p > m
    check [] = False
