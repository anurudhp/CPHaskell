import Control.Arrow ((>>>))
import qualified Data.Set as S

type SetInt = S.Set Int

main :: IO ()
main =
  interact $
    lines
      >>> (head . tail)
      >>> words
      >>> map read
      >>> solve
      >>> map show
      >>> unlines

solve :: [Int] -> [Int]
solve xs = tail . reverse . snd $ foldl f (S.empty, [0]) xs
  where
    f :: (SetInt, [Int]) -> Int -> (SetInt, [Int])
    f (s, ys) x = (s', nxt s' (head ys) : ys)
      where
        s' = S.insert x s

    nxt :: SetInt -> Int -> Int
    nxt s x
      | S.member x s = nxt s (x + 1)
      | otherwise = x
