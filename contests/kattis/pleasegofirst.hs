-- https://open.kattis.com/problems/pleasegofirst
import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (groupBy, sort)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs =
  let (hs, ts) = splitAt k xs
   in hs : chunksOf k ts

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

main :: IO ()
main =
  interact $
  lines >>> drop 1 >>> chunksOf 2 >>> map solve >>> map show >>> unlines

solve :: [String] -> Int
solve [_, ps] = 5 * (sum (map icost fs) - fcost (map length fs))
  where
    fs = map (map snd) $ groupOn fst $ sort $ zip ps [1 ..]
    icost g = length g * last g
    fcost =
      snd .
      foldl
        (\(len, acc) l ->
           let len' = len + l
            in (len', acc + l * len'))
        (0, 0)
