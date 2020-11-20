import Control.Arrow ((>>>))
import Data.Bits

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> showAns
  where
    showAns Nothing = "NO"
    showAns (Just xs) =
      unlines ("YES" : show (length xs) : map (unwords . map show) xs)

solve :: [Int] -> Maybe [[Int]]
solve xs
  | odd n = Just (extra ++ [[1, 2, 3]] ++ extra)
  | foldl1 xor xs /= 0 = Nothing
  | otherwise = solve (tail xs)
  where
    n = length xs
    extra = (3 :) <$> chunksOf 2 [4 .. n]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (ls, xs') = splitAt n xs
   in ls : chunksOf n xs'
