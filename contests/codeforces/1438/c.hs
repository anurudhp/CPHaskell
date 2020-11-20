import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>>
  map (words >>> map read) >>>
  process >>> map (map show >>> unwords) >>> unlines

process :: [[Int]] -> [[Int]]
process [] = []
process ([n, _]:xss) =
  let (xs, xss') = splitAt n xss
   in solve xs ++ process xss'

solve :: [[Int]] -> [[Int]]
solve = zipWith (\p -> zipWith f (drop p zo)) zo
  where
    zo = 0 : 1 : zo
    f p x
      | x `mod` 2 == p = x
      | otherwise = x + 1
