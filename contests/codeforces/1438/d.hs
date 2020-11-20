import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> showAns
  where
    showAns Nothing = "NO"
    showAns (Just xs) =
      unlines ("YES" : show (length xs) : map (unwords . map show) xs)

solve :: [Int] -> Maybe [[Int]]
solve xs
  | all (== head xs) xs = Just []
  | odd (length xs) = Just []
  | otherwise = Nothing
