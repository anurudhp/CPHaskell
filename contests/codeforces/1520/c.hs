import Control.Arrow

main = interact $ 
         words >>> drop 1 
         >>> map (
               read
               >>> solve
               >>> maybe "-1\n" (unlines . map (unwords . map show)))
         >>> concat

solve :: Int -> Maybe [[Int]]
solve 1 = Just [[1]]
solve 2 = Nothing
solve n = Just [[val $ i*n + j | j <- [0..n-1]] | i <- [0..n-1]]
    where
        val x = 1 + (if even x then (n^2 `div` 2) else 0) + x `div` 2
