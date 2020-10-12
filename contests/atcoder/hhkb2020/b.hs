import Control.Arrow ((>>>))

main :: IO ()
main = interact $ lines >>> solve >>> show >>> (++ "\n")

solve :: [String] -> Int
solve xs = count (tail <$> xs) + count (tail xs)
  where
    count = length . filter (== ('.', '.')) . concat . zipWith zip xs
