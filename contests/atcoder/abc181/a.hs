import Control.Arrow ((>>>))

main :: IO ()
main = interact $ read >>> solve

solve :: Int -> String
solve n
  | even n = "White"
  | otherwise = "Black"
