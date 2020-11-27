import Control.Arrow ((>>>))

main =
  interact $
  words >>> drop 1 >>> map (read >>> solve >>> map show >>> unwords) >>> unlines

solve n = n : [1 .. n - 1]
