-- AC https://codeforces.com/contest/1328/submission/74464903

import Control.Arrow

main = interact $ 
  lines >>> drop 1 >>> map (words >>> map read) 
  >>> process 
  >>> map (map show >>> unwords) >>> unlines

process :: [[Integer]] -> [[Integer]]
process [] = []
process ([n]:a:rest) = solve n a ++ process rest 

solve :: Integer -> [Integer] -> [[Integer]]
solve n a = [[foldl1 max fin], fin]
  where
    nn = fromIntegral n
    fin
      | a == (take nn $ repeat (head a)) = take nn $ repeat 1
      | head cols == head (reverse cols)
          && head a /= head (reverse a) = 
            case newcols of Nothing -> 3 : tail cols
                            (Just a) -> a
      | otherwise = cols
          where newcols = update cols

    cols = colour 1 a
    
    update :: [Integer] -> Maybe [Integer]
    update [] = Nothing
    update [_] = Nothing
    update (x:y:rest)
      | x == y = Just (x : map (pick True) (y : rest))
      | otherwise = case rec of (Just a) -> Just (x : a)
                                Nothing -> Nothing 
        where rec = update (y : rest) 
    
    colour :: Integer -> [Integer] -> [Integer]
    colour _ [] = []
    colour cur [x] = [cur]
    colour cur (x:y:rest) = cur : colour (pick (x /= y) cur) (y : rest)

    pick :: Bool -> Integer -> Integer
    pick fl x
      | fl = 3 - x
      | otherwise = x
