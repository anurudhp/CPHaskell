import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read) >>> process >>> map show >>> unlines

process :: [[Int]] -> [Int]
process [] = []
process (_:xs:xss) = solve xs : process xss

solve :: [Int] -> Int
solve xs = depth gs gs
  where
    gs = map length . group . drop 1 $ xs
    group [y] = [[y]]
    group (y:ys) =
      let gs = group ys
       in if (head . head) gs < y
            then [y] : gs
            else (y : head gs) : tail gs
    depth _ _ = error ""
