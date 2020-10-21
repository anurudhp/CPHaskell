-- order - take some heights of people, and return their ordering
-- indexed from 1
order :: [Int] -> [Int]
order = sort . map swap . zip [1 ..]

--- For variable lines per test case
process :: [String] -> [String]
process [] = []
process (n:xs) = solve ys : process xs'
  where
    n' = read n -- n
    (ys, xs') = splitAt n xs
