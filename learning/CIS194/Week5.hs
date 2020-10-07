module Week5 where

import Data.Char (isAscii, isControl, isDigit)
import Data.List (nub)

-- Exercise 1
halveEvens :: [Integer] -> [Integer]
halveEvens xs = (`div` 2) <$> filter even xs

safeString :: String -> String
safeString = map makeSafe
  where
    makeSafe c
      | Data.Char.isControl c = '_'
      | Data.Char.isAscii c = c
      | otherwise = '_'

holes :: [a] -> [[a]]
holes [] = []
holes (x : xs) = xs : ((x :) <$> holes xs)

longestText :: Show a => [a] -> a
longestText = foldl1 (\u v -> if length (show u) > length (show v) then u else v)

adjacents :: [a] -> [(a, a)]
adjacents [] = []
adjacents [_] = []
adjacents (x : y : xs) = (x, y) : adjacents (y : xs)

commas :: [String] -> String
commas [] = []
commas ss = foldr1 (\a s -> a ++ ", " ++ s) ss

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1 (zipWith (+))

sumNumbers :: String -> Integer
sumNumbers s = sum $ map read $ words $ map (\c -> if Data.Char.isDigit c then c else ' ') s

-- Exercise 2: Word Count
wordCount :: String -> String
wordCount s =
  unlines $
    zipWith
      (++)
      [ "Number of lines: ",
        "Number of empty lines: ",
        "Number of words: ",
        "Number of unique words: ",
        "Number of words followed by themselves: ",
        "Length of the longest line: "
      ]
      $ show
        <$> ( [ length . lines,
                length . filter null . lines,
                length . words,
                length . nub . words,
                length . filter (uncurry (==)) . adjacents . words,
                foldl max 0 . map length . lines
              ]
                <*> pure s
            )

-- Testing Harness --
main :: [String] -> IO ()
main _ = interact $ const (formatTests testResults)

testResults :: [(String, [Bool])]
testResults =
  [ ("halveEvens", ex_halveEvens),
    ("safeString", ex_safeString),
    ("holes", ex_holes),
    ("longestText", ex_longestText),
    ("adjacents", ex_adjacents),
    ("commas", ex_commas),
    ("addPolynomials", ex_addPolynomials),
    ("sumNumbers", ex_sumNumbers),
    ("wordCount", ex_wordCount)
  ]

formatTests :: [(String, [Bool])] -> String
formatTests ts = unlines $ formatTest <$> ts

formatTest :: (String, [Bool]) -> String
formatTest (s, rs) = concat [s, ": ", msg]
  where
    numTotal = length rs
    numPassed = length $ filter id rs
    failed :: [(Integer, Bool)]
    failed = filter (not . snd) $ zip [1 ..] rs
    msg :: String
    msg
      | numPassed == numTotal =
        concat
          [ show numPassed,
            "/",
            show numTotal,
            " OK"
          ]
      | otherwise =
        concat $
          [ show numPassed,
            "/",
            show numTotal,
            ". Failing: "
          ]
            ++ map (show . fst) failed

ex_halveEvens :: [Bool]
ex_halveEvens =
  [ halveEvens [] == [],
    halveEvens [1, 2, 3, 4, 5] == [1, 2],
    halveEvens [6, 6, 6, 3, 3, 3, 2, 2, 2] == [3, 3, 3, 1, 1, 1]
  ]

ex_safeString :: [Bool]
ex_safeString =
  [ safeString [] == [],
    safeString "Hello World!" == "Hello World!",
    safeString "That’s your line:\n" == "That_s your line:_",
    safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
  ]

ex_holes :: [Bool]
ex_holes =
  [ holes "" == [],
    holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
  ]

ex_longestText :: [Bool]
ex_longestText =
  [ longestText [True, False] == False,
    longestText [2, 4, 16, 32] == (32 :: Int),
    longestText (words "Hello World") == "World",
    longestText (words "Olá mundo") == "Olá"
  ]

ex_adjacents :: [Bool]
ex_adjacents =
  [ adjacents "" == [],
    adjacents [True] == [],
    adjacents "Hello" == [('H', 'e'), ('e', 'l'), ('l', 'l'), ('l', 'o')]
  ]

ex_commas :: [Bool]
ex_commas =
  [ commas [] == "",
    commas ["Hello"] == "Hello",
    commas ["Hello", "World"] == "Hello, World",
    commas ["Hello", "", "World"] == "Hello, , World",
    commas ["Hello", "new", "World"] == "Hello, new, World"
  ]

ex_addPolynomials :: [Bool]
ex_addPolynomials =
  [ addPolynomials [[]] == [],
    addPolynomials [[0, 1], [1, 1]] == [1, 2],
    addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
  ]

ex_sumNumbers :: [Bool]
ex_sumNumbers =
  [ sumNumbers "" == 0,
    sumNumbers "Hello world!" == 0,
    sumNumbers "a1bc222d3f44" == 270,
    sumNumbers "words0are1234separated12by3integers45678" == 46927,
    sumNumbers "000a." == 0,
    sumNumbers "0.00a." == 0
  ]

ex_wordCount :: [Bool]
ex_wordCount =
  [ wordCount "Hello world!\nI have have repeated myself.\n\nand a blank line\n"
      == unlines
        [ "Number of lines: 4",
          "Number of empty lines: 1",
          "Number of words: 11",
          "Number of unique words: 10",
          "Number of words followed by themselves: 1",
          "Length of the longest line: 28"
        ]
  ]
