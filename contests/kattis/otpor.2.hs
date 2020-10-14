-- PROBLEM: https://open.kattis.com/problems/otpor
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Char (digitToInt)
import safe Numeric (showFFloat)
import safe Text.Parsec (char, digit, parse, sepBy, (<|>))

main :: IO ()
main =
  interact $ lines >>> drop 1 >>> process >>> showFFloat (Just 10) >>> ($ "\n")

process :: [String] -> Double
process [rs, e] = solve (map read . words $ rs) e
process _ = 0

solve :: [Double] -> String -> Double
solve rs = either undefined id . parse parseE ""
  where
    parseE = (char '(' *> parseEs <* char ')') <|> parseR
    parseR = (rs !!) . subtract 1 . digitToInt <$> (char 'R' *> digit)
    parseEs = do
      e <- parseE
      t <- char '-' <|> char '|'
      es <- parseE `sepBy` char t
      return $ (if t == '-' then ser else par) (e : es)

    ser = sum
    par = recip . sum . map recip
