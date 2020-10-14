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
solve rs = evalExpr rs . parseExpr

parseExpr :: String -> Expr
parseExpr = either undefined id . parse parseE ""
  where
    parseE = (char '(' *> parseEs <* char ')') <|> parseR
    parseR = Res . digitToInt <$> (char 'R' *> digit)
    parseEs = do
      e <- parseE
      t <- char '-' <|> char '|'
      es <- parseE `sepBy` char t
      return $ (if t == '-' then Ser else Par) (e : es)

data Expr
  = Ser [Expr] -- series
  | Par [Expr] -- parallel
  | Res Int -- resistor
  deriving (Show)

evalExpr :: [Double] -> Expr -> Double
evalExpr xs (Res i) = xs !! (i - 1)
evalExpr xs (Ser rs) = sum $ evalExpr xs <$> rs
evalExpr xs (Par rs) = recip . sum $ recip . evalExpr xs <$> rs
