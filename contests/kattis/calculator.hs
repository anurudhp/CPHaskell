-- PROBLEM: https://open.kattis.com/problems/calculator
import Control.Arrow ((>>>))
import Data.Functor
import Text.Parsec
import Text.Printf (printf)

showFloat :: Double -> String
showFloat = printf "%.2f"

main :: IO ()
main = interact $ lines >>> map (solve >>> showFloat) >>> unlines

solve :: String -> Double
solve = evalExpr . parseExpr

f = do
  x <- getLine
  y <- getLine
parseExpr :: String -> Expr
parseExpr =
  convertEtoExpr . either undefined id . parse parseE "" . filter (/= ' ')
  where
    parseE = EExpr <$> parseF <*> parseR
    parseR =
      (RAdd <$> (char '+' *> parseF) <*> parseR) <|>
      (RSub <$> (char '-' *> parseF) <*> parseR) <|>
      return REps
    parseF = FExpr <$> parseT <*> parseS
    parseS =
      (SMul <$> (char '*' *> parseT) <*> parseS) <|>
      (SDiv <$> (char '/' *> parseT) <*> parseS) <|>
      return SEps
    parseT =
      option id (char '-' $> TNeg) <*>
      ((TWrap <$> (char '(' *> parseE <* char ')')) <|>
       (TNum . read <$> many digit))

{- parser grammar -}
-- E = F R
data E =
  EExpr F R

convertEtoExpr :: E -> Expr
convertEtoExpr (EExpr f r) =
  foldl (flip (.)) id (convertRtoExprs r) (convertFtoExpr f)

-- R = `+` F R | `-` F R | eps
data R
  = RAdd F R
  | RSub F R
  | REps

convertRtoExprs :: R -> [Expr -> Expr]
convertRtoExprs REps = []
convertRtoExprs (RAdd f r) = flip Add (convertFtoExpr f) : convertRtoExprs r
convertRtoExprs (RSub f r) = flip Sub (convertFtoExpr f) : convertRtoExprs r

-- F = T S
data F =
  FExpr T S

convertFtoExpr :: F -> Expr
convertFtoExpr (FExpr t s) =
  foldl (flip (.)) id (convertStoExprs s) (convertTtoExpr t)

-- S = `*` T S | `/` T S | eps
data S
  = SMul T S
  | SDiv T S
  | SEps

convertStoExprs :: S -> [Expr -> Expr]
convertStoExprs SEps = []
convertStoExprs (SMul t s) = flip Mul (convertTtoExpr t) : convertStoExprs s
convertStoExprs (SDiv t s) = flip Div (convertTtoExpr t) : convertStoExprs s

-- T = `-`? { `(` E `)` | digit* }
data T
  = TNeg T
  | TWrap E
  | TNum Int

convertTtoExpr :: T -> Expr
convertTtoExpr (TWrap e) = convertEtoExpr e
convertTtoExpr (TNum x) = Const $ fromIntegral x
convertTtoExpr (TNeg t) = Neg $ convertTtoExpr t

-- syntax tree
data Expr
  = Const Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Show)

evalExpr :: Expr -> Double
evalExpr (Const x) = x
evalExpr (Add u v) = evalExpr u + evalExpr v
evalExpr (Sub u v) = evalExpr u - evalExpr v
evalExpr (Mul u v) = evalExpr u * evalExpr v
evalExpr (Div u v) = evalExpr u / evalExpr v
evalExpr (Neg u) = -evalExpr u
