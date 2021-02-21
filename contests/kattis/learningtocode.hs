-- https://open.kattis.com/problems/learningtocode
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Functor (($>))
import safe qualified Data.Map as M
import safe Data.Maybe (catMaybes)
import safe Text.Parsec
  ( (<|>)
  , anyChar
  , char
  , choice
  , many
  , many1
  , manyTill
  , parse
  , spaces
  , string
  , try
  )

main :: IO ()
main = interact $ lines >>> solve >>> unlines

solve :: [String] -> [String]
solve = reverse . catMaybes . snd . foldl eval (emptyContext, []) . map parse1
  where
    eval (gamma, ys) st =
      let (gamma', s) = evalStatement gamma st
       in (gamma', s : ys)

parse1 :: String -> Statement
parse1 = either undefined id . parse parseStatement ""
  where
    parseStatement = parseDecl <|> parsePrint <|> parseEnd
    parseDecl =
      Decl <$> (string "var " *> many1 var <* string " = ") <*>
      (parseExpr <* char ';')
    parsePrint = Print <$> (string "print " *> parseExpr <* char ';')
    parseEnd = spaces *> string "end." $> End
    parseExpr = parseLit <|> parseTLit <|> (Var <$> many1 var)
    parseLit = Lit <$> (char '"' *> manyTill anyChar (char '"'))
    parseTLit =
      TLit <$>
      (char '`' *>
       many
         ((Left <$> many1 lit) <|>
          (Right <$> (try (string "${") *> parseExpr <* char '}')) <|>
          (Left <$> string "$")) <*
       char '`')
    var = someCharOf ('_' : ['a' .. 'z'])
    lit =
      someCharOf $
      "!@#%^&*()-_=+. " ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    someCharOf = choice . fmap char

type Context = M.Map String String -- variable: value

emptyContext :: Context
emptyContext = M.empty

data Expr
  = Var String -- variable
  | Lit String -- string literal
  | TLit [Either String Expr] -- template literal
  deriving (Show)

data Statement
  = Decl String Expr -- `var x = e`
  | Print Expr -- `print e`
  | End

evalExpr :: Context -> Expr -> String
evalExpr gamma (Var s) =
  let (Just v) = M.lookup s gamma
   in v
evalExpr _ (Lit s) = s
evalExpr gamma (TLit t) = concatMap (either id (evalExpr gamma)) t

evalStatement :: Context -> Statement -> (Context, Maybe String)
evalStatement gamma (Decl x e) = (M.insert x (evalExpr gamma e) gamma, Nothing)
evalStatement gamma (Print e) = (gamma, Just $ evalExpr gamma e)
evalStatement gamma End = (gamma, Nothing)
