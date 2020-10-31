module Week8 where

import Control.Monad (unless, void)
import Data.Bifunctor (Bifunctor(first))
import Data.Char (isAlphaNum)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- Exercise 1: A `Functor` instance
-- Functor laws
-- fmap id  ==  id
-- fmap (f . g) == fmap f . fmap g
data ComplicatedA a b
  = Con1 a b
  | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
  = Con3 (f a)
  | Con4 (g b)
  | Con5 (g (g [b]))

instance Functor (ComplicatedA a) where
  fmap f (Con1 x y) = Con1 x (f y)
  fmap f (Con2 xs) = Con2 (map (fmap (f .)) xs)

instance Functor g => Functor (ComplicatedB f g a) where
  fmap _ (Con3 x) = Con3 x
  fmap f (Con4 y) = Con4 (fmap f y)
  fmap f (Con5 zs) = Con5 (fmap (fmap (fmap f)) zs)

-- Exercise 2: Rewriting monadic code
func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
  f . f <$> xs

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = f . f <$> xs

func1 :: Monad f => f a -> f (a, a)
func1 xs = xs >>= (\x -> return (x, x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = (\x -> (x, x)) <$> xs

func2 :: Monad f => f a -> f (a, a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x, y))

func3 :: Monad f => f a -> f (a, a)
func3 xs = xs >>= (\x -> xs >> return (x, x))

func4 :: Monad f => f a -> f a -> f (a, a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x, y))

func4' :: Applicative f => f a -> f a -> f (a, a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
  x <- xs
  let x' = x + 1
  y <- (+ 1) <$> ys
  return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (+) <$> ((+ 1) <$> xs) <*> ((+ 1) <$> ys)

func6 :: Monad f => f Integer -> f (Integer, Integer)
func6 xs = do
  x <- xs
  return $
    if x > 0
      then (x, 0)
      else (0, x)

func6' :: Functor f => f Integer -> f (Integer, Integer)
func6' =
  fmap
    (\x ->
       if x > 0
         then (x, 0)
         else (0, x))

func7 :: Monad f => f Integer -> f (Integer, Integer)
func7 xs = do
  x <- xs
  if x > 0
    then return (x, 0)
    else return (0, x)

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = ((+) <$> xs) <*> pure x

func8' :: Applicative f => f Integer -> Integer -> f Integer
func8' xs x = ((+) <$> xs) <*> pure x

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs =
  xs >>= \x ->
    if even x
      then ys
      else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
  x <- xs >>= (\x -> return (x * x))
  return (x + 10)

func10' :: Applicative f => f Integer -> f Integer
func10' xs = (+ 10) . (\x -> x * x) <$> xs

-- Exercise 3: A parser monad
data Parser a =
  P (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = do
  (a, s') <- runParser p s
  if not (null s')
    then Nothing
    else return a

noParser :: Parser a
noParser = P (const Nothing)

pureParser :: a -> Parser a
pureParser a = P (\s -> Just (a, s))

instance Functor Parser where
  fmap f p = P (fmap (first f) . runParser p)

instance Applicative Parser where
  pure = pureParser
  fp <*> fx =
    P
      (\s ->
         case runParser fp s of
           Nothing -> Nothing
           Just (f, s') ->
             case runParser fx s' of
               Nothing -> Nothing
               Just (a, s'') -> Just (f a, s''))

instance Monad Parser where
  return = pureParser
  fa >>= k =
    P
      (\s ->
         case runParser fa s of
           Nothing -> Nothing
           Just (a, s') -> runParser (k a) s')

anyChar :: Parser Char
anyChar =
  P (\s ->
       if null s
         then Nothing
         else Just (head s, tail s))

char :: Char -> Parser ()
char c = do
  c' <- anyChar
  unless (c' == c) noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
  c' <- anyChar
  if c /= c'
    then return c'
    else noParser

orElse :: Parser a -> Parser a -> Parser a
orElse p q =
  P (\s ->
       case runParser p s of
         Nothing -> runParser q s
         Just (a, s') -> Just (a, s'))

many :: Parser a -> Parser [a]
many p =
  (do a <- p
      as <- many p
      return (a : as)) `orElse`
  pure []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy pa ps =
  (do a <- pa
      (do ps
          (a :) <$> sepBy pa ps) `orElse`
        return [a]) `orElse`
  return []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
      char '"'
      content <- many (anyCharBut '"')
      char '"'
      return content

-- Exercise 4 : Parsing an INI file
type Identifer = String

type Declaration = (Identifer, String)

type Section = (Identifer, [Declaration])

type INIFile = [Section]

letterOrDigit :: Parser Char
letterOrDigit = do
  c <- anyChar
  if isAlphaNum c
    then return c
    else noParser

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p `orElse` pure []
  return (a : as)

skipCommentsAndEmptyLines :: Parser ()
skipCommentsAndEmptyLines = void $ many (skipEmptyLine `orElse` skipComment)

skipComment :: Parser ()
skipComment = char '#' >> many (anyCharBut '\n') >> char '\n'

skipEmptyLine :: Parser ()
skipEmptyLine = char '\n'

parseIdentifier :: Parser Identifer
parseIdentifier = many letterOrDigit

parseSectionHeader :: Parser Identifer
parseSectionHeader = do
  _ <- char '['
  s <- parseIdentifier
  _ <- char ']'
  return s

parseDeclaration :: Parser Declaration
parseDeclaration = do
  skipCommentsAndEmptyLines
  key <- parseIdentifier
  _ <- many (char ' ')
  _ <- char '='
  _ <- many (char ' ')
  value <- many1 (anyCharBut '\n')
  _ <- char '\n'
  return (key, value)

parseSection :: Parser Section
parseSection = do
  skipCommentsAndEmptyLines
  header <- parseSectionHeader
  decls <- many1 parseDeclaration
  return (header, decls)

parseINI :: Parser INIFile
parseINI = many1 parseSection <* skipCommentsAndEmptyLines

-- Testing Harness
main :: [String] -> IO ()
main args = do
  input <-
    case args of
      [] -> getContents
      [fileName] -> readFile fileName
      _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
  case parse parseINI input of
    Just i -> print i
    Nothing -> do
      hPutStrLn stderr "Failed to parse INI file."
      exitFailure
