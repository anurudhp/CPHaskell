module Week8 where

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

-- instance Functor (ComplicatedA f g a) where
-- fmap f (Con3 f a) =

-- Exercise 2: Rewriting monadic code
func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
  x <- xs
  return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = f . f <$> xs

func1 :: Monad f => f a -> f (a, a)
func1 xs = xs >>= (\x -> return (x, x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = (\x -> (x, x)) <$> xs

func2 :: Monad f => f a -> f (a, a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x, y))

func3 :: Monad f => f a -> f (a, a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x, x))

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
func6' = fmap (\x -> if x > 0 then (x, 0) else (0, x))

func7 :: Monad f => f Integer -> f (Integer, Integer)
func7 xs = do
  x <- xs
  if x > 0
    then return (x, 0)
    else return (0, x)

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Applicative f => f Integer -> Integer -> f Integer
func8' xs x = pure (+) <*> xs <*> pure x

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
  x <- xs >>= (\x -> return (x * x))
  return (x + 10)

-- Testing Harness
main :: IO ()
main = return ()
