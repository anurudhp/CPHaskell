module Week7 where

-- Exercise 1: Fibonacci numbers
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0 ..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 2: Streams
data Stream a =
  Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show ss = show $ streamToList ss

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate g x = Cons x (streamIterate g (g x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) (Cons y ys) =
  Cons x (Cons y (streamInterleave xs ys))

nats :: Stream Integer
nats = streamIterate (+ 1) 0

-- TODO: I don't really understand how this works...
ruler :: Stream Integer
ruler = Cons 0 (streamInterleave (streamMap (+ 1) ruler) (streamRepeat 0))

-- Exercise 3: The Supply monad
data Supply s a =
  S (Stream s -> (a, Stream s))

get :: Supply s s
get =
  S (\xs ->
       case xs of
         (Cons x xs') -> (x, xs'))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S sa) =
  S (\s ->
       let (x, s') = sa s
        in (f x, s'))

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S sa) (S sb) =
  S (\s ->
       let (x, s') = sa s
           (y, s'') = sb s'
        in (f x y, s''))

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S sa) f =
  S (\s ->
       let (x, s') = sa s
        in case (f x) of
             S sb -> sb s')

runSupply :: Stream s -> Supply s a -> a
runSupply s (S sa) = fst (sa s)

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply

data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node lt rt) = do
      lt' <- go lt
      rt' <- go rt
      return (Node lt' rt')
    go (Leaf _) =
      S
        (\s ->
           case s of
             (Cons x s') -> (Leaf x, s'))

-- Testing Harness --
main :: [String] -> IO ()
main _ = return ()
