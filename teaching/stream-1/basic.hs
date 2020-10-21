{-

int sq(int x) {
  return x * x;
}

int add(int u, int v) {
  return u + v;
}

-}

sq :: Int -> Int
sq x = x * x

add :: Int -> Int -> Int
add u v = u + v

-- Int -> (Int -> Int)
-- "currying"

plus1 :: Int -> Int
plus1 = add 1

-- myProcess True x = x + 1
-- myProcess False x = x - 1
myProcess :: Bool -> Int -> Int
myProcess True = add 1
myProcess False = add (-1)

myGCD :: Int -> Int -> Int
myGCD 0 y = y
myGCD x 0 = x
myGCD x y
  | x < y = myGCD x (y - x)
  | otherwise = myGCD (x - y) y
