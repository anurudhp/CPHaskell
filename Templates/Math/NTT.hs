__nttAux :: (Integral a) => a -> a -> [a] -> [a]
__nttAux _ _ [] = []
__nttAux _ _ [x] = [x]
__nttAux m g poly = map ((`mod` m) . (+m) . (`mod` m)) $ uncurry (++) res
  where
    n = length poly
    ls = __nttAux m (g * g) [ x | (x, i) <- zip poly [1..],  odd i ]
    rs = __nttAux m (g * g) [ x | (x, i) <- zip poly [1..], even i ]
    res = combine ls rs 1
      where
        combine [] [] _ = ([], [])
        combine (x:xs) (y:ys) w = (x + w * y : rxs, x - w * y : rys)
          where
            (rxs, rys) = combine xs ys ((w * g) `mod` m)

__closestPowerOf2 :: (Integral a) => a -> a
__closestPowerOf2 n = compute 1
  where
    compute x
      | x >= n = x
      | otherwise = compute $ 2 * x

-- x^n mod m
modpow :: (Integral a) => a -> a -> a -> a
modpow m x n
  | n == 0 = 1
  | odd n = (x * modpow m x (n - 1)) `mod` m
  | otherwise = let y = modpow m x (n `div` 2) in (y * y) `mod` m

data NTT a = NTT a a a

ntt :: (Integral a) => NTT a -> [a] -> [a]
ntt (NTT m g l) ps
  | __closestPowerOf2 (length ps) == length ps = __nttAux m gg (map (`mod` m) ps)
  | otherwise = error "NTT: size is not a power of 2!"
    where
      gg = modpow m g (l `div` fromIntegral (length ps))

intt :: (Integral a) => NTT a -> [a] -> [a]
intt (NTT m g l) ps = map ((`mod` m) . (*ni)) $ ntt (NTT m gi l) ps 
  where
    gi = modpow m g (m - 2)
    ni = modpow m (fromIntegral $ length ps) (m - 2)

multiply :: (Integral a) => NTT a -> [a] -> [a] -> [a]
multiply d@(NTT m g l) ps qs = 
  intt d $ zipWith (*) (ntt d $ pad ps) (ntt d $ pad qs)
    where
      size = __closestPowerOf2 $ length ps + length qs - 1
      pad xs = xs ++ replicate (size - length xs) 0

-- Examples
ntt7340033 = NTT 7340033 5 (2^20)
ntt998244353 = NTT 998244353 961777435 (2^23)
