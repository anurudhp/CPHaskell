import Data.Complex

-- Polynomial type
type Poly a = [Complex a]

__fftAux :: (RealFloat a) => a -> Poly a -> Poly a
__fftAux _ [] = []
__fftAux _ [x] = [x]
__fftAux c poly = fst res ++ snd res
  where
    n = length poly
    ls = __fftAux c [ x | (x, i) <- zip poly [1..],  odd i ]
    rs = __fftAux c [ x | (x, i) <- zip poly [1..], even i ]
    res = combine ls rs 1
      where
        ang = c * 2 * pi / (fromIntegral n)
        wn = cos ang :+ sin ang
        combine [] [] _ = ([], [])
        combine (x:xs) (y:ys) w = (x + w * y : rxs, x - w * y : rys)
          where
            (rxs, rys) = combine xs ys (w * wn)

__closestPowerOf2 :: Int -> Int
__closestPowerOf2 n = compute 1
  where
    compute x
      | x >= n = x
      | otherwise = compute $ 2 * x

__fftWrapper :: (RealFloat a) => a -> Poly a -> Poly a
__fftWrapper c poly
  | oddf (length poly) == 1 = __fftAux c poly
  | otherwise = error "FFT: size is not a power of 2!"
    where
      oddf x
        | odd x = x
        | otherwise = oddf $ x `div` 2

fft :: (RealFloat a) => Poly a -> Poly a
fft = __fftWrapper 1

ifft :: (RealFloat a) => Poly a -> Poly a
ifft poly = map (/n) $ __fftWrapper (-1) poly
  where
    n = fromIntegral $ length poly

multiply :: (RealFloat a) => Poly a -> Poly a -> Poly a
multiply ps qs = ifft $ zipWith (*) (fft $ pad ps) (fft $ pad qs)
  where
    size = __closestPowerOf2 $ length ps + length qs - 1
    pad xs = xs ++ take (size - length xs) (repeat 0)

multiplyReal :: (RealFloat a) => [a] -> [a] -> [a]
multiplyReal ps qs = map realPart $ multiply (conv ps) (conv qs)
  where
    conv = map (:+ 0)

multiplyIntegral :: (Integral a) => [a] -> [a] -> [a]
multiplyIntegral ps qs = map round $ multiplyReal (conv ps) (conv qs)
  where
    conv = map fromIntegral
