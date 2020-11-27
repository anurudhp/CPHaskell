import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $ words >>> drop 1 >>> map (read >>> solve >>> showAns) >>> unlines
  where
    showAns xs = show (length xs) ++ "\n" ++ unwords (map show xs)

type I64 = Integer

solve :: I64 -> [I64]
solve n
  | null fs =
    case root 1 n n of
      Nothing -> [n]
      Just r -> [r, r]
  | otherwise =
    foldl1 (zipWith (*)) $
    map (\(p, f) -> replicate (k - f) 1 ++ replicate f p) $ (extra, 1) : fs
  where
    fs = factorize n
    extra = foldl (\v (p, m) -> iterate (`div` p) v !! m) n fs
    k = maximum (map snd fs)

primes :: [I64]
primes = filter prime [2 .. 3120]
  where
    prime p = all ((/= 0) . (p `mod`)) [2 .. p - 1]

factorize :: I64 -> [(I64, Int)]
factorize n = filter ((> 0) . snd) . map f $ primes
  where
    f p = (p, mult n p)
    mult u d
      | u `mod` d == 0 = 1 + mult (u `div` d) d
      | otherwise = 0

root :: I64 -> I64 -> I64 -> Maybe I64
root l r n
  | l > r = Nothing
  | mid * mid == n = Just mid
  | mid * mid > n = root l (mid - 1) n
  | mid * mid < n = root (mid + 1) r n
  where
    mid = (l + r) `div` 2
