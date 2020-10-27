{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map (read >>> solve >>> showAns) >>> unlines
  where
    showAns Nothing = "-1"
    showAns (Just (x, y, z)) = unwords $ show <$> [x, y, z]

type TIII = (Int, Int, Int)

solve :: Int -> Maybe TIII
solve n = dp !! n
  where
    dp :: [Maybe TIII]
    dp =
      [ Just (0, 0, 0),
        Nothing,
        Nothing,
        Just (1, 0, 0),
        Nothing,
        Just (0, 1, 0),
        Just (2, 0, 0)
      ]
        ++ zipWith3
          try3
          (map (i3 <$>) $ drop 4 dp)
          (map (i5 <$>) $ drop 2 dp)
          (map (i7 <$>) dp)

    i3 :: TIII -> TIII
    i3 (x, y, z) = (x + 1, y, z)
    i5 :: TIII -> TIII
    i5 (x, y, z) = (x, y + 1, z)
    i7 (x, y, z) = (x, y, z + 1)

    try3 x y z = x `tryNext` y `tryNext` z

tryNext :: Maybe a -> Maybe a -> Maybe a
tryNext (Just x) _ = Just x
tryNext _ (Just y) = Just y
tryNext _ _ = Nothing
