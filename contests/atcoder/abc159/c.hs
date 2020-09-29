-- AC https://atcoder.jp/contests/abc159/submissions/11116857

import Control.Arrow
import Numeric

main =
  interact $
    read
      >>> solve
      >>> (\x -> showFFloat (Just 10) x "")

solve :: Double -> Double
solve l = (l / 3) ^ 3
