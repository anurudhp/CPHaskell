-- AC https://atcoder.jp/contests/abc159/submissions/17192890

import Control.Arrow ((>>>))
import Numeric (showFFloat)

showDouble :: RealFloat a => a -> String
showDouble x = showFFloat (Just 10) x ""

main :: IO ()
main = interact $ read >>> ((^ 3) . (/ 3)) >>> showDouble
