-- AC https://atcoder.jp/contests/abc159/submissions/17192623

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> head
      >>> solve
      >>> ( \b ->
              if b
                then "Yes"
                else "No"
          )

solve :: String -> Bool
solve s =
  and $
    (\s -> s == reverse s) <$> ([id, take half, drop (1 + half)] <*> pure s)
  where
    half = length s `div` 2
