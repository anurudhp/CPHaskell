import Control.Arrow ((>>>))

main :: IO ()
main = interact $ read >>> max 0 >>> show
