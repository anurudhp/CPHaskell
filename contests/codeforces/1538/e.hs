import Control.Arrow ((>>>))
import Data.List (tails)
import qualified Data.Map as M
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ lines >>> drop 1 >>> process >>> map show >>> unlines

type I64 = Integer

process :: [String] -> [I64]
process [] = []
process (n:ss) = let (hs, ts) = splitAt (read n) ss in solve hs : process ts

-- Left = Small, Right = Large
type Value = Either String (String, I64, String)
type Id = String
type Env = M.Map Id Value

count :: Value -> I64
count (Left _) = 0
count (Right (_, c, _)) = c

build :: String -> Value
build s
  | length s <= 3 = Left s
  | otherwise =
    Right
      ( take 3 s
      , toInteger . length . filter ((== "haha") . take 4) $ tails s
      , reverse . take 3 . reverse $ s)

combine :: Value -> Value -> Value
combine (Left l) (Left r) = build (l ++ r)
combine (Left l) (Right (rl, c, rr)) =
  case build (l ++ rl) of
    (Left l') -> Right (l', c, rr)
    (Right (l', c', _)) -> Right (l', c' + c, rr)
combine (Right (ll, c, lr)) (Left r) =
  case build (lr ++ r) of
    (Left r') -> Right (ll, c, r')
    (Right (_, c', r')) -> Right (ll, c + c', r')
combine (Right (ll, cl, lr)) (Right (rl, cr, rr)) =
  Right (ll, cl + (count . build $ lr ++ rl) + cr, rr)

solve :: [String] -> I64
solve ss =
  count . fromJust $
  M.lookup
    (takeWhile (/= ' ') $ last ss)
    (foldl (flip (update . words)) M.empty ss)
  where
    update [var, ":=", val] env = M.insert var (build val) env
    update [var, "=", lhs, "+", rhs] env =
      M.insert
        var
        (fromJust $ combine <$> M.lookup lhs env <*> M.lookup rhs env)
        env
