{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Function
import Data.Int
import Data.List
import Data.Maybe

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output
-- main = C.interact $ runScanner (numberOf input) >>> map (solve >>> output) >>> C.unlines

-- type Input = ()
-- type Output = ()

-- input :: Scanner Input
input = undefined

-- output :: Output -> C.ByteString
output = undefined

-- solve :: Input -> Output
solve = undefined

-------------------------- Template ------------------------------------------
-- Lists
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

adjacents :: [a] -> [(a, a)]
adjacents xs = zip xs (tail xs)

adjacentsWith :: (a -> a -> b) -> [a] -> [b]
adjacentsWith f xs = zipWith f xs (tail xs)

count :: Eq a => a -> [a] -> Int
count = countBy . (==)

countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

-- Scanner
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets head

bstr :: Scanner C.ByteString
bstr = get >>= \case s : ss -> put ss >> return s

str :: Scanner String
str = C.unpack <$> bstr

readStr :: Read a => Scanner a
readStr = read <$> str

int :: Scanner Int
int = fst . fromJust . C.readInt <$> bstr

integer :: Scanner Integer
integer = readStr

int64 :: Scanner Int64
int64 = readStr

double :: Scanner Double
double = readStr

decimal :: Int -> Scanner Int
decimal p = round . ((10 ^ p) *) <$> double

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case [] -> return []; _ -> (:) <$> s <*> many s

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
  t <- peek
  if p t
    then return []
    else (:) <$> s <*> till p s

times :: Int -> Scanner a -> Scanner [a]
times = replicateM

(><) = times

two, three, four :: Scanner a -> Scanner [a]
[two, three, four] = map times [2 .. 4]

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

showB :: Show a => a -> C.ByteString
showB = C.pack . show
