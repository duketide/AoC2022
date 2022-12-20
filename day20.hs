import MyUtils (readInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (find)
import Data.Maybe (fromJust)

type Pair = (Int, Int)

newIndex :: Pair -> Int -> Pair 
newIndex (x, y) int = ((x + y) `mod` (int - 1), y)

mixer :: Map Int Pair -> Int -> Map Int Pair
mixer mp num = go mp 0 M.empty 
  where
    go :: Map Int Pair -> Int -> Map Int Pair -> Map Int Pair
    go m n lst
      | n == num  = lst
      | otherwise = go newMap (n + 1) (M.insert n v newLst)
          where
            old@(oldInd, val) = m M.! n
            v@(newInd, _) = newIndex old num
            newMap' = M.delete n m
            newMap = M.map mapper newMap'
            newLst = M.map mapper lst
            mapper (ind, value)
             | ind == newInd = if val > 0 then if newInd > oldInd then ((ind - 1) `mod` num, value) else ((ind + 1) `mod` num, value) else if newInd < oldInd then ((ind + 1) `mod` num, value) else ((ind - 1) `mod` num, value)
             | ind > newInd && ind < oldInd = ((ind + 1) `mod` num, value)
             | ind < newInd && ind > oldInd = ((ind - 1) `mod` num, value)
             | otherwise = (ind, value)

multiMixer :: Map Int Pair -> Int -> Int -> Map Int Pair
multiMixer mp len 0 = mp
multiMixer mp len t = multiMixer (mixer mp len) len (t - 1)

main = do
  rawInput <- readFile "day20.txt"
  let input = M.fromList $ zip [0..] $ zip [0..] $ map readInt $ lines rawInput
      input' = M.fromList $ zip [0..] $ zip [0..] $ map ((*811589153) . readInt) $ lines rawInput
      mSize = M.size input
      singleMix = map snd $ M.toList $ mixer input mSize 
      tenMixes = map snd $ M.toList $ multiMixer input' mSize 10
      zeroIndex = fst $ fromJust $ find (\(x, y) -> y == 0) singleMix
      num1 = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex + 1000) `mod` mSize) singleMix
      num2 = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex + 2000) `mod` mSize) singleMix
      num3 = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex + 3000) `mod` mSize) singleMix
      zeroIndex' = fst $ fromJust $ find (\(x, y) -> y == 0) tenMixes 
      num1' = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex' + 1000) `mod` mSize) tenMixes 
      num2' = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex' + 2000) `mod` mSize) tenMixes
      num3' = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex' + 3000) `mod` mSize) tenMixes
  print $ num1 + num2 + num3
  print $ num1' + num2' + num3' 
