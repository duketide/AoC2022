import MyUtils (readInt)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (find, sort)
import Data.Maybe (fromJust)

type Pair = (Int, Int)

newIndex :: Pair -> Int -> Pair 
newIndex (x, y) int = ((x + y + wrap) `mod` int, y)
 where
   wrap = (x + y) `div` int

mixer :: Map Int Pair -> Int -> [Pair]
mixer mp num = go mp 0 []
  where
    go :: Map Int Pair -> Int -> [Pair] -> [Pair]
    go m n lst
      | n == num  = lst
      | otherwise = go newMap (n + 1) (v : newLst)
          where
            old@(oldInd, val) = m M.! n
            v@(newInd, _) = newIndex old num
            newMap' = M.delete n m
            newMap = M.map mapper newMap'
            newLst = map mapper lst
            mapper (ind, value)
             | ind == newInd = if val > 0 then if newInd > oldInd then ((ind - 1) `mod` num, value) else ((ind + 1) `mod` num, value) else if newInd < oldInd then ((ind + 1) `mod` num, value) else ((ind - 1) `mod` num, value)
             | ind > newInd && ind < oldInd = ((ind + 1) `mod` num, value)
             | ind < newInd && ind > oldInd = ((ind - 1) `mod` num, value)
             | otherwise = (ind, value)

main = do
  rawInput <- readFile "day20.txt"
  let input = M.fromList $ zip [0..] $ zip [0..] $ map readInt $ lines rawInput
      input' = M.fromList $ zip [0..] $ zip [0..] $ map ((*811589153) . readInt) $ lines rawInput
      mSize = M.size input
      singleMix = mixer input mSize 
      singleMix' = mixer input' mSize
      zeroIndex = fst $ fromJust $ find (\(x, y) -> y == 0) singleMix
      num1 = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex + 1000) `mod` mSize) singleMix
      num2 = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex + 2000) `mod` mSize) singleMix
      num3 = snd $ fromJust $ find (\(x, y) -> x == (zeroIndex + 3000) `mod` mSize) singleMix
  print $ num1 + num2 + num3
  print $ sort singleMix'
  print $ sort singleMix
