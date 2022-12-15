import MyUtils (readInt)
import Data.List (sort, find, nub)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)

type Point = (Int, Int)

target1 = 2000000 
target2 = 4000000

isDigOrNeg x = isDigit x || x == '-' 

manDistance :: Point -> Point -> Int
manDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findPoints :: Point -> Int -> Int -> (Int, Int)
findPoints (xs, ys) dist maxNum 
  | maxXDist < 0 = (-1, -1) 
  | otherwise = (xs + maxXDist, xs - maxXDist)
    where
      yDist  = maxNum - ys
      maxXDist  = dist - abs yDist

allPointMapper :: Int -> [Point] -> (Int, Int)
allPointMapper num (s:b:xs) = findPoints s (manDistance s b) num

pairTaker :: [[Point]] -> Point
pairTaker prs = go target2
  where
    go 0 = (-1, -1)
    go y = if flag then (x, y) else go (y - 1)
      where 
        var = filter (\x -> x/= (-1,-1)) $ map (allPointMapper y) prs
        sorted = map (\(x,y) -> let [a,b] = sort [x,y] in (a, b)) var
        (flag, x) = gapFinder sorted


gapFinder :: [(Int, Int)] -> (Bool, Int)
gapFinder row = go 1 True
  where
    go int bool
      | not bool = (True, int + 1)
      | int >= target2 = (False, 0)
      | otherwise = if not newBool then (True, int + 1) else go newInt newBool
          where
            pair@(_, newInt) = fromMaybe (-1, -1) $ find (\(a, b) -> a <= int + 1 && b > int) row
            newBool = pair /= (-1, -1)

main = do
  rawInput <- readFile "day15.txt"
  let input = map ((\x -> [(x!!2, x!!3), (x!!8, x!!9)]) . words) $ lines rawInput
      pairs = map (map (bimap (readInt . takeWhile isDigOrNeg . drop 2) (readInt . takeWhile isDigOrNeg . drop 2)))  input
      dists = map (\(x:y:xs) -> (x, manDistance x y - 1)) pairs
      beaconsOnRow = filter (\x -> snd (last x) == target1) pairs
      badX = nub $ map (fst . last) beaconsOnRow
      segments = filter (\x -> x/= (-1,-1)) $ map (allPointMapper target1) pairs
      sortedSegments = map (\(x,y) -> let [a,b] = sort [x,y] in (a, b)) segments
      mapped = S.fromList $ map (\(x, y) -> S.fromList [x..y]) sortedSegments
      con = foldl S.union S.empty mapped
      (x, y) = pairTaker pairs
  print $ S.size con - length badX
  print $ x * 4000000 + y
