import MyUtils (readInt)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Types.CostCentre (currentCCS)

type Point = (Int, Int)

lineDrawer :: Point -> Point -> [Point]
lineDrawer (x1, y1) f@(x2, y2)
  | y2 > y1 = f : zip (repeat x1) [y1..y2]
  | y1 > y2 = f : zip (repeat x1) [y2..y1]
  | x2 > x1 = f : zip [x1..x2] (repeat y1)
  | x1 > x2 = f : zip [x2..x1] (repeat y1) 


rowFolder :: [Point] -> [Point] 
rowFolder row = foldl folder [head row] (tail row)
  where
    folder (x:xs) pt = lineDrawer x pt ++ xs

sandMove :: Set Point -> Point -> Point
sandMove mp (x, y)
  | not (S.member down mp) = down
  | not (S.member dl mp)   = dl
  | not (S.member dr mp)   = dr
  | otherwise              = (x, y)
    where
      down = (x, y + 1)
      dl   = (x - 1, y + 1)
      dr   = (x + 1, y + 1)

sandIter :: Set Point -> Point -> Int -> Int -> Int -> (Set Point, Bool)
sandIter mp origin maxY minX maxX = go origin
  where go current@(xc, yc)
          | result == current                      = (S.insert current mp, False)
          | xr >= maxX || xr <= minX || yr >= maxY = (mp, True) 
          | otherwise                              = go result
             where result@(xr, yr) = sandMove mp current 

sandIter' :: Set Point -> Point -> Int -> (Set Point, Bool)
sandIter' mp origin maxY = go origin
  where go current@(xc, yc)
          | result == current || yc == maxY + 1 = (S.insert current mp, False)
          | otherwise                           = go result
             where result@(xr, yr) = sandMove mp current 

voidFinder :: Set Point -> Point -> Int -> Int -> Int -> Set Point
voidFinder mp origin maxY minX maxX = go mp origin False
  where
    go :: Set Point -> Point -> Bool -> Set Point
    go mp _ True      = mp 
    go mp origin bool = go newMp origin newBool
      where
        (newMp, newBool) = sandIter mp origin maxY minX maxX

floorFinder :: Set Point -> Point -> Int -> Set Point
floorFinder mp origin maxY = go mp origin False
  where
    go :: Set Point -> Point -> Bool -> Set Point
    go mp _ True      = mp 
    go mp origin bool = if S.member (500, 0) mp then mp else go newMp origin newBool
      where
        (newMp, newBool) = sandIter' mp origin maxY

main = do
  rawInput <- readFile "day14.txt"
  let input = map (map (map readInt . splitOn ",") . filter (/= "->") . words) $ lines rawInput
      pairs = map (map (\y -> (head y, last y))) input
      startMap = S.fromList $ concatMap rowFolder pairs
      maxY = maximum $ S.map snd startMap
      maxX = maximum $ S.map fst startMap
      minX = minimum $ S.map fst startMap
      completed1 = voidFinder startMap (500, 0) maxY minX maxX
      completed2 = floorFinder startMap (500, 0) maxY
  print $ S.size completed1 - S.size startMap
  print $ S.size completed2 - S.size startMap
