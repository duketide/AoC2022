import MyUtils (readInt, trim)
import Data.List.Split (splitOneOf, splitOn)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M

type Point = (Int, Int)
data Tile = Open | Wall | Remove deriving (Show, Eq, Ord)
type Floor = Map Point Tile
data Facing = N | E | S | W deriving (Show, Eq, Ord)

instance Enum Facing where
  toEnum int = N
  fromEnum x = 1
  succ N = E
  succ E = S
  succ S = W
  succ W = N
  pred E = N
  pred N = W
  pred W = S
  pred S = E

floorMapper :: [[(Int, Tile)]] -> Floor 
floorMapper rows = go rows [] 1
  where
    go :: [[(Int, Tile)]] -> [[(Point, Tile)]] -> Int -> Map Point Tile
    go []       mapped _      = M.fromList $ concat mapped
    go (r:rows) mapped rowNum = go rows (map mapper r : mapped) (rowNum + 1) 
      where
        mapper :: (Int, Tile) -> ((Int, Int), Tile)
        mapper (int, tile) = ((int, rowNum), tile)

wrapper :: Point -> Facing -> Floor -> Point
wrapper (x,y) facing mp = point
  where
    col = filter (\(x1,y1) -> x1 == x) $ M.keys mp
    row = filter (\(x1,y1) -> y1 == y) $ M.keys mp
    wrapToW = minimum $ map fst row
    wrapToE = maximum $ map fst row
    wrapToN = minimum $ map snd col
    wrapToS = maximum $ map snd col
    point = case facing of
      N -> (x, wrapToS)
      S -> (x, wrapToN)
      E -> (wrapToW, y)
      W -> (wrapToE, y)

--this wrapper' works only for folds like mine
wrapper':: Point -> Facing -> Floor -> (Point, Facing)
wrapper' (x, y) facing mp
  | zone1 && n = ((1, x + 100), E) --zone6
  | zone1 && w = ((1, 151 - y), E) --zone4
  | zone2 && n = ((x - 100, 200), N) --zone6
  | zone2 && e = ((100, 151 - y), W) --zone5
  | zone2 && s = ((100, x - 50), W) --zone3
  | zone3 && e = ((y + 50, 50), N) --zone2
  | zone3 && w = ((y - 50, 101), S) --zone4
  | zone4 && n = ((51, x + 50), E) --zone3
  | zone4 && w = ((51, 151 - y), E) --zone1
  | zone5 && e = ((150, 151 - y), W) --zone2
  | zone5 && s = ((50, x + 100), W) --zone6
  | zone6 && w = ((y - 100, 1)  , S) --zone1
  | zone6 && e = ((y - 100, 150), N) --zone5
  | zone6 && s = ((x + 100, 1), S) --zone2
      where
        n = facing == N
        e = facing == E
        w = facing == W
        s = facing == S
        zone1 = x < 101 && y < 51
        zone2 = x > 100 && y < 51
        zone3 = y < 101 && not (zone1 || zone2)
        zone4 = y > 100 && y < 151 && x < 51
        zone5 = y > 100 && y < 151 && x > 50
        zone6 = y > 150

rotate :: Facing -> String -> Facing
rotate f d
  | d == "R"  = succ f
  | otherwise = pred f

mover :: Floor -> Point -> Facing -> Int -> Point
mover floor start facing = go start
  where
    go :: Point -> Int -> Point
    go pt        0          = pt
    go pt@(x, y) int
      | nextTile == Wall    = pt
      | otherwise           = go nextPoint (int - 1)  
        where
          nextPoint' = case facing of
            N -> (x, y - 1)
            S -> (x, y + 1)
            E -> (x + 1, y)
            W -> (x - 1, y)
          nextPoint = if M.member nextPoint' floor then nextPoint' else wrapper pt facing floor
          nextTile = floor M.! nextPoint

mover' :: Floor -> Point -> Facing -> Int -> (Point, Facing)
mover' floor start facing int = go start int facing
  where
    go :: Point -> Int -> Facing -> (Point, Facing)
    go pt        0   f      = (pt, f)
    go pt@(x, y) int f
      | nextTile == Wall    = (pt, f)
      | otherwise           = go nextPoint (int - 1) nextFacing 
        where
          nextPoint' = case f of
            N -> (x, y - 1)
            S -> (x, y + 1)
            E -> (x + 1, y)
            W -> (x - 1, y)
          wrap = not (M.member nextPoint' floor)
          wrappedTile = wrapper' pt facing floor
          nextPoint = if not wrap then nextPoint' else fst wrappedTile 
          nextFacing = if not wrap then f else snd wrappedTile
          nextTile = floor M.! nextPoint

moveOrRotate :: Floor -> (Point, Facing) -> String -> (Point, Facing)
moveOrRotate mp (pt, facing) dir
  | isAlpha $ head dir = (pt, rotate facing dir)
  | otherwise          = (mover mp pt facing (readInt dir), facing)

moveOrRotate' :: Floor -> (Point, Facing) -> String -> (Point, Facing)
moveOrRotate' mp (pt, facing) dir
  | isAlpha $ head dir = (pt, rotate facing dir)
  | otherwise          = mover' mp pt facing (readInt dir)

solver :: Floor -> (Point, Facing) -> [String] -> [(Point, Facing)]
solver mp = scanl $ moveOrRotate mp

solver' :: Floor -> (Point, Facing) -> [String] -> [(Point, Facing)]
solver' mp = scanl $ moveOrRotate' mp

main = do
  rawInput <- readFile "day22.txt"
  let input = splitOn "\n\n" rawInput
      rawMap =  filter (not . null) $ map (filter (\(x,y) -> y /= Remove) . zip [1..] . map (\x -> if x == '.' then Open else if x == '#' then Wall else Remove)) $ lines $ head input
      finalMap = floorMapper rawMap
      topRow = filter (\(x, y) -> y == 1) $ M.keys finalMap
      startingPoint = (minimum $ map fst topRow, 1)
      rawDirs = trim $ last input
      dirNums = splitOneOf "RL"  rawDirs 
      dirDirs = filter isAlpha rawDirs
      dirTups = zip dirNums dirDirs 
      finalDirs = concatMap (\(x, y) -> [x, [y]]) dirTups ++ [last dirNums]
      startState = (startingPoint, E)
      allStates = solver finalMap (startingPoint, E) finalDirs
      allStates' = solver' finalMap (startingPoint, E) finalDirs
      ((fCol, fRow), fFacing) = last allStates
      ((fCol', fRow'), fFacing') = last allStates'
      fFacingVal = case fFacing of
        E -> 0
        S -> 1
        W -> 2
        N -> 3
      fFacingVal' = case fFacing' of
        E -> 0
        S -> 1
        W -> 2
        N -> 3
  print $ (fRow * 1000) + (fCol * 4) + fFacingVal
  print $ (fRow' * 1000) + (fCol' * 4) + fFacingVal'
