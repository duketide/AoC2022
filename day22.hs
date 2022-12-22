import MyUtils (readInt, trim)
import Data.List.Split (splitOneOf, splitOn, chunksOf)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

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

moveOrRotate :: Floor -> (Point, Facing) -> String -> (Point, Facing)
moveOrRotate mp (pt, facing) dir
  | isAlpha $ head dir = (pt, rotate facing dir)
  | otherwise          = (mover mp pt facing (readInt dir), facing)

solver :: Floor -> (Point, Facing) -> [String] -> [(Point, Facing)]
solver mp = scanl $ moveOrRotate mp

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
      ((fCol, fRow), fFacing) = last allStates
      fFacingVal = case fFacing of
        E -> 0
        S -> 1
        W -> 2
        N -> 3
  print $ (fRow * 1000) + (fCol * 4) + fFacingVal
