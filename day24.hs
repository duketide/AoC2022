import MyUtils (readInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Point = (Int, Int)
type Blizzards = Set Blizzard
data Dir = N | E | W | S | Remove deriving (Show, Ord, Eq)
type Blizzard = (Point, Dir)

blizzardMapper :: [[(Int, Char)]] -> Blizzards 
blizzardMapper input = go 0 input []
  where
    go :: Int -> [[(Int, Char)]] -> [[Blizzard]] -> Blizzards 
    go _ []    set = S.filter (\(x, y) -> y /= Remove) $ S.fromList $ concat set
    go n (u:m) set = go (n +1) m (map (\(x, t) -> ((x, n), tile t)) u : set)
    tile t = case t of
      '#' -> Remove
      '.' -> Remove 
      '>' -> E
      'v' -> S
      '<' -> W
      '^' -> N

floorMapper :: [[(Int, Char)]] -> Set Point 
floorMapper input = go 0 input []
  where
    go :: Int -> [[(Int, Char)]] -> [[(Point, Char)]] -> Set Point 
    go _ []    set = S.map fst $ S.filter (\(x, y) -> y /= '#') $ S.fromList $ concat set
    go n (u:m) set = go (n +1) m (map (\(x, t) -> ((x, n), t)) u : set)

blizzardIter :: Blizzards -> (Int, Int) -> (Int, Int) -> Blizzards
blizzardIter b (wBound, eBound) (sBound, nBound) = S.map mapper b
  where
    mapper :: Blizzard -> Blizzard
    mapper ((x, y), dir) = ((newX, newY), dir)
      where
        newX
         | dir == N || dir == S = x
         | dir == E = if x+1 > eBound then wBound else x + 1
         | dir == W = if x-1 < wBound then eBound else x - 1
        newY
         | dir == E || dir == W = y
         | dir == N = if y-1 < nBound then sBound else y - 1
         | dir == S = if y+1 > sBound then nBound else y + 1

singleTurn :: Set Point -> Set Point -> Blizzards -> Set Point
singleTurn locs poss blizz = S.filter (`S.member` blizPoss) $ S.foldr (\(x, y) acc -> S.insert (x, y) $ S.insert (x+1, y) $ S.insert (x-1, y) $ S.insert (x, y+1) $ S.insert (x, y-1) acc)  S.empty locs
  where
    blizPoss = S.filter (\x -> not $ S.member x blizz') poss
    blizz' = S.map fst blizz

solver :: (Int, Int) -> (Int, Int) -> Set Point -> Set Point -> Blizzards -> Int
solver we sn locs poss blizz = go locs blizz 0
  where
    go :: Set Point -> Blizzards -> Int -> Int
    go locs blizz n
      | S.member (100, 36) locs = n
      | otherwise               = go (singleTurn locs poss nextBlizz) nextBlizz (n + 1)
          where
            nextBlizz = blizzardIter blizz we sn

main = do
  rawInput <- readFile "day24.txt"
  let input = map (zip [0..]) $ lines rawInput
      blizzards = blizzardMapper input
      floor = floorMapper input
      wBound = 1
      nBound = 1
      sBound = length input - 2
      eBound = length (head input) - 2
      we = (wBound, eBound)
      sn = (sBound, nBound)
  print $ solver we sn (S.singleton (1, 0)) floor blizzards
