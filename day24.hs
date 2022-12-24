import MyUtils (readInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Point = (Int, Int)
data Tile = Wall | B {n :: Bool, e :: Bool, s :: Bool, w :: Bool} deriving (Show, Ord, Eq)
type Valley = Map Point (Point, Tile)
type Blizzards = Set Blizzard
data Dir = N | E | W | S | Remove deriving (Show, Ord, Eq)
type Blizzard = (Point, Dir)

{-inputMapper :: [[(Int, Char)]] -> Valley
inputMapper input = go 0 input []
  where
    go :: Int -> [[(Int, Char)]] -> [[(Point, (Point, Tile))]] -> Valley
    go _ []    mapped = M.fromList $ concat mapped
    go n (u:m) mapped = go (n +1) m (map (\(x, t) -> ((x,n), ((x, n), tile t))) u : mapped)
    open = B {n = False, e = False, s = False, w = False}
    tile t = case t of
      '#' -> Wall
      '.' -> open
      '>' -> open {e=True}
      'v' -> open {s=True} 
      '<' -> open {w=True} 
      '^' -> open {n=True} 

mapIter :: Valley -> Valley
mapIter v = M.map (\t@(x, y) -> if y == Wall then t else (x, B {n = xn x, e = xe x, s = xs x, w = xw x})) v
  where 
    xn x = n $ v M.! nNeighbor
    xs x = s $ v M.! sNeighbor
    xe x = e $ v M.! eNeighbor
    xw x = w $ v M.! wNeighbor
    nNeighbor' (x, y) = M.lookup 
-}


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

blizzardIter :: Blizzards -> (Int, Int) -> (Int, Int) -> Blizzards
blizzardIter b (wBound, eBound) (sBound, nBound) = S.map mapper b
  where
    mapper :: Blizzard -> Blizzard
    mapper ((x, y), dir) = ((newX, newY), dir)
      where
        newX
         | dir == N || dir == S = x
         | dir == E = if newX' > eBound then wBound else x + 1
         | dir == W = if newX' < wBound then eBound else x - 1
        newY
         | dir == E || dir == W = y
         | dir == N = if newX' < nBound then sBound else y - 1
         | dir == S = if newX' > sBound then nBound else y + 1
        newX'
         | dir == E = x + 1
         | dir == W = x - 1
         | otherwise = x
        newY'
         | dir == S = y + 1
         | dir == N = y - 1
         | otherwise = y

main = do
  rawInput <- readFile "day24.txt"
  let input = map (zip [0..]) $ lines rawInput
      blizzards = blizzardMapper input
      wBound = 1
      nBound = 1
      sBound = length input - 1
      eBound = length (head input) - 1
      we = (wBound, eBound)
      sn = (sBound, nBound)

  print $ blizzardIter blizzards we sn
