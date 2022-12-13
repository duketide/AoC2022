--Used Monday Morning Haskell's Dijkstra blog post as a guide:
--https://mmhaskell.com/blog/2022/8/22/dijkstras-algorithm-in-haskell.

import Data.Char(ord)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sort)

data DState = DState
  { nodeQueue :: MinPrioHeap Int Int
  , distMap :: Map Int Int
  , visited :: Set Int
  }

type Index = Int
type Height = Int
type Neighbors = [Index]
type Graph = Map Index (Height, Neighbors)

trillion = 10^12

nodeGrapher :: Graph -> Int -> Int -> Graph 
nodeGrapher graph len totLen = go (M.size graph - 1) graph
  where
    go :: Index -> Graph -> Graph
    go (-1) graph = graph
    go index graph = go (index - 1) $ M.insert index (height, filtered) graph
      where
        height = fst $ fromJust $ M.lookup index graph
        unfiltered = [up, down, left, right]
        up = index - len
        down = index + len
        left = if index `mod` len == 0 then (-1) else index - 1
        right = if index `mod` len + 1 == 0 then (-1) else index + 1
        filtered = filter (\x -> x >= 0 && x < totLen) unfiltered

trav :: Graph -> Int -> Int -> Map Int Int 
trav graph src dest = go startState
  where
    startQueue = H.singleton (0, src) :: MinPrioHeap Int Int
    distMap = M.singleton src 0
    visited = S.empty
    startState = DState startQueue distMap visited
    go :: DState -> Map Int Int
    go ds@(DState q0 d0 v0) = case H.view q0 of
      Nothing -> d0
      Just ((minDist, node), q1) -> if node == dest then d0
        else if S.member node v0 then go (ds {nodeQueue=q1})
        else
          let v1 = S.insert node v0
              neighbors = snd $ fromJust $ M.lookup node graph
              filtNeighbors = filter (\x -> not $ S.member x v0) neighbors
            in go $ foldl (folder node) (DState q1 d0 v1) filtNeighbors    
              where 
                folder :: Int -> DState -> Int -> DState
                folder current ds@(DState q1 d0 v1) neigh =
                  let altDist = fromMaybe trillion (M.lookup current d0) + cost
                      curHeight = fst $ graph M.! current
                      neighHeight = fst $ graph M.! neigh
                      cost = if neighHeight > curHeight + 1 then trillion else 1 
                  in if altDist < fromMaybe trillion (M.lookup neigh d0)
                      then DState (H.insert (altDist, neigh) q1) (M.insert neigh altDist d0) v1 
                      else ds


multiTrav :: Graph -> [Int] -> Int -> [Int]
multiTrav graph srcs dest = go srcs []
  where
    go :: [Int] -> [Int] -> [Int]
    go []   dists = dists
    go (x:xs) dists = go xs (fromMaybe trillion (M.lookup dest $ trav graph x dest):dists)


main = do
  rawInput <- readFile "day12.txt"
  let input = lines rawInput
      lineLen = length $ head input
      indexed = M.fromList $ zip [0..] $ zip (concat input) (repeat [0])
      source = fst $ head $ M.toList $ M.filter (\(x, y) -> x == 'S') indexed 
      otherSources = map fst $ M.toList $ M.filter (\(x, y) -> x == 'a') indexed
      dest = fst $ head $ M.toList $ M.filter (\(x, y) -> x == 'E') indexed 
      nums = M.map (\(y,z) ->if y `elem` ['a'..'z'] then (ord y, z) else if y == 'S' then (ord 'a', z) else (ord 'z', z)) indexed
      mapSize = M.size nums
      queue = H.singleton (0, source) :: MinPrioHeap Int Int
      distMap = M.singleton source 0
      graph = nodeGrapher nums lineLen mapSize
      traversed = trav graph source dest
      dists = multiTrav graph (source:otherSources) dest
  print $ traversed M.! dest
  print $ minimum dists 
