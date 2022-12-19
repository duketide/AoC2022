import MyUtils (readInt)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Vector = Vector {vx :: Int, vy :: Int, vz :: Int} deriving (Show, Ord, Eq)


above, below, behind, before, toLeft, toRight :: Vector -> Vector
above v@(Vector {vx, vy, vz}) = v {vy = vy + 1}
below v@(Vector {vx, vy, vz}) = v {vy = vy - 1}
behind v@(Vector {vx, vy, vz}) = v {vz = vz + 1}
before v@(Vector {vx, vy, vz}) = v {vz = vz - 1}
toLeft v@(Vector {vx, vy, vz}) = v {vx = vx - 1}
toRight v@(Vector {vx, vy, vz}) = v {vx = vx + 1}

naiveTally :: Vector -> Set Vector -> Int
naiveTally vec sv = length $ filter not [
                                        S.member (above vec) sv
                                        , S.member (below vec) sv
                                        , S.member (behind vec) sv
                                        , S.member (before vec) sv
                                        , S.member (toLeft vec) sv
                                        , S.member (toRight vec) sv
                                        ]

mapInput :: Set Vector -> Vector -> [Vector] 
mapInput sv vec = filter (not . flip S.member sv) 
                    [above vec, below vec, behind vec, before vec, toLeft vec, toRight vec]

isInside :: Vector -> Set Vector -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
isInside vec sv maxX minX maxY minY maxZ minZ = go (S.singleton vec) (S.singleton vec)
  where
    go :: Set Vector -> Set Vector -> Bool
    go current visited
     | null nextVecs      = True
     | any isFree current = False
     | otherwise          = go nextVecs (S.union visited nextVecs)
       where
         isFree v = vx v > maxX || vx v < minX || vy v > maxY || vy v < minY || vz v > maxZ || vz v < minZ
         nextVecs' :: Vector -> Set Vector
         nextVecs' v = S.fromList $ filter (\x -> not (S.member x sv) && not  (S.member x visited))
                                       [above v, below v, behind v, before v, toLeft v, toRight v]
         nextVecs = S.foldl (\acc v -> S.union acc (nextVecs' v)) S.empty current

main = do
  rawInput <- readFile "day18.txt"
  let input = map ((\(x:y:z:xs) -> Vector {vx = x, vy = y, vz = z}) . map readInt . splitOn ",") $ lines rawInput
      cubeSet = S.fromList input
      result1 = S.foldl (\acc cube -> acc + naiveTally cube cubeSet) 0 cubeSet
      cx = S.map vx cubeSet
      cy = S.map vy cubeSet
      cz = S.map vz cubeSet
      maxX = maximum cx
      minX = minimum cx
      maxY = maximum cy
      minY = minimum cy
      maxZ = maximum cz
      minZ = minimum cz
      candidates = concatMap (mapInput cubeSet) input
      candSet = S.fromList candidates
      candMap = foldl (\acc cand -> M.insert cand (fromMaybe 0 (M.lookup cand acc) + 1) acc) M.empty candidates
      airPockets = S.filter (\v -> isInside v cubeSet maxX minX maxY minY maxZ minZ) candSet 
      num = S.foldl (\acc ap -> acc + fromMaybe 0 (M.lookup ap candMap)) 0 airPockets
  print result1 
  print $ result1 - num 
