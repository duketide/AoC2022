import MyUtils
import Data.List (transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Crates = M.Map Int String

moveFrom :: Crates -> [Int] -> Bool -> (Crates, Int, String)
moveFrom myMap [num, start, end] bool = (M.insert start newStart myMap, end, newEnd) 
  where
   starter  = fromJust $ M.lookup start myMap
   newStart = drop num starter 
   deposit  = take num starter
   newEnd   = (if bool then reverse deposit else deposit) ++ fromJust (M.lookup end myMap)

moveTo :: (Crates, Int, String) -> Crates 
moveTo (myMap, end, newEnd) = M.insert end newEnd myMap

moveIter :: [Int] -> Crates -> Bool -> Crates
moveIter move myMap bool = moveTo $ moveFrom myMap move bool

makeMoves :: [[Int]] -> Crates -> Bool -> Crates 
makeMoves moves myMap bool = foldl (\acc x -> moveIter x acc bool) myMap moves

main = do
  rawInput <- readFile "day5.txt"
  let crates = map trim $ filter (any (`elem` ['A'..'Z'])) $ transpose $ init $ lines $ head $ splitOn "\n\n" rawInput
      stacks = M.fromList $ zip [1..] crates 
      moveInput = map (map readInt . words . \str -> filter (`notElem` "movefrt") str) $ lines $ last $ splitOn "\n\n" rawInput
      solution1 = map (head . snd) $ M.toList $ makeMoves moveInput stacks True 
  print solution1
  let solution2 = map (head . snd) $ M.toList $ makeMoves moveInput stacks False 
  print solution2
