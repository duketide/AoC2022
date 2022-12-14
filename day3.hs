import MyUtils
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)

create3s :: [String] -> [[String]]
create3s [] = []
create3s myList = take 3 myList : create3s (drop 3 myList)

main = do
  rawInput <- readFile "day3.txt"
  let sacks = lines rawInput
  let comps = map (\x -> let half = length x `div` 2 in [take half x, take half $ reverse x]) sacks
  let theItems = map (\x -> nub $ intersect (head x) (last x)) comps
  let alphabet = map (: []) $ ['a'..'z'] ++ ['A' .. 'Z']
  let myMap = M.fromList $ zip alphabet [1..]
  let solution1 = sum $ map (\x -> fromJust $ M.lookup x myMap) theItems 
  print solution1
  let input2 = create3s sacks 
  let badges = map (\(w:x:y:zs) -> nub $ (w `intersect` x) `intersect` y) input2
  let solution2 = sum $ map (\x -> fromJust $ M.lookup x myMap) badges
  print solution2
