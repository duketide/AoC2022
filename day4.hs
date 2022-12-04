import MyUtils
import Data.List
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M

resolve :: Maybe a -> a 
resolve (Just x) = x
resolve Nothing = error "You've committed a grave error. Stop trying."

main = do
  rawInput <- readFile "day4.txt"
  let input1 = map (map ((\x -> [(head x)..(last x)]) . map readInt . splitOn "-") . splitOn ",") $ lines rawInput
  let solution1 = sum $ map (\x -> if length (intersect (head x) (last x)) == length (last x) || length (intersect (head x) (last x)) == length (head x) then 1 else 0) input1
  print solution1
  let input2 = input1
  let solution2 = length $ filter (\x -> length x > 0) $ map (\x -> intersect (head x) (last x)) input2 
  print solution2
