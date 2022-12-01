import MyUtils
import Data.List.Split
import Data.List

main = do
  rawInput <- readFile "day1.txt"
  let input = map (sum . map readInt . lines) $ splitOn "\n\n" rawInput
  let solution1 = maximum input 
  let list2 = reverse $ sort input
  let solution2 = head list2 + list2 !! 1 + list2 !! 2
  print solution1 
  print solution2
