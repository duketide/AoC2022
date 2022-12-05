import MyUtils
import Data.List
import Data.List.Split (splitOn)

overlap :: [[Int]] -> Bool
overlap x = common == length (last x) || common == length (head x)
  where common = length $ head x `intersect` last x

main = do
  rawInput <- readFile "day4.txt"
  let input = map (map ((\x -> [(head x)..(last x)]) . map readInt . splitOn "-") . splitOn ",") $ lines rawInput
  let solution1 = sum $ map (\x -> if overlap x then 1 else 0) input
  print solution1
  let solution2 = length $ filter (not . null) $ map (\x -> head x `intersect` last x) input 
  print solution2
