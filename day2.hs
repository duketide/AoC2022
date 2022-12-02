import MyUtils
import Data.List
import Data.List.Split

mapper str
 | str == "A Y" = 8
 | str == "B X" = 1
 | str == "C Z" = 6
 | str == "A X" = 4
 | str == "A Z" = 3
 | str == "B Y" = 5
 | str == "B Z" = 9
 | str == "C X" = 7
 | str == "C Y" = 2

mapper2 str
 | str == "A Y" = 4
 | str == "B X" = 1
 | str == "C Z" = 7
 | str == "A X" = 3
 | str == "A Z" = 8
 | str == "B Y" = 5
 | str == "B Z" = 9
 | str == "C X" = 2
 | str == "C Y" = 6


main = do
  rawInput <- readFile "day2.txt"
  let input1 = map mapper $ lines rawInput 
  let solution1 = sum input1
  print solution1
  let input2 = map mapper2 $ lines rawInput
  let solution2 = sum input2
  print solution2
