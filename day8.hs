import MyUtils (readInt)
import Data.List (transpose)

mapper :: [(Int, Bool)] -> [(Int, Bool)]
mapper trees = go (-1) trees []
  where
    go _   []      result = reverse result
    go max current result
      | num > max = go num (tail current) ((num, True) : result)
      | otherwise = go max (tail current) (keep : result)
          where
            keep = head current
            num  = fst keep 

mapper' :: [(Int, [Int])] -> [(Int, [Int])]
mapper' trees = go trees []
  where
    go [] result       = reverse result 
    go current result = go (tail current) ((num, newView :view) : result)
      where
        tree = head current
        num = fst tree
        view = snd tree
        checklist = map fst $ tail current
        viewUntilStop = length $ takeWhile (<num) checklist
        newView' = if viewUntilStop == length checklist then viewUntilStop else viewUntilStop + 1
        newView = max newView' 1

fullMapper :: ([a] -> [a]) -> [[a]] -> [[a]]
fullMapper func lst = fourth
  where
     first = map func lst
     second = map (func . reverse) first
     third = map func $ transpose second
     fourth = map (func . reverse) third


main = do
  rawInput <- readFile "day8.txt"
  let input = map (map (\x -> (readInt [x], False))) $ lines rawInput
      first = fullMapper mapper input
      solution = sum $ map (foldr (\x acc -> if snd x then acc + 1 else acc) 0) first
      input2 = map (map (\x -> (readInt [x], []))) $ lines rawInput
      second = fullMapper mapper' input2
      chopped = tail $ init $ transpose $ tail $ init second
      solution2 = maximum $ concatMap (map (product . snd)) chopped 
  print solution 
  print solution2
