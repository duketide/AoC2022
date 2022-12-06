import Data.List

func :: String -> Int -> Int
func str size = go (take size str) (drop size str) size 
 where
  go :: String -> String -> Int -> Int
  go test str pos
    | length (nub test) == size = pos
    | otherwise = go (tail test ++ [head str]) (tail str) (pos + 1)

main = do
  rawInput <- readFile "day6.txt"
  let solution  = func rawInput 4
      solution2 = func rawInput 14
  print solution
  print solution2
