import MyUtils (readInt)
import Data.List.Split (chunksOf)

counter :: [Int] -> Int -> Int
counter strs int = int * (strs !! (int - 1))

sprtPos :: Int -> [Int] -> [Int] -> [Int]
sprtPos _ []    result = reverse result 
sprtPos n (m : moves) result = sprtPos (n + m) moves ((n + m) : result)

main = do
 rawInput <- readFile "day10.txt"
 let padded = map words $ concatMap (\x -> if head x == 'a' then ["0", x] else [x]) $ lines rawInput
     moves = map (\x -> if length x > 1 then readInt $ last x else 0) padded
     posByCycle = init $ 1 : sprtPos 1 moves []
     posWithCycle = zip posByCycle (map (`mod` 40) [0..])
     total = counter posByCycle
     result = unlines $ chunksOf 40 $ concatMap (\x -> if abs (uncurry (-) x) < 2 then "#" else ".") posWithCycle
 print $ sum $ map total [20, 60..220] 
 putStr result
