import MyUtils (readInt)
import Data.List.Split (chunksOf)
import Data.Char (isAlpha)

counter :: [String] -> Int -> Int
counter strs int = int * (1 + sum (map readInt $ filter  (not . isAlpha . head) $ concatMap words $ filter (\x -> head x == 'a') $ take (int -1) strs))

sprtPos :: Int -> [Int] -> [Int] -> [Int]
sprtPos _ []    result = reverse result 
sprtPos n moves result = sprtPos (n + head moves) (tail moves) ((n + head moves) : result)

main = do
 rawInput <- readFile "day10.txt"
 let input = lines rawInput
     padded = concatMap (\x -> if head x == 'a' then ["addx 0", x] else [x]) input
     total = counter padded
     input2 = map words padded
     moves = map (\x -> if length x > 1 then readInt $ last x else 0) input2
     posByCycle = init $ 1 : sprtPos 1 moves []
     posWithCycle = zip posByCycle (map (`mod` 40) [0..])
     result = unlines $ chunksOf 40 $ concatMap (\x -> if abs (uncurry (-) x) < 2 then "#" else ".") posWithCycle
 print $ total 20 + total 60 + total 100 + total 140 + total 180 + total 220 
 putStr result
