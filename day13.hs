import MyUtils (readInt)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.List (sortBy)


isOpen :: Char -> Bool
isOpen ch = ch == '['

isClose :: Char -> Bool
isClose ch = ch == ']'

comparison :: [String] -> [String] -> Ordering
comparison [] (x:xs) = LT
comparison (x:xs) [] = GT
comparison lx@(x:xs) ly@(y:ys)
  | isDigit subX && isDigit subY = case compare (readInt x) (readInt y) of
     LT -> LT
     GT -> GT
     EQ -> comparison xs ys
  | not (isClose subX) && isClose subY = GT 
  | isClose subX && not (isClose subY) = LT
  | isClose subX && isClose subY = comparison xs ys
  | isOpen subX && isOpen subY = comparison xs ys
  | isOpen subX = comparison lx ("[":y:"]":ys)
  | isOpen subY = comparison ("[":x:"]":xs) ly
    where
      subX = head x
      subY = head y

main = do
  rawInput <- readFile "day13.txt"
  let padded = concatMap (\x -> if x == '[' then [x, ','] else if x == ']' then [',', x] else [x]) rawInput  
      input = map (map (splitOn ",") . lines) $ splitOn "\n\n" padded 
      filterEmpties = map (map (filter (not . null))) input
      mapped = map (\(x:y:zs) -> comparison x y) filterEmpties 
      div2 = ["[","[","2","]","]"]
      div6 = ["[","[","6","]","]"]
      concatMapped = div6 : div2 : concat filterEmpties 
      zipMapped = zip [1..] mapped
      sorted = sortBy comparison concatMapped
      zipSorted = zip [1..] sorted
  print $ sum $ map fst $ filter (\x -> snd x == LT) zipMapped 
  print $ product $ map fst $ filter (\(x,y) -> y == div2 || y == div6) zipSorted
