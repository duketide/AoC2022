import MyUtils
import Data.List
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M

one = "RHMPZ"
two = "BJCP"
three = "DCLGHNS"
four = "LRSQDMTF"
five = "MZTBQPSF"
six = "GBZSFT"
seven = "VRN"
eight = "MCVDTLGP"
nine = "LMFJNQW"

stacks = M.fromList $ zip [1..] [one, two, three, four, five, six, seven, eight, nine]

moveFrom :: M.Map Int String -> [Int] -> (M.Map Int String, Int, String)
moveFrom myMap [num, start, end] = (M.insert start newStart myMap, end, newEnd) 
  where
   starter  = resolver $ M.lookup start myMap
   newStart = drop num starter 
   newEnd   = reverse (take num starter) ++ resolver (M.lookup end myMap)

--Just remove the "reverse" from newEnd
moveFrom' :: M.Map Int String -> [Int] -> (M.Map Int String, Int, String)
moveFrom' myMap [num, start, end] = (M.insert start newStart myMap, end, newEnd) 
  where
   starter  = resolver $ M.lookup start myMap
   newStart = drop num starter 
   newEnd   = take num starter ++ resolver (M.lookup end myMap)

moveTo :: (M.Map Int String, Int, String) -> M.Map Int String
moveTo (myMap, end, newEnd) = M.insert end newEnd myMap

moveIter :: [Int] -> M.Map Int String -> M.Map Int String
moveIter move myMap = moveTo $ moveFrom myMap move

moveIter' :: [Int] -> M.Map Int String -> M.Map Int String
moveIter' move myMap = moveTo $ moveFrom' myMap move

makeMoves :: [[Int]] -> M.Map Int String -> M.Map Int String
makeMoves moves myMap = foldl (flip moveIter) myMap moves

makeMoves' :: [[Int]] -> M.Map Int String -> M.Map Int String
makeMoves' moves myMap = foldl (flip moveIter') myMap moves

main = do
  rawInput <- readFile "day5.txt"
  let moveInput = map (map readInt . words . \str -> filter (`notElem` "movefrt") str) $ lines rawInput
  let solution1 = map (head . snd) $ M.toList $ makeMoves moveInput stacks 
  print solution1
  let solution2 = map (head . snd) $ M.toList $ makeMoves' moveInput stacks 
  print solution2
