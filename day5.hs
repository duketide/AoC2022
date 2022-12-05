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

type Crates = M.Map Int String

moveFrom :: Crates -> [Int] -> (Crates, Int, String)
moveFrom myMap [num, start, end] = (M.insert start newStart myMap, end, newEnd) 
  where
   starter  = resolver $ M.lookup start myMap
   newStart = drop num starter 
   newEnd   = reverse (take num starter) ++ resolver (M.lookup end myMap)

--Just remove the "reverse" from newEnd
moveFrom' :: Crates -> [Int] -> (Crates, Int, String)
moveFrom' myMap [num, start, end] = (M.insert start newStart myMap, end, newEnd) 
  where
   starter  = resolver $ M.lookup start myMap
   newStart = drop num starter 
   newEnd   = take num starter ++ resolver (M.lookup end myMap)

moveTo :: (Crates, Int, String) -> Crates 
moveTo (myMap, end, newEnd) = M.insert end newEnd myMap

moveIter :: [Int] -> Crates -> Crates
moveIter move myMap = moveTo $ moveFrom myMap move

moveIter' :: [Int] -> Crates -> Crates
moveIter' move myMap = moveTo $ moveFrom' myMap move

makeMoves :: [[Int]] -> Crates -> Crates 
makeMoves moves myMap = foldl (flip moveIter) myMap moves

makeMoves' :: [[Int]] -> Crates -> Crates 
makeMoves' moves myMap = foldl (flip moveIter') myMap moves

main = do
  rawInput <- readFile "day5.txt"
  let moveInput = map (map readInt . words . \str -> filter (`notElem` "movefrt") str) $ lines rawInput
  let solution1 = map (head . snd) $ M.toList $ makeMoves moveInput stacks 
  print solution1
  let solution2 = map (head . snd) $ M.toList $ makeMoves' moveInput stacks 
  print solution2
