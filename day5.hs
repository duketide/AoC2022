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

moveFrom :: Crates -> [Int] -> Bool -> (Crates, Int, String)
moveFrom myMap [num, start, end] bool = (M.insert start newStart myMap, end, newEnd) 
  where
   starter  = resolver $ M.lookup start myMap
   newStart = drop num starter 
   deposit  = take num starter
   newEnd   = (if bool then reverse deposit else deposit) ++ resolver (M.lookup end myMap)

moveTo :: (Crates, Int, String) -> Crates 
moveTo (myMap, end, newEnd) = M.insert end newEnd myMap

moveIter :: [Int] -> Crates -> Bool -> Crates
moveIter move myMap bool = moveTo $ moveFrom myMap move bool

makeMoves :: [[Int]] -> Crates -> Bool -> Crates 
makeMoves moves myMap bool = foldl (\acc x -> moveIter x acc bool) myMap moves

main = do
  rawInput <- readFile "day5.txt"
  let moveInput = map (map readInt . words . \str -> filter (`notElem` "movefrt") str) $ lines rawInput
  let solution1 = map (head . snd) $ M.toList $ makeMoves moveInput stacks True 
  print solution1
  let solution2 = map (head . snd) $ M.toList $ makeMoves moveInput stacks False 
  print solution2
