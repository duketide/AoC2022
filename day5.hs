import MyUtils
import Data.List
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M

main = do
  rawInput <- readFile "day5.txt"
  let input = lines rawInput
  print input
  let myMap = M.singleton "a" 7
  print $ resolver $ M.lookup "a" myMap
