import MyUtils (readDouble, readInt)
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor (second)
import Data.Char (isAlpha)

type Nums = [(String, Double)]
type Ops = [(String, [String])]

onceOver :: Nums -> Ops -> Ops 
onceOver nums ops = foldl (\ops n -> map (second (map (\z -> if z == fst n then show $ snd n else z))) ops) ops nums

prune :: Ops -> (Nums, Ops)
prune ops = result
  where
    (complete, incomplete) = partition (\(x, y) -> not (isAlpha (head $ head y) || isAlpha (head $ last y))) ops 
    result = (newNums, incomplete)
    newNums = map mapper complete
    mapper (k, x:y:z:xs) = 
      let nx = readDouble x; ny = readDouble y; nz = readDouble z in case y of
      "*" -> (k, nx * nz)
      "+" -> (k, nx + nz)
      "-" -> (k, nx - nz)
      "/" -> (k, nx / nz)

prune' :: Ops -> (Nums, Ops)
prune' ops = result
  where
    (complete, incomplete) = partition (\(x, y) -> not (isAlpha (head $ head y) || isAlpha (head $ last y) || x == "root")) ops 
    result = (newNums, incomplete)
    newNums = map mapper complete
    mapper (k, x:y:z:xs) = 
      let nx = readDouble x; ny = readDouble y; nz = readDouble z in case y of
      "*" -> (k, nx * nz)
      "+" -> (k, nx + nz)
      "-" -> (k, nx - nz)
      "/" -> (k, nx / nz)

solver :: Nums -> Ops -> Nums
solver nums ops = go nums ops []
  where
    go :: Nums -> Ops -> Nums -> Nums
    go nums []  complete = nums ++ complete
    go nums ops complete = go newNums newOps (nums ++ complete)
      where
        (newNums, newOps) = prune $ onceOver nums ops

solver' :: Nums -> Ops -> (String, [String])
solver' nums ops = go nums ops []
  where
    go :: Nums -> Ops -> Nums -> (String, [String])
    go nums [x]  complete = head $ onceOver nums [x]
    go nums ops complete = go newNums newOps (nums ++ complete)
      where
        (newNums, newOps) = prune' $ onceOver nums ops

rootChecker :: (String, [String]) -> Double
rootChecker (rt, x:y:z:xs) = readDouble x - readDouble z 

magnitudeDeterminer :: Double -> Int
magnitudeDeterminer db = length $ show $ abs $ round db

eqFinder :: Nums -> Ops -> Int -> Double
eqFinder nums ops mag = go (readDouble $ '9' : replicate (mag - 1) '0') (mag - 1)
  where
    go :: Double -> Int -> Double
    go curr place
     | place < 0   = 42.0 
     | diff == 0.0 = curr
     | diff > 0    = go (curr + 9*(10^(place - 2))) (place - 1)
     | otherwise   = go (curr - 10^(place - 1)) place
         where 
           diff   = rootChecker $ solver' numMap ops
           numMap = M.toList $ M.insert "humn" curr $ M.fromList nums


main = do
  rawInput <- readFile "day21.txt"
  let input = map ((\x -> (init $ head x, tail x)) . words) $ lines rawInput
      (nums, ops) = partition (\x -> length (snd x) == 1) input 
      numMap = map (\(x, y) -> (x, readDouble $ head y)) nums
      result = M.fromList $ solver numMap ops
      humanVal = result M.! "humn"
      numMap' = M.toList $ M.insert "humn" (humanVal + 1) $ M.fromList numMap
      diff = rootChecker $ solver' numMap ops
      diff2 = rootChecker $ solver' numMap' ops
      mag = magnitudeDeterminer (diff/(diff2 - diff))
  print $ result M.! "root"
  print $ eqFinder numMap ops mag