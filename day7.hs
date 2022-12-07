import MyUtils (readInt)
import Data.List (nub, sortBy, find)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Bifunctor (second)

type Dir = (Int, [Path])
type FS = M.Map Path Dir
type Inst =[[String]]
type PWD = String
type Path = [String]

cmdFind :: Inst -> (Inst, Inst)
cmdFind = span (\x -> head x == "$") 

dirMover :: Path -> Inst -> Path
dirMover path []   = path
dirMover path (x:xs)
  | x !! 2 == ".." = dirMover (init path) xs
  | otherwise      = dirMover (path ++ [x !! 2]) xs

getFandD :: FS -> Path -> Inst -> (FS, Inst)
getFandD fs loc inst = (M.insert loc new fs, dropWhile (\x -> head x /= "$") inst)
  where current   = M.lookup loc fs
        new       = if isNothing current then (sizes, newDirs) else (fst (fromJust current) + sizes, snd (fromJust current) ++ newDirs)
        sizes     = sum $ map (readInt . head) files
        cntntList = sortBy (\a b -> head a `compare` head b) $ takeWhile (\x -> head x /= "$") inst
        files     = filter (\x -> head x /= "dir") cntntList
        dirs      = filter (\x -> head x == "dir") cntntList
        newDirs   = map (\x -> loc ++ [last x]) dirs

fullMove :: FS -> Path -> Inst -> (FS, Inst, Path)
fullMove fs path []      = (fs, [], path)
fullMove fs path inst = (nextFs, nextInst, nextPath)
 where (cmds', cts)       = cmdFind inst
       cmds               = init cmds'
       nextPath           = dirMover path cmds
       (nextFs, nextInst) = getFandD fs nextPath cts
       pwd               = last nextPath 

populate :: (FS, Inst, Path) -> FS
populate (fs, [], path)   = fs
populate (fs, inst, path) = populate (fullMove fs path inst)

sumSizes :: Dir -> FS -> Int
sumSizes (size, dirs) fs = go size dirs [] 
 where
   go :: Int -> [Path] -> [Path] -> Int
   go size [] _               = size
   go size uncounted counted
     | any (\x -> x == head uncounted) counted = go size (tail uncounted) counted
     | otherwise = go (size + fst thisDir) (tail uncounted ++ filter (\x -> isNothing (find (==x) counted)) (snd thisDir)) (head uncounted : counted)
         where thisDir = fromJust $ M.lookup (head uncounted) fs

main = do
  rawInput <- readFile "day7.txt"
  let input = lines rawInput
      inst = map words input
      populated = M.map (second nub) $ populate (M.empty, inst, [])
      sized = M.map (`sumSizes` populated) populated
      filtered = M.filter (<= 100000) sized
      solution = foldl (\acc x -> acc + snd x) 0 $ M.toList filtered
  print solution
  let usedSpace = fromJust $ M.lookup ["/"] sized
      filtered2 = M.filter (>= (usedSpace - 40000000)) sized   
  print $ minimum $ map snd $ M.toList filtered2
