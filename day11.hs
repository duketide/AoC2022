import MyUtils
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as C
import Data.Char

type MnkMap = M.Map Int Mnky 
type Mnky = M.Map String String

monkeyMapper :: Int -> [[String]] -> Mnky 
monkeyMapper int (id:items:op:tst:tr:fls:xs) = M.fromList monkeyMap 
  where
    mId = [head $ last id]
    mItems = map (filter (/= ',')) $ drop 2 items
    mOp = unwords $ drop 4 op
    mTest = last tst
    mTrue = last tr
    mFalse = last fls
    monkeyMap = [("id", mId), ("items", unwords mItems), ("op", mOp), ("test", mTest), ("true", mTrue), ("false", mFalse), ("handled", "0"), ("lcm", show int)]

monkeyTosser :: Bool -> Mnky -> (Mnky, [(Int, Int)])
monkeyTosser bool mnk
  | fromJust (M.lookup "items" mnk) == "empty" = (mnk, [])
  | otherwise = (M.insert "handled" newNum (M.insert "items" "empty" mnk), pairs)
  where
     newNum = show $ length items + readInt (fromJust $ M.lookup "handled" mnk)
     items = map readInt $ words $ fromJust $ M.lookup "items" mnk
     op = words $ fromJust $ M.lookup "op" mnk 
     operator = if head op == "+" then (+) else (*)
     old = last op == "old"
     testNum = readInt $ fromJust $ M.lookup "test" mnk
     pairs = map mapper items
         where
            mapper item = (newMnk, result)
               where
                  result = if bool then operator operand item `div` 3 else operator operand item `mod` myLcm
                  newMnk = if test then readInt $ fromJust $ M.lookup "true" mnk else readInt $ fromJust $ M.lookup "false" mnk
                  operand  = if old then item else readInt (last op)
                  test = mod result testNum == 0
                  myLcm = readInt $ fromJust $ M.lookup "lcm" mnk

inserter :: (MnkMap, [(Int, Int)]) -> MnkMap 
inserter (mp, [])   = mp
inserter (mp, (x, y):xs) = inserter (newMap, xs)
  where
    oldSub = fromJust $ M.lookup x mp
    oldItems = fromJust $ M.lookup "items" oldSub
    newItems = if oldItems == "empty" then show y else oldItems ++ " " ++ show y
    newSub = M.insert "items" newItems oldSub
    newMap = M.insert x newSub mp

turn :: Bool -> MnkMap -> Int -> MnkMap
turn bool mp int = inserter (newMap, pairs)
  where 
    monkey = fromJust $ M.lookup int mp
    (newMnk, pairs) = monkeyTosser bool monkey
    newMap = M.insert int newMnk mp

singleRound :: Bool -> MnkMap -> MnkMap
singleRound bool mp = go 0 mp
  where
    go 8   mp = mp
    go num mp = go (num + 1) (turn bool mp num)

roundIter :: Bool -> Int -> MnkMap -> MnkMap
roundIter bool int mp = go int mp
  where
    go 0   mp = mp
    go num mp = go (num - 1) (singleRound bool mp)

main = do
  rawInput <- readFile "day11.txt"
  let input = map (map words . lines) $ splitOn "\n\n" rawInput
      myLcm = product $ map (\x -> readInt $ last (x !! 3)) input
      monkeys = map (monkeyMapper myLcm) input
      monkeyMap = M.fromList $ zip [0..] monkeys
      result1 = product $ take 2 $ reverse $ sort $ map (\(x, y) -> readInt $ fromJust $ M.lookup "handled" y) $ M.toList $ roundIter True 20 monkeyMap
      result2 = product $ take 2 $ reverse $ sort $ map (\(x, y) -> readInt $ fromJust $ M.lookup "handled" y) $ M.toList $ roundIter False 10000 monkeyMap
  print result1
  print result2
