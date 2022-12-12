import MyUtils (readInt)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

data Monkey = Monkey {
                        mId :: Int
                      , mItems :: [Int]
                      , mOp :: Int -> Int 
                      , mTest :: Int
                      , mTrue :: Int
                      , mFalse :: Int
                      , handled :: Int
                      , mLcm :: Int
                      } 

type MonkeyMap = M.Map Int Monkey

monkeyMapper :: Int -> [[String]] -> Monkey 
monkeyMapper int (idList
                  :items
                  :op
                  :tst
                  :tr
                  :fls
                  :xs) = Monkey {
                                   mId=newId
                                 , mItems=startItems
                                 , mOp=newOp
                                 , mTest
                                 , mTrue=newTrue
                                 , mFalse=newFalse
                                 , handled=0
                                 , mLcm=int
                                 } 
  where
    newId = readInt [head $ last idList]
    startItems = map (readInt . filter (/= ',')) $ drop 2 items
    newOp 
      | selfOp         = (^2)
      | op !! 4 == "+" = (+ opNum)
      | otherwise      = (* opNum)
    selfOp = op !! 5 == "old"
    opNum = if selfOp then 0 else readInt (op !! 5)
    mTest = readInt $ last tst
    newTrue = readInt $ last tr
    newFalse = readInt $ last fls

updateItems :: [Int] -> Monkey -> Monkey
updateItems ints m@(Monkey {
                             mId
                           , mItems
                           , mOp
                           , mTest
                           , mTrue
                           , mFalse
                           , handled
                           , mLcm
                           }) = m {mItems=ints, handled=if null ints then handled + length mItems else handled}

monkeyTosser :: Bool -> Monkey -> (Monkey, [(Int, Int)])
monkeyTosser bool mnk
  | null $ mItems mnk = (mnk, [])
  | otherwise = (newMnk, pairs)
  where
     newMnk = updateItems [] mnk
     oldItems = mItems mnk
     op = mOp mnk 
     testNum = mTest mnk 
     pairs = map mapper oldItems
         where
            mapper item = (nextMnk, result)
               where
                  result = if bool then op item `div` 3 else op item `mod` myLcm
                  nextMnk = if test then mTrue mnk else mFalse mnk
                  test = mod result testNum == 0
                  myLcm = mLcm mnk

inserter :: (MonkeyMap, [(Int, Int)]) -> MonkeyMap 
inserter (mp, [])   = mp
inserter (mp, (x, y):xs) = inserter (newMap, xs)
  where
    oldSub = fromJust $ M.lookup x mp
    oldItems = mItems oldSub
    newItems = if null oldItems then [y] else oldItems ++ [y]
    newSub = updateItems newItems oldSub
    newMap = M.insert x newSub mp

turn :: Bool -> MonkeyMap -> Int -> MonkeyMap
turn bool mp int = inserter (newMap, pairs)
  where 
    monkey = fromJust $ M.lookup int mp
    (newMnk, pairs) = monkeyTosser bool monkey
    newMap = M.insert int newMnk mp

singleRound :: Bool -> MonkeyMap -> MonkeyMap
singleRound bool mp = go 0 mp
  where
    go num mp
      | num == M.size mp = mp
      | otherwise        = go (num + 1) (turn bool mp num)

roundIter :: Bool -> Int -> MonkeyMap -> MonkeyMap
roundIter bool int mp = go int mp
  where
    go 0   mp = mp
    go num mp = go (num - 1) (singleRound bool mp)

solver :: Bool -> Int -> MonkeyMap -> Int
solver bool int mp = product $ take 2 $ reverse $ sort $ map (\(x, y) -> handled y) $ M.toList $ roundIter bool int mp

main = do
  rawInput <- readFile "day11.txt"
  let input = map (map words . lines) $ splitOn "\n\n" rawInput
      myLcm = product $ map (\x -> readInt $ last (x !! 3)) input
      monkeys = map (monkeyMapper myLcm) input
      monkeyMap = M.fromList $ zip [0..] monkeys
  print $ solver True 20 monkeyMap 
  print $ solver False 10000 monkeyMap 
