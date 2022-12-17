import MyUtils
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (find, nub)

data Node = N {
              nId :: String
              , valve :: Int  
              , tunnels :: [String]
              } deriving (Show)

type Graph = Map String Node

type TGraph = Map String (Int, [(String, Int)])

data State = State {
                     pos :: (String, Int)
                   , opened :: Set String
                   , score :: Int
                   } deriving (Show)

data TwoState = Two {
                      pos1 :: (String, Int)
                    , pos2 :: (String, Int)
                    , opened2 :: Set String
                    , score2 :: Int
                    } deriving (Show)

bfs :: Graph -> String -> String -> (String, Int)
bfs gr src dest = go 0 [src] 
  where
    go :: Int -> [String] -> (String, Int)
    go trn locs
      | isJust (find (==dest) locs) = (dest, trn)
      | otherwise = go (trn + 1) newTrn
        where
          newTrn = nub $ concatMap (\x -> tunnels $ fromJust $ M.lookup x gr) locs
        
bfsMapper :: Graph -> Set String -> Map String (Int, [(String, Int)])
bfsMapper gr rms = M.fromList $ S.toList $ S.map mapper rms
  where
    mapper rm = (rm, (vlv, map (bfs gr rm) $ S.toList $ S.delete "AA" $ S.delete rm rms))
      where vlv = valve $ fromJust $ M.lookup rm gr

stateSpawns :: TGraph -> Int -> State -> [State]
stateSpawns graph turn s@(State { pos, opened, score }) = newStates 
  where
    newStates
      | timer == 1  = [State {pos = (dest, 0), opened = S.insert dest opened, score = score+sameScore}]
      | timer == 0 = tnls
      | otherwise   = [s {pos = (dest, timer -1)}]
    sameValve = fst $ fromJust $ M.lookup dest graph 
    (dest, timer) = pos
    mult = turn - 1
    sameScore = sameValve * mult
    notOpened x = not (S.member x opened)
    tnls = map (\x -> s {pos = x}) cnxs
    cnxs = filter (\(x, y) -> notOpened x) $ snd $ fromJust $ M.lookup dest graph

stateSpawns' :: TGraph -> Int -> TwoState -> [TwoState]
stateSpawns' graph turn s@(Two { pos1, pos2, opened2, score2 }) = newStates 
  where
    newStates
      | timer1 > 1 && timer2 > 1    = [Two {pos1 = next1, pos2= next2, opened2 = opened2, score2 = score2}]
      | timer1 == 1 && timer2 > 1   = [Two {pos1 = next1, pos2= next2, opened2 = S.insert dest1 opened2, score2 = score2 + oneScore}]
      | timer2 == 1 && timer1 > 1   = [Two {pos1 = next1, pos2= next2, opened2 = S.insert dest2 opened2, score2 = score2 + twoScore}]
      | timer1 == 1 && timer2 == 1  = [Two {pos1 = next1, pos2= next2, opened2 = S.insert dest2 $ S.insert dest1 opened2, score2 = if dest1 == dest2 then score2+oneScore else score2+oneScore+twoScore}]
      | timer1 == 0 && timer2 > 1   = tnls1 
      | timer2 == 0 && timer1 > 1   = tnls2 
      | timer1 == 0 && timer2 == 1  = tnls1score 
      | timer2 == 0 && timer1 == 1  = tnls2score 
      | otherwise                   = multiTuns
    oneValve = fst $ fromJust $ M.lookup dest1 graph 
    twoValve = fst $ fromJust $ M.lookup dest2 graph 
    (dest1, timer1) = pos1
    (dest2, timer2) = pos2
    next1 = (dest1, timer1 - 1)
    next2 = (dest2, timer2 - 1)
    mult = turn - 1
    oneScore = if notOpened dest1 then oneValve * mult else 0
    twoScore = if notOpened dest2 then twoValve * mult else 0
    notOpened x = not (S.member x opened2)
    tnls1 = map (\x -> s {pos1 = x, pos2=next2}) cnxs1
    tnls1score = map (\x -> s {pos1 = x, pos2=next2, opened2 = S.insert dest2 opened2, score2 = score2 + twoScore}) cnxs1
    tnls2score = map (\x -> s {pos2 = x, pos1=next1, opened2 = S.insert dest1 opened2, score2 = score2 + oneScore}) cnxs2
    cnxs1 = filter (\(x, y) -> notOpened x) $ snd $ fromJust $ M.lookup dest1 graph
    tnls2 = map (\x -> s {pos2 = x, pos1=next1}) cnxs2
    cnxs2 = filter (\(x, y) -> notOpened x) $ snd $ fromJust $ M.lookup dest2 graph
    multiTuns  = map (\(x,y) -> s {pos1 = x, pos2 = y}) multiPairs
    multiPairs = filter (uncurry (/=)) $ concatMap folder cnxs1
    folder cn1 = foldl (\acc cn2 -> (cn1, cn2) :  acc) [] cnxs2 


stateIter :: [State] -> TGraph -> Int -> [State]
stateIter sts graph turn = concatMap (stateSpawns graph turn) sts

stateIter' :: [TwoState] -> TGraph -> Int -> [TwoState]
stateIter' sts graph turn = concatMap (stateSpawns' graph turn) sts

turns :: Int -> TGraph -> State -> [State]
turns int graph initState = go [initState] int 
  where
    go :: [State] -> Int -> [State]
    go sts 1 = sts
    go sts t = go (stateIter sts graph t) (t - 1)

turns' :: Int -> TGraph -> TwoState -> [TwoState]
turns' int graph initState = go [initState] int 
  where
    go :: [TwoState] -> Int -> [TwoState]
    go sts 1 = sts
    go sts t = go (stateIter' sts graph t) (t - 1)

nodeMaker :: [String] -> Node
nodeMaker (a:b:c:d:e:f:g:h:i:xs) = N {nId=b, valve=int, tunnels=pruned}
  where
    int = readInt $ drop 5 $ init e
    pruned = map (filter (/= ',')) xs

main = do
  rawInput <- readFile "day16.txt"
  let input = map words $ lines rawInput
      rawNodes = map nodeMaker input
      scoreNodes = S.fromList $ "AA" : map nId (filter (\x -> valve x > 0) rawNodes)
      graph = M.fromList $ map (\x -> (nId x, x)) rawNodes
      tGraph = bfsMapper graph scoreNodes
      result = turns 30 tGraph (State {pos = ("AA", 0), opened = S.empty, score = 0})
      result' = turns' 26 tGraph (Two {pos1 = ("AA", 0), pos2 = ("AA", 0), opened2 = S.empty, score2 = 0})
  print $ maximum $ map score result
  print $ maximum $ map score2 result' 
