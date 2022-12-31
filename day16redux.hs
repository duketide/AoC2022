import MyUtils
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (find, nub, maximumBy, tails)

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
                   , freeze :: Bool
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
stateSpawns graph turn s@(State { pos, opened, score, freeze }) = newStates 
  where
    newStates
      | S.size opened == M.size graph - 1 = [s]
      | freeze                            = [s]
      | timer == 1                        = [s{pos = (dest, 0), opened = S.insert dest opened, score = score+sameScore}]
      | timer == 0                        = (s{freeze=True}) : tnls
      | otherwise                         = [s {pos = (dest, timer -1)}]
    sameValve = fst $ fromJust $ M.lookup dest graph 
    (dest, timer) = pos
    mult = turn - 1
    sameScore = sameValve * mult
    notOpened x = not (S.member x opened)
    tnls = map (\x -> s {pos = x}) cnxs
    cnxs = filter (\(x, y) -> notOpened x) $ snd $ fromJust $ M.lookup dest graph

stateIter :: [State] -> TGraph -> Int -> [State]
stateIter sts graph turn = sts >>= stateSpawns graph turn

turns :: Int -> TGraph -> State -> [State]
turns int graph initState = go [initState] int 
  where
    go :: [State] -> Int -> [State]
    go sts 1 = sts
    go sts t = go (stateIter sts graph t) (t - 1)

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
      oState = (State {pos = ("AA", 0), opened = S.empty, score = 0, freeze=False})
      result = turns 30 tGraph oState
      resultList = M.fromListWith max [(opened x, score x) | x <- turns 26 tGraph oState]
      pairs = [x + y | (ox, x) : a <- tails (M.assocs resultList), (oy, y) <- a, S.null (S.intersection ox oy)] 
  print $ score $ maximumBy (\x y -> score x `compare` score y) result
  print $ maximum pairs 
