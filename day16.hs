import MyUtils
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S

data Node = N {
              nId :: String
              , valve :: Int  
              , tunnels :: [String]
              } deriving (Show)

data CostNode = CN {
              cId :: String
              , cValve :: Int  
              , cTunnels :: [(Int, String)]
              } deriving (Show)

type Graph = Map String Node

--keep track of turn outside of state, could deDup based only on pos and opened (keeping highest score), but might not be worht it
--need to write a function that spawns all successor states given a current state
data State = State {
                   pos :: String
                   , opened :: Set String 
                   , score :: Int
                   , imPrev :: String
                   } deriving (Show)

stateSpawns :: Graph -> Int -> State -> [State]
stateSpawns graph turn s@(State { pos, opened, score, imPrev }) = newStates 
  where
    newStates = if sameValve > 0 && notOpened then (State {pos = pos, opened = S.insert pos opened, score = score+sameScore, imPrev = ""}) : tnls else tnls
    sameNode = fromJust $ M.lookup pos graph 
    sameValve = valve sameNode
    mult = turn - 1
    sameScore = sameValve * mult
    notOpened = not (S.member pos opened)
    tnls =  filter (\x@(State {pos=p}) -> p /= imPrev) $ map mapper $ tunnels sameNode
    mapper tn = s {pos=tn, imPrev=pos} 

stateIter :: [State] -> Graph -> Int -> [State]
stateIter sts graph turn = concatMap (stateSpawns graph turn) sts

thirtyTurns :: Graph -> State -> [State]
thirtyTurns graph initState = go [initState] 30 
  where
    go :: [State] -> Int -> [State]
    go sts 1 = sts
    go sts t = go (stateIter sts graph t) (t - 1)
      where
        filt :: [State] -> [State]
        filt sts = filter (\x -> score x == localMax x) sts
          where
            localMax x = maximum (locals x)
            locals x = map score $ filter (\st -> pos st == pos x) sts

nodeMaker :: [String] -> Node
nodeMaker (a:b:c:d:e:f:g:h:i:xs) = N {nId=b, valve=int, tunnels=pruned}
  where
    int = readInt $ drop 5 $ init e
    pruned = map (filter (/= ',')) xs

valueAdder :: Graph -> Node -> CostNode
valueAdder gr n = CN {cId = nId n, cValve = valve n, cTunnels = map mapper (tunnels n)}
  where
    mapper :: String -> (Int, String) 
    mapper str = (valve (fromJust $ M.lookup str gr), str)

main = do
  rawInput <- readFile "day16.txt"
  let input = map words $ lines rawInput
      rawNodes = map nodeMaker input
      graph = M.fromList $ map (\x -> (nId x, x)) rawNodes
      var = M.map (valueAdder graph) graph
      result = thirtyTurns graph (State {pos = "AA", opened = S.empty, score = 0, imPrev = ""})
  print $ maximum $ map score result 
