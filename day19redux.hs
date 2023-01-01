import MyUtils (readInt)
import Data.Set (Set)
import Data.Heap (MaxPrioHeap)
import qualified Data.Heap as H
import qualified Data.Set as S
import Data.Maybe (fromJust)

data Blueprint = BP 
  { bpId :: Int,
    oreCost :: Int,
    clayCost :: Int,
    obsCost :: (Int, Int),
    geoCost :: (Int, Int)
  } deriving (Show, Eq, Ord)

data Robot = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

data State = S
  { blueprint :: Blueprint,
    target :: Robot,
    minutes :: Int,
    candidates :: [Robot],
    numOre :: Int,
    numClay :: Int,
    numObs :: Int,
    numGeo :: Int,
    robOre :: Int,
    robClay :: Int,
    robObs :: Int,
    robGeo :: Int
  } deriving (Show, Eq, Ord)

data MState = M
  { queue :: MaxPrioHeap Int State,
    visited :: Set State
  } deriving (Show, Eq, Ord)

stateIter :: State -> [State]
stateIter st@(S {blueprint, target, minutes, candidates, numOre, numClay, numObs, numGeo, robOre, robClay, robObs, robGeo}) = nextStates
  where
   robotType = target 
   orePrice = oreCost blueprint
   clayPrice = clayCost blueprint
   (obsOrePrice, obsClayPrice) = obsCost blueprint  
   (geoOrePrice, geoObsPrice) = geoCost blueprint
   spend = case robotType of
     Ore -> numOre >= orePrice
     Clay -> numOre >= clayPrice
     Obsidian -> numOre >= obsOrePrice && numClay >= obsClayPrice
     Geode -> numOre >= geoOrePrice && numObs >= geoObsPrice
   candidates'
     | robObs' > 0 = [Ore, Clay, Obsidian, Geode]
     | robClay > 0 = [Ore, Clay, Obsidian]
     | otherwise   = [Ore, Clay]
   numOre' = numOre + robOre - spentOre
   numClay' = numClay + robClay - spentClay
   numObs' = numObs + robObs - spentObs
   numGeo' = numGeo + robGeo
   robOre' = if spend && robotType == Ore then robOre + 1 else robOre
   robClay' = if spend && robotType == Clay then robClay + 1 else robClay
   robObs' = if spend && robotType == Obsidian then robObs + 1 else robObs 
   robGeo' = if spend && robotType == Geode then robGeo + 1 else robGeo
   spentObs = if spend && robotType == Geode then geoObsPrice else 0
   spentClay = if spend && robotType == Obsidian then obsClayPrice else 0
   spentOre = if not spend then 0 else case robotType of
     Ore -> orePrice
     Clay -> clayPrice
     Obsidian -> obsOrePrice
     Geode -> geoOrePrice
   nextTemplate = st {target=target, minutes=minutes + 1, candidates = candidates', numOre = numOre', numClay = numClay', numObs = numObs', numGeo = numGeo', robOre = robOre', robClay = robClay', robObs = robObs', robGeo = robGeo'}
   nextStates = if spend then map (\x -> nextTemplate {target=x}) candidates' else [nextTemplate]

initMetaState :: Int-> Blueprint -> MState
initMetaState turns bp = M (H.fromList [(numInc 0 turns, template), (numInc 0 turns, template {target=Clay})]) S.empty
  where
    template = S { blueprint = bp,
                   target = Ore,
                   minutes = 0,
                   candidates = [Ore, Clay],
                   numOre = 0,
                   numClay = 0,
                   numObs = 0,
                   numGeo = 0,
                   robOre = 1,
                   robClay = 0,
                   robObs = 0,
                   robGeo = 0
                 }

numInc :: Int -> Int -> Int
numInc x y = go (x +1) (y -1) x
  where
    go x y z
      | y <= 0    = z
      | otherwise = go (x + 1) (y -1) (x + z)

multiTurn :: MState -> Int -> State
multiTurn mst n = if timer == n then node else multiTurn nextMSt n
  where
    q0 = queue mst 
    ((prio, node), q1) = fromJust $ H.view q0
    timer = minutes node
    vis = visited mst
    visCheck = node{minutes=0}
    neighbors = stateIter node
    nextVis = S.insert visCheck vis
    nextQ = if S.member visCheck vis then q1 else foldr (\nd q -> H.insert (maxPossible nd, nd) q) q1 neighbors
    nextMSt = M nextQ nextVis
    maxPossible x = numGeo x + numInc (robGeo x) (n - timer)

solver :: [MState] -> Int -> Int
solver sts int = foldl (folder int) 0 sts 
  where
    folder int acc st = acc + (bpId (blueprint winningState) * numGeo winningState)
      where winningState = multiTurn st int

main = do
  rawInput <- readFile "day19.txt"
  let input = map words $ lines rawInput
      blueprints = map (\x -> BP {bpId = readInt $ init $ x !! 1, oreCost = readInt $ x !! 6, clayCost = readInt $ x !! 12, obsCost = (readInt $ x !! 18, readInt $ x !! 21), geoCost = (readInt $ x !! 27, readInt $ x !! 30)}) input
      allInitStates = map (initMetaState 24) blueprints
      part2InitStates = map (initMetaState 32) $ take 3 blueprints
      part2WinningStates = map (`multiTurn` 32) part2InitStates 
  print $ solver allInitStates 24
  print $ product $ map numGeo part2WinningStates
