import MyUtils (readInt)
import Data.Set (Set)
import qualified Data.Set as S

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

stateIter :: State -> [State]
stateIter st@(S {blueprint, target, candidates, numOre, numClay, numObs, numGeo, robOre, robClay, robObs, robGeo}) = nextStates
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
   nextTemplate = st {target=target, candidates = candidates', numOre = numOre', numClay = numClay', numObs = numObs', numGeo = numGeo', robOre = robOre', robClay = robClay', robObs = robObs', robGeo = robGeo'}
   nextStates = if spend then map (\x -> nextTemplate {target=x}) candidates' else [nextTemplate]

initStates :: Blueprint -> Set State
initStates bp = S.fromList [template, template {target=Clay}]
  where
    template = S { blueprint = bp,
                   target = Ore,
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

multiTurn :: Set State -> Int -> Set State
multiTurn sts 0 = sts  
multiTurn sts n = ( multiTurn $ S.filter (\x -> maxPossible x >= maxCurrent) unfiltered) (n-1)
  where
    unfiltered = S.fromList $ concatMap stateIter sts
    maxCurrent = maximum $ S.map (\x -> numGeo x + n * robGeo x) unfiltered
    maxPossible x = numGeo x + (robGeo x * (n^2 + n) `div` 2) 

solver :: [Set State] -> Int -> Int
solver sts int = foldl (folder (int -1)) 0 sts 
  where
    folder int acc st = acc + maximum (S.map (\x -> bpId (blueprint x) * (numGeo x + robGeo x)) $ multiTurn st int)
  

main = do
  rawInput <- readFile "day19.txt"
  let input = map words $ lines rawInput
      blueprints = map (\x -> BP {bpId = readInt $ init $ x !! 1, oreCost = readInt $ x !! 6, clayCost = readInt $ x !! 12, obsCost = (readInt $ x !! 18, readInt $ x !! 21), geoCost = (readInt $ x !! 27, readInt $ x !! 30)}) input
      initHeadStates = initStates (head blueprints)
      initLasttates = initStates (last blueprints)
      allInitStates = map initStates blueprints
  print $ solver allInitStates 24
