import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Point = (Int, Int)
data Dir = N | E | S | W deriving (Show, Eq, Ord)
data Tile = Occupied | Not deriving (Show, Eq)
data Elf = Elf {loc :: Point, proposed :: [Point]} deriving (Show, Eq, Ord)

instance Enum Dir where
  toEnum int = N
  fromEnum x = 1
  succ N = S
  succ E = N
  succ S = W
  succ W = E
  pred E = W
  pred N = E
  pred W = S
  pred S = N

initElfLocator :: [String] -> Set Elf 
initElfLocator elves = go 0 elves []
  where
    go :: Int -> [String] -> [[(Point, Tile)]] -> Set Elf 
    go _ []     done = S.fromList $ map (\x -> Elf (fst x) []) $ filter (\(_, y) -> y == Occupied) $ concat done
    go n (e:xs) done = go (n+1) xs (mapped : done)
      where
        mapped = foldl (\acc space -> ((length acc, n), if space == '#' then Occupied else Not) : acc) [] e

mover :: Point -> Dir -> Point
mover (x, y) N = (x, y - 1)
mover (x, y) S = (x, y + 1)
mover (x, y) W = (x-1, y)
mover (x, y) E = (x+1, y)

generateProposals :: Set Elf -> Dir -> Set Elf
generateProposals elves dir = S.map (\e@(Elf {loc, proposed}) -> e {proposed = prop loc}) elves
  where
    locs = S.map loc elves
    prop pt
     | first && second && third && fourth = []
     | first = [mover pt dir]
     | second = [mover pt $ succ dir]
     | third = [mover pt $ succ $ succ dir]
     | fourth = [mover pt $ pred dir]
     | otherwise = []
       where
         first = dirCheck dir
         second = dirCheck (succ dir)
         third = dirCheck (succ $ succ dir)
         fourth = dirCheck (pred dir)
         n (x, y) = not (S.member (x-1, y-1) locs || S.member (x, y-1) locs || S.member (x+1, y-1) locs)
         e (x, y) = not (S.member (x+1, y-1) locs || S.member (x+1, y) locs || S.member (x+1, y+1) locs)
         w (x, y) = not (S.member (x-1, y-1) locs || S.member (x-1, y) locs || S.member (x-1, y+1) locs)
         s (x, y) = not (S.member (x-1, y+1) locs || S.member (x, y+1) locs || S.member (x+1, y+1) locs)
         dirCheck N = n pt
         dirCheck E = e pt
         dirCheck W = w pt
         dirCheck S = s pt

mapProposals :: Set Elf -> Map Point Int
mapProposals = foldl (\acc elf -> if null $ proposed elf then acc else M.insert (head $ proposed elf) (fromMaybe 0 (M.lookup (head $ proposed elf) acc) + 1) acc) M.empty

makeMoves :: Set Elf -> Map Point Int -> Set Elf
makeMoves s m = S.map (\e@(Elf {loc, proposed}) -> if null proposed then e else if m M.! head proposed == 1 then Elf {loc = head proposed, proposed = []} else e {proposed = []}) s

singleRound :: Set Elf -> Dir -> Set Elf
singleRound s d = makeMoves setElf (mapProposals setElf)
  where
    setElf = generateProposals s d 

severalRounds :: Set Elf -> Dir -> Int -> Set Elf
severalRounds s _ 0 = s
severalRounds s d n = severalRounds (singleRound s d) (succ d) (n - 1)

severalRounds' :: Set Elf -> Dir -> Int -> Int
severalRounds' s d n
  | all null (S.map proposed (generateProposals s d)) = n
  | otherwise                                         = severalRounds' (singleRound s d) (succ d) (n + 1)

main = do
 rawInput <- readFile "day23.txt"
 let input = initElfLocator $ lines rawInput
     numElves = S.size input
     tenRounds = S.map loc $ severalRounds input N 10
     xMin = minimum $ S.map fst tenRounds
     xMax = maximum $ S.map fst tenRounds
     yMin = minimum $ S.map snd tenRounds
     yMax = maximum $ S.map snd tenRounds
     area = (xMax - xMin + 1) * (yMax - yMin + 1)
 print $ area - numElves 
 print $ severalRounds' input N 1

