import MyUtils
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (group, intersperse, sortBy)
import Data.Bifunctor (bimap)
import Data.Function (on)

type Point = (Int, Int)
data Move = D | R | L deriving (Eq, Show)
data Shape = Wide | Crs | El | Tall | Sqr deriving (Show, Eq)

shapeFunc :: Shape -> (Point -> Set Point -> Move -> Point)
shapeFunc shape = case shape of
  Wide -> wide
  Crs -> cross
  El -> el
  Tall -> tall
  Sqr -> sqr

instance Enum Shape where
  toEnum 1 = Wide
  fromEnum Wide = 1
  succ Wide = Crs
  succ Crs = El
  succ El = Tall
  succ Tall = Sqr
  succ Sqr = Wide

move :: Point -> Move -> Point
move (x, y) mv = case mv of
  D -> (x, y-1)
  R -> (x+1, y)
  L -> (x-1, y)

--use far left
wide, tall, cross, el, sqr :: Point -> Set Point -> Move -> Point
wide p@(x, y) rocks mv = 
  let down  = not $ S.member (x, y-1) rocks || 
                    S.member (x+1, y-1) rocks ||
                    S.member (x+2, y-1) rocks ||
                    S.member (x+3, y-1) rocks 
      right = not $ x + 3 == 6 || 
                    S.member (x+4, y) rocks
      left  = not $ x == 0 ||
                    S.member (x-1, y) rocks
      bool 
        | mv == D   = down
        | mv == R   = right
        | otherwise = left
  in  if bool then move p mv else p

--use bottom
tall p@(x, y) rocks mv =
  let right = not $ S.member (x+1, y) rocks || 
                    S.member (x+1, y+1) rocks ||
                    S.member (x+1, y+2) rocks ||
                    S.member (x+1, y+3) rocks ||
                    x == 6
      left  = not $ S.member (x-1, y) rocks || 
                    S.member (x-1, y+1) rocks ||
                    S.member (x-1, y+2) rocks ||
                    S.member (x-1, y+3) rocks ||
                    x == 0
      down  = not $ S.member (x, y-1) rocks
      bool 
        | mv == D   = down
        | mv == R   = right
        | otherwise = left
  in  if bool then move p mv else p


--use bottom left
el p@(x, y) rocks mv =
  let right = not $ S.member (x+3, y) rocks || 
                    S.member (x+3, y+1) rocks ||
                    S.member (x+3, y+2) rocks ||
                    x + 2 == 6
      left  = not $ S.member (x-1, y) rocks || 
                    S.member (x+1, y+1) rocks ||
                    S.member (x+1, y+2) rocks ||
                    x == 0
      down  = not $ S.member (x, y-1) rocks ||
                    S.member (x+1, y-1) rocks ||
                    S.member (x+2, y-1) rocks
      bool 
        | mv == D   = down
        | mv == R   = right
        | otherwise = left
  in  if bool then move p mv else p

--use bottom
cross p@(x, y) rocks mv =
  let right = not $ S.member (x+1, y) rocks || 
                    S.member (x+2, y+1) rocks ||
                    S.member (x+1, y+2) rocks ||
                    x + 1 == 6
      left  = not $ S.member (x-1, y) rocks || 
                    S.member (x-2, y+1) rocks ||
                    S.member (x-1, y+2) rocks || 
                    x - 1 == 0
      down  = not $ S.member (x, y-1) rocks ||
                    S.member (x-1, y) rocks ||
                    S.member (x+1, y) rocks
      bool 
        | mv == D   = down
        | mv == R   = right
        | otherwise = left
  in  if bool then move p mv else p

--use bottom left
sqr p@(x, y) rocks mv =
  let right = not $ S.member (x+2, y) rocks || 
                    S.member (x+2, y+1) rocks ||
                    x + 1 == 6
      left  = not $ S.member (x-1, y) rocks || 
                    S.member (x-1, y+1) rocks ||
                    x == 0
      down  = not $ S.member (x, y-1) rocks ||
                    S.member (x+1, y-1) rocks ||
                    S.member (x+1, y) rocks
      bool 
        | mv == D   = down
        | mv == R   = right
        | otherwise = left
  in  if bool then move p mv else p

moveIter :: Int -> Set Point -> String -> (Set Point, [(Int, Int)])
moveIter rest starterSet moves = go starterSet Wide (2, 4) moves rest []
  where
    go :: Set Point -> Shape -> Point -> String -> Int -> [(Int, Int)] -> (Set Point, [(Int, Int)])
    go rocks _     _   _       0       cycles = (rocks, cycles)
    go rocks shape loc (m:mvs) restNum cycles = go filtRocks nextShape nextLoc nextMoves nextNum nextCycles
      where
        mv
          | m == '<'  = L
          | m == '>'  = R
          | otherwise = D
        mover = shapeFunc shape
        tryLoc = mover loc rocks mv
        nextLoc 
          | not toRest        = tryLoc
          | nextShape == Crs  = (3, highest + 4)
          | otherwise         = (2, highest + 4)
        highest = maximum $ S.map snd filtRocks
        nextMoves = if null mvs then moves else mvs 
        toRest = mv == D && tryLoc == loc
        nextShape = if toRest then succ shape else shape
        nextNum = if toRest then restNum - 1 else restNum
        nextCycles = if nextMoves == moves then (rest - nextNum, highest) : cycles else cycles
        wideEx = mapper [(1, 0), (2, 0), (3, 0)]
        tallEx = mapper [(0, 1), (0, 2), (0, 3)]
        crsEx = mapper [(0, 1), (0, 2), (1, 1), (-1, 1)]
        elEx = mapper [(1, 0), (2, 0), (2, 1), (2, 2)]
        sqrEx = mapper [(1, 0), (0, 1), (1, 1)]
        folder = foldr S.insert rocks
        mapper xs = map (bimap (+ fst loc) (+ snd loc)) $ (0, 0) : xs
        maxLn = maximum $ S.map snd $ S.filter (\(_, y) -> S.member (0, y) nextRocks && S.member (1, y) nextRocks && S.member (2, y) nextRocks && S.member (3, y) nextRocks && S.member (4, y) nextRocks && S.member (5, y) nextRocks && S.member (6, y) nextRocks ) nextRocks
        filtRocks = if not toRest then rocks else S.filter (\(_, y) -> y >= maxLn) nextRocks
        nextRocks = case shape of
                   Wide -> folder wideEx
                   Tall -> folder tallEx
                   Crs -> folder crsEx
                   El -> folder elEx
                   Sqr -> folder sqrEx

main = do
  rawInput <- readFile "day17.txt"
  let startSet = S.fromList $ zip [0..6] (repeat 0)
      input = trim $ intersperse 'D' rawInput
      result = moveIter 2022 startSet input
      result' = moveIter (length input) startSet input
      cycles = reverse $ snd result'
      grpCycles = sortBy (compare `on` length) $ group $ snd $ foldl (\((x, y), diffs) (x2, y2) -> ((x2, y2), (x2 - x, y2 - y) : diffs)) ((0, 0), []) cycles
      (shapesOffTheTop, heightOffTheTop) = head $ head grpCycles
      (shapeCycle, heightCycle) = head $ last grpCycles
      cyclesNeeded = (10^12 - shapesOffTheTop) `div` shapeCycle
      remainderNeeded = 10^12 - (cyclesNeeded * shapeCycle) - shapesOffTheTop
      heightAfterCycles = heightOffTheTop + (cyclesNeeded * heightCycle)
  print $ maximum $ S.map snd $ fst result
  print $ heightAfterCycles + (maximum (S.map snd $ fst $ moveIter (shapesOffTheTop + remainderNeeded) startSet input) - heightOffTheTop)
