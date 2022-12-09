import MyUtils (readInt)
import qualified Data.Set as S

type Point = (Int, Int)

headMover :: Point -> String -> Point
headMover (x, y) dir
  | dir == "U" = (x, y + 1)
  | dir == "D" = (x, y - 1)
  | dir == "R" = (x + 1, y)
  | dir == "L" = (x - 1, y)

tailMover :: Point -> Point -> Point
tailMover (x1, y1) (x2, y2)
  | adjacent   = (x2, y2)
  | horizontal = (x2 + xm, y2)
  | vertical   = (x2, y2 + ym)
  | otherwise  = (x2 + xm, y2 + ym)
      where
        dx = x1 - x2
        dy = y1 - y2
        xm = if dx > 0 then 1 else (-1)
        ym = if dy > 0 then 1 else (-1)
        horizontal = y1 == y2
        vertical = x1 == x2
        adjacent = abs dx <= 1 && abs dy <= 1

fullMove :: ([Point], S.Set Point) -> String -> ([Point], S.Set Point)
fullMove (knots, visited) str = go movedHead (tail knots) [movedHead] visited
  where
    movedHead = headMover (head knots) str
    go :: Point -> [Point] -> [Point] -> S.Set Point -> ([Point], S.Set Point)
    go _      []        result visited = (reverse result, S.insert (head result) visited)
    go leader followers result visited = go nextLeader nextFollowers nextResult visited
      where
        nextLeader = tailMover leader (head followers)
        nextFollowers = tail followers
        nextResult = nextLeader : result

main = do
  rawInput <- readFile "day9.txt"
  let input = map words $ lines rawInput
      singles = concatMap (\x -> replicate (readInt $ last x) (head x)) input
      (_, mvs) = foldl fullMove ([(0,0), (0,0)], S.empty) singles
      primeStarter = (replicate 10 (0,0), S.empty)
      (_, mvs') = foldl fullMove primeStarter singles
  print $ S.size mvs 
  print $ S.size mvs'
