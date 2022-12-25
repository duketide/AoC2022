import Data.Map (Map)
import qualified Data.Map as M
import Data.List (find)
import Data.Either (isLeft, isRight, fromRight)
import Data.Maybe (fromJust)

type MonkeyTree = Map String (Either Int [String])
data Monkey = M {mId :: String, payload :: Either Int [String]}
type MonkeyTree' = Map String Monkey

readTree :: [String] -> Either Int [String]
readTree l
 | length l == 1 = Left (read (head l))
 | otherwise     = Right l

opFinder :: String -> (Int -> Int -> Int)
opFinder "+" = (+)
opFinder "-" = (-)
opFinder "/" = div
opFinder "*" = (*)

eval :: String -> MonkeyTree' -> Int
eval st mt = case payload $ mt M.! st of
  Left int -> int
  Right [left, op, right] -> opFinder op (eval left mt) (eval right mt)

finder :: String -> Monkey -> Bool
finder node v
  | isRight (payload v) && node `elem` fromRight [""] (payload v) = True
  | otherwise                                                     = False

revEval :: String -> MonkeyTree -> MonkeyTree' -> Int 
revEval st mt mt'
  | op == "==" = other
  | op == "+" || op == "*" = upward `revOp` other
  | otherwise = other `revOp` upward
      where
        monkey = fromJust $ find (finder st) mt'
        newExp = fromRight ["","",""] $ payload monkey  
        upward = revEval (mId monkey) mt mt'
        op = newExp !! 1
        other' = head $ filter (\x -> length x == 4 && x /= st) newExp
        other = eval other' mt'
        otherIsFirst = head newExp == other'
        revOp = case op of
          "+" -> (-)
          "*" -> div
          "-" -> if otherIsFirst then (-) else (+)
          "/" -> if otherIsFirst then div else (*)
  
main = do
  rawInput <- readFile "day21.txt"
  let input = map words $ lines rawInput
      inputMap = foldr (\(x:xs) acc -> M.insert (init x) (readTree xs) acc) M.empty input
      inputMap' = foldr (\(x:xs) acc -> M.insert (init x) (M {mId = init x, payload = readTree xs}) acc) M.empty input
      [x, y, z] = fromRight ["","",""] $ inputMap M.! "root"
      inputMap'' = M.insert "root" (M {mId = "root", payload = Right [x, "==", z]}) inputMap'
  print $ eval "root" inputMap'
  print $ revEval "humn" inputMap inputMap'' 
