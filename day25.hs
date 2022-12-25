import MyUtils (readInt)
import Data.Map (Map)
import qualified Data.Map as M

snafuAdder :: Map Char Int -> [String] -> Int
snafuAdder mp fl = sum $ map (snd . mapper) fl
  where
    mapper = foldr (\char (p, tot) -> (p + 1, tot + (mp M.! char) * (5^p))) (0, 0)

decToFive :: Double -> String 
decToFive int = go int mag []
  where
    mag = floor $ logBase 5 int 
    go :: Double -> Int -> String -> String 
    go _ (-1) str = reverse str
    go n m    str = go (n - num) (m - 1) (head (show res) : str)
      where
        res = floor (n / (5^m)) :: Int 
        num = fromIntegral $ res * (5^m)

fiveToSnafu :: String -> String
fiveToSnafu s = snd $ foldr folder (0, []) s
  where
   folder :: Char -> (Int, String) -> (Int, String)
   folder c (carry, str) = case readInt [c] + carry of
     0 -> (0, '0' : str)
     1 -> (0, '1' : str)
     2 -> (0, '2' : str)
     3 -> (1, '=' : str)
     4 -> (1, '-' : str)
     5 -> (1, '0' : str)
   
main = do
  rawInput <- readFile "day25.txt"
  let input = lines rawInput
      myMap = M.fromList [('2', 2), ('1', 1), ('0', 0), ('-', -1), ('=', -2)]
      inDec = snafuAdder myMap input
      inFives = decToFive $ fromIntegral inDec
      inSnafu = fiveToSnafu inFives
  print inSnafu
