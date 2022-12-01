module MyUtils
( readInt
, trim
) where

import Data.Char

readInt :: String -> Int
readInt = read

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace



