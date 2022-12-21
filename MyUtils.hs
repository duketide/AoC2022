module MyUtils
( readInt
, readDouble
, trim
) where

import Data.Char (isSpace)

readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble = read

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace




