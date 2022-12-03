module MyUtils
( readInt
, resolver
, trim
) where

import Data.Char

readInt :: String -> Int
readInt = read

resolver :: Maybe a -> a 
resolver (Just x) = x
resolver Nothing = error "You've committed a grave error. Stop trying."

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace



