module Part01 (wordFrequency) where

import Data.Char (isAlpha, toLower)
import Data.List (sort, group)

mapReduce :: ([a] -> [b]) -> ([b] -> c) -> [a] -> c
mapReduce mapFunction reduceFunction = reduceFunction . mapFunction

mapper :: String -> [String]
mapper = sort . words . simplify
    where simplify =  map (\c -> if isAlpha c then toLower c else ' ')

reducer :: [String] -> [(Int, String)]
reducer =  sort . map (\s -> (length s, head s)) . group

wordFrequency :: String -> [(Int, String)]
wordFrequency = mapReduce mapper reducer
