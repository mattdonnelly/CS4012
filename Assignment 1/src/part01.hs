module Part01 (wordFrequency) where

import Control.Arrow ((&&&))
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)

type FrequencyTable = [(Int, String)]

mapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
mapReduce mapFunction reduceFunction = reduceFunction . (map mapFunction)

mapper :: String -> [String]
mapper = words . strip
    where strip = map (\s -> if isAlpha s then toLower s else ' ')

reducer :: [[String]] -> FrequencyTable
reducer = map (length &&& head) . group . sort . concat

wordFrequency :: [String] -> FrequencyTable
wordFrequency = mapReduce mapper reducer
