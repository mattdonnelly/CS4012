module Part01 (wordFrequency) where

import Control.Arrow (&&&)
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)

mapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
mapReduce mapFunction reduceFunction = reduceFunction . (map mapFunction)

mapper :: Char -> Char
mapper c = if isAlpha c then toLower c else ' '

reducer :: String -> [(Int, String)]
reducer =  sort . map (length &&& head) . group . sort . words

wordFrequency :: String -> [(Int, String)]
wordFrequency = mapReduce mapper reducer
