module Part02 (wordFrequency) where

import Control.Arrow ((&&&))
import Control.Parallel (pseq)
import Control.Parallel.Strategies (Strategy, parMap, using, dot, rpar, rdeepseq, rseq)
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)
import Data.Map as Map (Map, insertWith, empty, toList)

type FrequencyTable = [(String, Int)]

mapReduce :: Strategy b -> (a -> b) -> Strategy c -> ([b] -> c) -> [a] -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input = mapResult `pseq` reduceResult
     where
        mapResult = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

mapper :: String -> FrequencyTable
mapper = map (head &&& length) . group . sort . words . strip
    where strip = map (\c -> if isAlpha c then toLower c else ' ')

reducer :: [FrequencyTable] -> FrequencyTable
reducer = toList . foldl insertOrAdd accum . concat
    where
        accum = empty :: Map String Int
        insertOrAdd m (k, v) = insertWith (+) k v m

wordFrequency :: [String] -> FrequencyTable
wordFrequency = mapReduce (rpar `dot` rdeepseq) mapper rseq reducer
