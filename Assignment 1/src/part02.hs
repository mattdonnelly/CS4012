module Part02 (wordFrequency) where

import Control.Arrow ((&&&))
import Control.Parallel (pseq)
import Control.Parallel.Strategies (Strategy, parMap, using, dot, rpar, rdeepseq, rseq)
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)

type FrequencyTable = [(Int, String)]

mapReduce :: Strategy b -> (a -> b) -> Strategy c -> ([b] -> c) -> [a] -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input = mapResult `pseq` reduceResult
     where
        mapResult = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

mapper :: String -> [String]
mapper = words . strip
    where strip = map (\s -> if isAlpha s then toLower s else ' ')

reducer :: [[String]] -> FrequencyTable
reducer = sort . map (length &&& head) . group . sort . concat

wordFrequency :: [String] -> FrequencyTable
wordFrequency = mapReduce (rpar `dot` rdeepseq) mapper rseq reducer
