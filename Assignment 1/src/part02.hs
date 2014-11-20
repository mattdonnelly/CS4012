module Part02 (wordFrequency) where

import Control.Arrow ((&&&))
import Control.Parallel (pseq)
import Control.Parallel.Strategies (Strategy, parMap, using, dot, rpar, rdeepseq, rseq)
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)

mapReduce :: Strategy b -> (a -> b) -> Strategy c -> ([b] -> c) -> [a] -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input = mapResult `pseq` reduceResult
     where
        mapResult = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

mapper :: Char -> Char
mapper c = if isAlpha c then toLower c else ' '

reducer :: String -> [(Int, String)]
reducer =  sort . map (length &&& head) . group . sort . words

wordFrequency :: String -> [(Int, String)]
wordFrequency = mapReduce (rpar `dot` rdeepseq) mapper rseq reducer
