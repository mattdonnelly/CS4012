import Control.Arrow ((&&&))
import Control.Parallel (pseq)
import Control.Parallel.Strategies (Strategy, parMap, using, dot, rpar, rdeepseq, rseq)
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)
import Data.Map (singleton, empty, toList, unionWith)
import System.Environment (getArgs)

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
reducer = toList . foldl (unionWith (+)) empty . map convertPairToMap . concat
    where convertPairToMap = uncurry singleton

wordFrequency :: [String] -> FrequencyTable
wordFrequency = mapReduce (rpar `dot` rdeepseq) mapper rseq reducer

main :: IO ()
main = do
    args <- getArgs
    state <- readFile (head args)
    let frequencies = wordFrequency $ lines state
    print frequencies
