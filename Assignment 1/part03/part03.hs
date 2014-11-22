import Control.Arrow ((&&&))
import Control.Parallel (pseq, par)
import Control.Parallel.Strategies (Strategy, parMap, using, dot, rpar, rdeepseq, rseq)
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)
import Data.Map (singleton, toList, unionWith, empty)
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
reducer = toList . fold (unionWith (+)) empty . map convertPairToMap . concat
    where convertPairToMap = uncurry singleton

{-- Parallel version of reducer which turned out to be slower

    reducer :: [FrequencyTable] -> FrequencyTable
    reducer = toList . pfold (unionWith (+)) . map convertPairToMap . concat
        where convertPairToMap = uncurry singleton

    pfold :: (a -> a -> a) -> [a] -> a
    pfold _ [x] = x
    pfold mappend xs  = (ys `par` zs) `pseq` (ys `mappend` zs) where
        len = length xs
        (ys', zs') = splitAt (len `div` 2) xs
        ys = pfold mappend ys'
        zs = pfold mappend zs'
--}

wordFrequency :: [String] -> FrequencyTable
wordFrequency = mapReduce (rpar `dot` rdeepseq) mapper rseq reducer

main :: IO ()
main = do
    args <- getArgs
    state <- readFile (head args)
    let frequencies = wordFrequency $ lines state
    print frequencies
