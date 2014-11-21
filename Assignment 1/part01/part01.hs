import Control.Arrow ((&&&))
import Data.Char (isAlpha, toLower)
import Data.List (sort, group)
import Data.Map (singleton, empty, toList, unionWith)
import System.Environment (getArgs)

type FrequencyTable = [(String, Int)]

mapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
mapReduce mapFunction reduceFunction = reduceFunction . (map mapFunction)

mapper :: String -> FrequencyTable
mapper = map (head &&& length) . group . sort . words . strip
    where strip = map (\c -> if isAlpha c then toLower c else ' ')

reducer :: [FrequencyTable] -> FrequencyTable
reducer = toList . foldl (unionWith (+)) empty . map convertPairToMap . concat
    where convertPairToMap = uncurry singleton

wordFrequency :: [String] -> FrequencyTable
wordFrequency = mapReduce mapper reducer

main :: IO ()
main = do
    args <- getArgs
    state <- readFile (head args)
    let frequencies = wordFrequency $ lines state
    print frequencies
