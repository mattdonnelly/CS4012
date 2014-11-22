import Control.Applicative ((<$>))
import Control.DeepSeq (NFData)
import Data.Char (isAlpha, toLower)
import Data.Hashable (Hashable, hash)
import Data.List (nub)
import System.Environment (getArgs)

import Control.Monad (forever, forM_, replicateM)
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

class Monad' m where
    return :: a -> m s x s a
    (>>=) :: (Eq b, NFData s'', NFData c) => m s a s' b -> ( b -> m s' b s'' c ) -> m s a s'' c

newtype MapReduce s a s' b = MR { runMR :: ([(s, a)] -> [(s', b)]) }

instance Monad' MapReduce where
    return = retMR
    (>>=) = bindMR

retMR :: a -> MapReduce s x s a
retMR k = MR (\ss -> [(s, k) | s <- fst <$> ss])

concurrentMap :: (a -> b) -> [a] -> [b]
concurrentMap _ [] = []
concurrentMap f xs = unsafePerformIO $ do
    inChan <- newChan
    outChan <- newChan
    forM_ [1..nrWorkers] (\_ -> forkIO $ worker f inChan outChan)
    forM_ xs (writeChan inChan)
    replicateM (length xs) (do
        x <- readChan outChan
        Prelude.return x)

worker :: (a -> b) -> Chan a -> Chan b -> IO ()
worker f inChan outChan =
    forever $ do
        x <- readChan inChan
        let mapped = f x
        writeChan outChan mapped

bindMR :: (Eq b, NFData s'', NFData c) => MapReduce s a s' b -> (b -> MapReduce s' b s'' c) -> MapReduce s a s'' c
bindMR f g = MR (\s ->
    let
        fs = (runMR f) s
        gs = map g $ nub $ snd <$> fs
    in
        concat $ concurrentMap (\g' -> runMR g' fs) gs)

runMapReduce :: MapReduce s () s' b -> [s] -> [(s',b)]
runMapReduce m ss = (runMR m) [(s, ()) | s <- ss]

distributeMR :: (Hashable s) => MapReduce s () s Int
distributeMR = MR (\ss -> [(s, hash s) | s <- fst <$> ss])

wrapMR :: (Eq a) => ([s] -> [(s',b)]) -> (a -> MapReduce s a s' b)
wrapMR f = (\k -> MR (g k))
    where g k ss = f $ fst <$> filter (\s -> k == snd s) ss

nrWorkers :: Int
nrWorkers = 8

mapper :: [String] -> [(String , String)]
mapper xs = concat $ concurrentMap parse xs
    where
        parse = (map (\w -> (w,w))) . words . strip
        strip = map (\c -> if isAlpha c then toLower c else ' ')

reducer :: [String] -> [(String , Int)]
reducer [] = []
reducer xs = [(head xs,length xs)]

wordFrequency :: [String] -> [(String , Int)]
wordFrequency state = runMapReduce mr state
    where mr = distributeMR Main.>>= wrapMR mapper Main.>>= wrapMR reducer

main :: IO ()
main = do
    args <- getArgs
    state <- readFile (head args)
    let frequencies = wordFrequency $ lines state
    print frequencies

