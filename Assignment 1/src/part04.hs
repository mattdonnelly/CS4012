module Part04 (wordFrequency) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData)
import Data.Char (isAlpha, toLower)
import Data.Hashable (Hashable, hash)
import Data.List (nub)
import Prelude hiding (return, (>>=))

class Monad' m where
    return :: a -> m s x s a
    (>>=) :: (Eq b, NFData s'', NFData c) => m s a s' b -> ( b -> m s' b s'' c ) -> m s a s'' c

newtype MapReduce s a s' b = MR { runMR :: ([(s, a)] -> [(s', b)]) }

instance Monad' MapReduce where
    return = retMR
    (>>=) = bindMR

retMR :: a -> MapReduce s x s a
retMR k = MR (\ss -> [(s, k) | s <- fst <$> ss])

bindMR :: (Eq b, NFData s'', NFData c) => MapReduce s a s' b -> (b -> MapReduce s' b s'' c) -> MapReduce s a s'' c
bindMR f g = MR (\s ->
    let
        fs = runMR f s
        gs = map g $ nub $ snd <$> fs
    in
        concat $ map (\g' -> runMR g' fs) gs)

runMapReduce :: MapReduce s () s' b -> [s] -> [(s',b)]
runMapReduce m ss = (runMR m) [(s, ()) | s <- ss]

distributeMR :: (Hashable s) => MapReduce s () s Int
distributeMR = MR (\ss -> [(s, hash s) | s <- fst <$> ss])

wrapMR :: (Eq a) => ([s] -> [(s',b)]) -> (a -> MapReduce s a s' b)
wrapMR f = (\k -> MR (g k))
    where g k ss = f $ fst <$> filter (\s -> k == snd s) ss

mapper :: [String] -> [(String , String)]
mapper [] = []
mapper (x:xs) = parse x ++ mapper xs
    where
        parse = (map (\w -> (w,w))) . words . strip
        strip = map (\c -> if isAlpha c then toLower c else ' ')

reducer :: [String] -> [(String , Int)]
reducer [] = []
reducer xs = [(head xs,length xs)]

wordFrequency :: [String] -> [(String , Int)]
wordFrequency state = runMapReduce mr state
    where mr = distributeMR >>= wrapMR mapper >>= wrapMR reducer
