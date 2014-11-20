module Main where

import System.Environment (getArgs)
import qualified Part01

main = do
    args <- getArgs
    contents <- readFile $ head args
    let frequencies1 = Part01.wordFrequency contents
    print frequencies1