module Main where

import qualified Part01
import qualified Part02

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let frequencies1 = Part01.wordFrequency contents
    --let frequencies2 = Part02.wordFrequency contents
    print frequencies1