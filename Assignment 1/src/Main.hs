module Main where

import qualified Part01
import qualified Part02
import qualified Part04

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    state <- readFile (head args)
    let frequencies = Part02.wordFrequency $ lines state
    print frequencies


