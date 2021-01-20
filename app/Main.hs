module Main where

import System.Environment

import Latex

main :: IO ()
main = do 
    args <- getArgs
    print args
    lines <- readLatexExpanding $ head args
    print lines
