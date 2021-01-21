module Main where

import System.Environment
import Warning
import Util
import Latex

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ "Processing File: " ++ (head args)
    lines <- readLatexExpanding $ head args
    let datas = (flatMap processLine lines) :: [UnrefLabelData]
    let warnings = aggregateLineData datas
    mapM_ printWarning warnings
