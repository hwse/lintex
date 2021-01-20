module Latex where

import qualified Control.Monad as Monad
import qualified Data.List as List

import Util

data Position = Position {
    file :: FilePath,
    lineNr :: Integer
} deriving (Show, Eq)

data Line = Line Position String 
    deriving (Show, Eq) 

expandRegexes :: [String]
expandRegexes = [".*\\input{(.*)}.*", ".*\\include{(.*)}.*"]

-- The file to which this line must be expended if expansion is required
expansionFile :: Line -> Maybe FilePath
expansionFile (Line nr lineString) = 
    firstJust $ fmap (\r -> firstRegexGroup r lineString) expandRegexes
    
-- Expand the line if there is  a \input or \include tag
expandLine :: Line -> IO [Line]
expandLine line = case expansionFile line of
    Just(fileName) -> readLatexExpanding fileName
    Nothing -> return [line] 


readLines :: FilePath -> IO [Line]
readLines filePath = do
    content <- readFile filePath
    return $ fmap buildLine $ zip [0..] $ lines content
    where 
        buildLine :: (Integer, String) -> Line
        buildLine (nr, line) = Line (Position filePath nr) line

-- Read a file and expand every input with the file
readLatexExpanding :: FilePath -> IO [Line]
readLatexExpanding filePath = do
    contentLines <- readLines filePath
    expandedLines <- sequence $ fmap expandLine contentLines
    return $ Monad.join expandedLines