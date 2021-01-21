module Util where

import Text.Regex.TDFA

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead list = case list of 
    [] -> Nothing
    (x: xs) -> Just x

firstRegexGroup :: String -> String -> Maybe String
firstRegexGroup regex text = let (_, _, _, groups) = text =~ regex :: (String, String, String, [String]) in
                             safeHead groups

findAllRegexGroups :: String -> String -> [String]
findAllRegexGroups regex text = let matches = getAllTextMatches (text =~ regex) :: [String] in
                                fmap (Maybe.fromJust . firstRegexGroup regex) matches

firstJust :: [Maybe a] -> Maybe a
firstJust = Monad.join . List.find Maybe.isJust 

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap function = Monad.join . fmap function

mapJust :: (a -> Maybe b) -> [a] -> [b] 
mapJust mapper list = fmap Maybe.fromJust $ filter Maybe.isJust $ fmap mapper list

withIndex :: [a] -> [(Integer, a)]
withIndex = zip [0..]