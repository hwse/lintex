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

firstJust :: [Maybe a] -> Maybe a
firstJust = Monad.join . List.find Maybe.isJust 

withIndex :: [a] -> [(Integer, a)]
withIndex = zip [0..]