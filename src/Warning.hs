module Warning where

import qualified Latex 

data Warning = Warning {
    message :: String,
    position :: Latex.Position
} deriving (Show, Eq)


-- find labels that are not referenced by a label
findUnreferencedLabels :: [Latex.Line] -> [Warning]
findUnreferencedLabels lines = error "TODO"
