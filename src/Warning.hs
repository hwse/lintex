module Warning where
import Text.Regex.TDFA

import qualified Latex 
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Util

data Warning = Warning {
    message :: String,
    position :: Latex.Position
} deriving (Show, Eq)

printWarning :: Warning -> IO ()
printWarning w = putStrLn $ (Latex.pFile $ position w) ++ ":" ++ 
                            (show $ Latex.pLineNr $ position w) ++ ":" ++ 
                            (message w) 


class Lint lineData where 
    processLine :: Latex.Line -> [lineData]
    aggregateLineData :: [lineData] -> [Warning]

data UnrefLabelData = Label Latex.Position String 
                    | Reference String
                    deriving (Show, Eq)

instance Lint UnrefLabelData where
    processLine line = foundLabels ++ foundRefs
        where 
            (Latex.Line position text) = line
            foundLabels = fmap (Label position) $ findAllRegexGroups "\\\\label{([a-zA-Z0-9_:]*)}" text
            foundRefs = fmap Reference $ findAllRegexGroups "\\\\ref{([a-zA-Z0-9_:]*)}" text

    aggregateLineData lineDatas = mapJust getWarning labels 
        where
            getWarning :: (Latex.Position, String) -> Maybe Warning
            getWarning (pos, l) = case List.find (==l) refs of 
                Just _ -> Nothing
                Nothing -> Just $ Warning ("Label never referenced: " ++ l) pos

            labels = mapJust unLabel lineDatas
            refs = mapJust unRef lineDatas 
            
            unRef :: UnrefLabelData -> Maybe String
            unRef (Reference t) = Just t
            unRef _ = Nothing

            unLabel :: UnrefLabelData -> Maybe (Latex.Position, String)
            unLabel (Label pos text) = Just (pos, text)
            unLabel (Reference _) = Nothing



