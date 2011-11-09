module HStyle.Checker
    ( Checker
    , checkLines
    ) where

import Data.Text (Text)

import HStyle.Block

-- | Takes a number of lines, and notifies of problems on each line.
type Checker a = a -> Block -> Range -> [(Int, Text)]

-- | Check every line of the block, possibly returning a problem description
checkLines :: (a -> Text -> Maybe Text) -> Checker a
checkLines checker x block range = do
    (ln, text) <- zip [fst range ..] $ getRange block range
    case checker x text of
        Nothing -> []
        Just p  -> [(ln , p)]
