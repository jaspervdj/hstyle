module HStyle.Fixer
    ( Fixer
    , fixNothing
    , fixLines
    ) where

import Data.Text (Text)

import HStyle.Block

-- | Takes a block and fixes it, if possible
type Fixer a = a -> Block -> Range -> Maybe [Text]

fixNothing :: Fixer a
fixNothing _ _ _ = Nothing

fixLines :: (a -> Text -> Text) -> Fixer a
fixLines f x block = Just . map (f x) . getRange block
