module HStyle.Fixer
    ( Fixer
    , fixNothing
    , fixLines
    ) where

import Data.Text (Text)

import HStyle.Block

-- | Takes a block and fixes it, if possible
type Fixer a = a -> Block -> Maybe Block

fixNothing :: Fixer a
fixNothing = const $ const Nothing

fixLines :: (a -> Text -> Text) -> Fixer a
fixLines f x = Just . mapLines (f x)
