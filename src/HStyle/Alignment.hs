{-# LANGUAGE OverloadedStrings #-}
module HStyle.Alignment where

import Data.List (sort)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Map as M

import HStyle.Block

-- | In many cases, we want to search for a specific form of
-- "backward-alignment".
--
-- > case x of
-- >     X               -> undefined
-- >     Y y | y < 0     -> undefined
-- >         | otherwise -> undefined
--
-- We can easily figure out the positions of the 'undefined' nodes. However, to
-- get the actual alignment, we need to backtrace and get the alignment of the
-- '->' strings. This is what this function is for.
backwardAlignment :: Range       -- ^ Relevant range
                  -> [Position]  -- ^ Position of the nodes
                  -> [Text]      -- ^ Strings to search for
                  -> Block       -- ^ Source code
                  -> [Position]  -- ^ Found alignment
backwardAlignment range positions strings block =
    catMaybes $ map backwardAlignment' positions
  where
    backwardAlignment' position =
        let cand = [findBackwards range position str block | str <- strings]
        in case catMaybes cand of
            [] -> Nothing
            xs -> Just $ maximum xs

checkAlignmentHead :: [Position]
                   -> Bool
checkAlignmentHead positions = equal $ catMaybes $
    map (listToMaybe . sort . snd) $ M.toAscList $
        foldr (\(l, c) m -> M.insertWith (++) l [c] m) M.empty positions

equal :: Eq a
      => [a]
      -> Bool
equal (x : y : r)
    | x == y    = equal (y : r)
    | otherwise  = False
equal _         = True
