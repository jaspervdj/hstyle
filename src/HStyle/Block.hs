-- | A block of code
{-# LANGUAGE OverloadedStrings #-}
module HStyle.Block
    ( Block
    , Range
    , Position
    , fromText
    , toText
    , prettyRange
    , toLines
    , numLines
    , getRange
    , updateRange
    , getCharacter
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T

newtype Block = Block
    { unBlock :: Vector Text
    } deriving (Eq, Show)

-- | Used to select a number of lines. (start, end), both included.
type Range = (Int, Int)

-- | A position in the file. (row, column).
type Position = (Int, Int)

fromText :: Text -> Block
fromText = Block .  V.fromList . T.lines

toText :: Block -> Text
toText = T.unlines . toLines

prettyRange :: Int -> Block -> Range -> Text
prettyRange indent block range@(start, end) = T.unlines $
    map ((T.replicate indent " " `T.append`) . pretty) $
    zip [start ..] $ getRange block range
  where
    width  = length $ show end

    pretty (ln, t) =
        let ln' = T.pack (show ln)
            lnl = T.length ln'
        in T.replicate (width - lnl) " " `T.append`
            ln' `T.append` " " `T.append` t

toLines :: Block -> [Text]
toLines = V.toList . unBlock

numLines :: Block -> Int
numLines = V.length . unBlock

getRange :: Block -> Range -> [Text]
getRange (Block lines') (start, end) = V.toList $
    V.slice start' (end' - start') lines'
  where
    -- Bounds checking
    start' = start - 1
    end'   = min (V.length lines') end

-- | Update a subblock
updateRange :: Range   -- ^ Range to replace
            -> [Text]  -- ^ New content
            -> Block   -- ^ Block to update
            -> Block   -- ^ Resulting block
updateRange (start, end) new (Block lines') = Block $
    V.take (start - 1) lines' V.++
        V.fromList new V.++
        V.drop end lines'

-- | Get a character at a certain position
getCharacter :: Position -> Block -> Maybe Char
getCharacter (row, col) (Block lines')
    | row < 1               = Nothing
    | row > V.length lines' = Nothing
    | col < 1               = Nothing
    | col > T.length line   = Nothing
    | otherwise             = Just $ line `T.index` (col - 1)
  where
    line = lines' V.! (row - 1)
