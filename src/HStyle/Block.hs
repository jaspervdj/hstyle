-- | A block of code
{-# LANGUAGE OverloadedStrings #-}
module HStyle.Block
    ( Block
    , fromText
    , prettyBlock
    , toLines
    , subBlock
    , perLine
    , absoluteLineNumber
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T

data Block = Block
    { blockOffset :: Int
    , blockLines  :: Vector Text
    } deriving (Show)

fromText :: Text -> Block
fromText text = Block
    { blockOffset = 0
    , blockLines  = V.fromList $ T.lines text
    }

prettyBlock :: Block -> Text
prettyBlock block = T.unlines $ map pretty $
    zip [offset + 1 ..] $ V.toList lines'
  where
    offset = blockOffset block
    lines' = blockLines block
    width  = length $ show (offset + V.length lines')

    pretty (ln, t) =
        let ln' = T.pack (show ln)
            lnl = T.length ln'
        in T.replicate (width - lnl) " " `T.append`
            ln' `T.append` " " `T.append` t

toLines :: Block -> [Text]
toLines = V.toList . blockLines

-- | Subblock from start to end -- including both.
subBlock :: Int -> Int -> Block -> Block
subBlock start end block = Block
    { blockOffset = blockOffset block + start - 1
    , blockLines  = V.slice (start - 1) (end - start + 1) $ blockLines block
    }

-- | Create a new block for every line.
perLine :: Block -> [Block]
perLine (Block offset lines')  = map line $
    zip [offset + 0 ..] $ V.toList lines'
  where
    line (i, t) = Block i $ V.singleton t

-- | Convert relative line number (within this block, 1-based) to an absolute
-- line number
absoluteLineNumber :: Int -> Block -> Int
absoluteLineNumber i = (+ i) . blockOffset
