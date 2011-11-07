-- | A block of code
{-# LANGUAGE OverloadedStrings #-}
module HStyle.Block
    ( Block
    , fromText
    , toText
    , prettyBlock
    , toLines
    , subBlock
    , updateSubBlock
    , perLine
    , absoluteLineNumber
    , mapLines
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T

data Block = Block
    { blockOffset :: Int
    , blockLines  :: Vector Text
    } deriving (Eq, Show)

fromText :: Text -> Block
fromText text = Block
    { blockOffset = 0
    , blockLines  = V.fromList $ T.lines text
    }

toText :: Block -> Text
toText = T.unlines . toLines

prettyBlock :: Int -> Block -> Text
prettyBlock indent block = T.unlines $
    map ((T.replicate indent " " `T.append`) . pretty) $
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
    { blockOffset = blockOffset block + start'
    , blockLines  = V.slice start' (end' - start') lines'
    }
  where
    -- Bounds checking
    lines' = blockLines block
    start' = start - 1
    end'   = min (V.length lines') end

-- | Update a subblock
updateSubBlock :: Block  -- ^ Old
               -> Block  -- ^ New
               -> Block  -- ^ Block to update
               -> Block  -- ^ Resulting block
updateSubBlock old new block
    | blockOffset old /= blockOffset new =
        error "HStyle.Block.updateSubBlock: Internal error"
    | otherwise                          = block
        { blockLines = V.take subOffset lines' V.++ blockLines new V.++
            V.drop (subOffset + V.length oldLines) lines'
        }
  where
    subOffset
        | blockOffset old == blockOffset new = blockOffset old
        | otherwise                          = error
            "HStyle.Block.updateSubBlock: Internal error"
    oldLines = blockLines old
    lines'   = blockLines block

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

-- | Map over the lines in a block
mapLines :: (Text -> Text) -> Block -> Block
mapLines f block = block {blockLines = V.map f (blockLines block)}
