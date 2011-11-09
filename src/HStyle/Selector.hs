module HStyle.Selector
    ( Selector
    , selectAll
    , selectLines
    , rangeFromScrSpanInfo
    , positionFromScrSpanInfo
    , everything
    ) where

import Control.Arrow ((>>>), (&&&))
import Data.Maybe (maybeToList)

import Data.Data (Data)
import Data.Typeable (cast)
import qualified Data.Generics as G
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block

-- | Selects a portion from a haskell module
type Selector a =
    (H.Module H.SrcSpanInfo, [H.Comment]) -> Block -> [(a, Range)]

selectAll :: Selector ()
selectAll _ block = [((), (1, numLines block))]

selectLines :: Selector ()
selectLines _ block = [((), (l, l)) | l <- [1 .. numLines block]]

rangeFromScrSpanInfo :: H.SrcSpanInfo -> Range
rangeFromScrSpanInfo = H.srcInfoSpan >>> H.srcSpanStartLine &&& H.srcSpanEndLine

positionFromScrSpanInfo :: H.SrcSpanInfo -> Position
positionFromScrSpanInfo =
    H.srcInfoSpan >>> H.srcSpanStartLine &&& H.srcSpanStartColumn

everything :: Data d => H.Module H.SrcSpanInfo -> [d]
everything = G.everything (++) (maybeToList . cast)
