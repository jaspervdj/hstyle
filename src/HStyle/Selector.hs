module HStyle.Selector
    ( Selector
    , selectAll
    , selectLines
    , fromSrcSpanInfo
    ) where

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block

-- | Selects a portion from a haskell module
type Selector a =
    (H.Module H.SrcSpanInfo, [H.Comment]) -> Block -> [(a, Block)]

selectAll :: Selector ()
selectAll _ block = [((), block)]

selectLines :: Selector ()
selectLines _ block = [((), b) | b <- perLine block]

fromSrcSpanInfo :: H.SrcSpanInfo -> Block -> Block
fromSrcSpanInfo ssi = subBlock start end
  where
    span' = H.srcInfoSpan ssi
    start = H.srcSpanStartLine span'
    end   = H.srcSpanEndLine span'
