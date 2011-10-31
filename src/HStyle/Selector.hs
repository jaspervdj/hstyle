module HStyle.Selector
    ( Selector
    , selectAll
    , selectLines
    , fromSrcSpanInfo
    ) where

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block

-- | Selects a portion from a haskell module
type Selector = H.Module H.SrcSpanInfo -> Block -> [Block]

selectAll :: Selector
selectAll _ = return

selectLines :: Selector
selectLines _ = perLine

fromSrcSpanInfo :: H.SrcSpanInfo -> Block -> Block
fromSrcSpanInfo ssi = subBlock start end
  where
    span' = H.srcInfoSpan ssi
    start = H.srcSpanStartLine span'
    end   = H.srcSpanEndLine span'
