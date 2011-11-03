module HStyle.Selector
    ( Selector
    , selectAll
    , selectLines
    , fromSrcSpanInfo
    , everything
    ) where

import Data.Maybe (maybeToList)

import Data.Data (Data)
import Data.Typeable (cast)
import qualified Data.Generics as G
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

everything :: Data d => H.Module H.SrcSpanInfo -> [d]
everything = G.everything (++) (maybeToList . cast)
