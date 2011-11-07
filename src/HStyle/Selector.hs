module HStyle.Selector
    ( Selector
    , Snippet (..)
    , selectAll
    , selectLines
    , fromSrcSpanInfo
    , fromSrcSpanInfoSnippet
    , everything
    ) where

import Data.Maybe (maybeToList)

import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (cast)
import qualified Data.Generics as G
import qualified Data.Text as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block

-- | Selects a portion from a haskell module
type Selector a =
    (H.Module H.SrcSpanInfo, [H.Comment]) -> Block -> [(a, Block)]

data Snippet = Snippet
    { snippetBlock :: Block
    , snippetText  :: Text
    , snippetCol   :: Int
    } deriving (Show)

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

fromSrcSpanInfoSnippet :: H.SrcSpanInfo -> Block -> Snippet
fromSrcSpanInfoSnippet ssi block = Snippet
    { snippetBlock = block'
    , snippetText  = text
    , snippetCol   = startCol
    }
  where
    (H.SrcSpan _ start startCol end endCol) = H.srcInfoSpan ssi
    block' = subBlock start end block
    lines' = toLines block'
    text
        | start == end = T.drop (startCol - 1) $
            T.take (endCol - 1) $ head lines'
        | otherwise    = T.unlines $
            [T.drop (startCol - 1) (head lines')] ++
            init (drop 1 lines') ++
            [T.take endCol (last lines')]

everything :: Data d => H.Module H.SrcSpanInfo -> [d]
everything = G.everything (++) (maybeToList . cast)
