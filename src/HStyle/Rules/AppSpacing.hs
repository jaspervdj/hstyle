-- | Checks spacing around function applications
{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.AppSpacing
    ( appSpacingRule
    , appSelector
    , appSpacingChecker
    ) where

import Data.Char (isSpace)

import qualified Data.Text as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

exps :: H.Module H.SrcSpanInfo -> [H.Exp H.SrcSpanInfo]
exps = everything

data AppInfo
    = Infix Snippet Snippet Snippet
    deriving (Show)

appSpacingRule :: Rule
appSpacingRule = Rule appSelector appSpacingChecker fixNothing

appSelector :: Selector AppInfo
appSelector (md, _) block =
    [ (Infix (ts e1) (ts o) (ts e2), fromSrcSpanInfo (H.ann e) block)
    | e@(H.InfixApp _ e1 o e2) <- exps md
    ]
  where
    ts :: H.Annotated f => f H.SrcSpanInfo -> Snippet
    ts = flip fromSrcSpanInfoSnippet block . H.ann

appSpacingChecker :: Checker AppInfo
appSpacingChecker (Infix _ so se2) _
    | spaceBefore so && spaceBefore se2 = []
    | otherwise                         =
        [(1, "Need spacing around " `T.append` snippetText so)]
  where
    spaceBefore (Snippet b _ c)
        | c < 1     = True
        | otherwise = isSpace $ T.index (toText b) (c - 2)
