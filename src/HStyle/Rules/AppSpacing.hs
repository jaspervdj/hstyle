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
    -- | Application of the form @x + y@
    = Infix Snippet Snippet Snippet
    -- | Application of the form @f x@
    | Prefix Snippet Snippet
    deriving (Show)

appSpacingRule :: Rule
appSpacingRule = Rule appSelector appSpacingChecker fixNothing

appSelector :: Selector AppInfo
appSelector (md, _) block =
    [ (Infix (ts e1) (ts o) (ts e2), fromSrcSpanInfo l block)
    | H.InfixApp l e1 o e2 <- exps md
    ] ++
    [ (Prefix (ts f) (ts x), fromSrcSpanInfo l block)
    | H.App l f x <- exps md
    ]
  where
    ts :: H.Annotated f => f H.SrcSpanInfo -> Snippet
    ts = flip fromSrcSpanInfoSnippet block . H.ann

appSpacingChecker :: Checker AppInfo
appSpacingChecker app _ = case app of
    (Infix _ so se2)
        | spaceBefore so && spaceBefore se2 -> []
        | otherwise                         ->
            [(1, "Need spacing around " `T.append` snippetText so)]
    (Prefix _ x)
        | spaceBefore x -> []
        | otherwise     ->
            [(1, "Need spacing before " `T.append` snippetText x)]
  where
    spaceBefore (Snippet b _ c)
        | c < 1     = True
        | otherwise = isSpace $ T.index (toText b) (c - 2)
