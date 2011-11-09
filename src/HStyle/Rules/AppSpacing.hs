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
    = Infix String Position Position Position
    -- | Application of the form @f x@
    | Prefix String Position Position
    deriving (Show)

appSpacingRule :: Rule
appSpacingRule = Rule appSelector appSpacingChecker fixNothing

appSelector :: Selector AppInfo
appSelector (md, _) _ =
    [ (Infix (H.prettyPrint o) (ts e1) (ts o) (ts e2), rangeFromScrSpanInfo l)
    | H.InfixApp l e1 o e2 <- exps md
    ] ++
    [ (Prefix (H.prettyPrint f) (ts f) (ts x), rangeFromScrSpanInfo l)
    | H.App l f x <- exps md
    ]
  where
    ts :: H.Annotated f => f H.SrcSpanInfo -> Position
    ts = positionFromScrSpanInfo . H.ann

appSpacingChecker :: Checker AppInfo
appSpacingChecker app block _ = case app of
    (Infix p _ so se2)
        | spaceBefore so && spaceBefore se2 -> []
        | otherwise                         ->
            [(1, "Need spacing around " `T.append` T.pack p)]
    (Prefix p _ x)
        | spaceBefore x -> []
        | otherwise     ->
            [(1, "Need spacing before " `T.append` T.pack p)]
  where
    spaceBefore (line, col) = case getCharacter (line, col - 1) block of
        Nothing -> True
        Just c  -> isSpace c
