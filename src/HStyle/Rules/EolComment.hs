{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.EolComment
    ( eolCommentRule
    ) where

import Data.Char (isSpace)
import Data.Maybe (maybeToList)

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

eolCommentRule :: Rule
eolCommentRule = Rule eolCommentSelector eolCommentChecker fixNothing

eolCommentSelector :: Selector Position
eolCommentSelector (_, comments) _ = do
    H.Comment False ss _ <- comments
    let ssi = H.noInfoSpan ss
    return (positionFromScrSpanInfo ssi, rangeFromScrSpanInfo ssi)

eolCommentChecker :: Checker Position
eolCommentChecker (line, column) block _ = maybeToList $ do
    c1 <- getCharacter (line, column - 2) block
    c2 <- getCharacter (line, column - 1) block
    if isSpace c1 && isSpace c2
        then Nothing
        else Just (line, "Need two spaces between code and comment")
