{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.EolComment
    ( eolCommentRule
    ) where

import Control.Monad (guard)
import Data.Char (isSpace)

import Debug.Trace

import qualified Data.Text as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

eolCommentRule :: Rule
eolCommentRule = Rule eolCommentSelector eolCommentChecker fixNothing

eolCommentSelector :: Selector Int
eolCommentSelector (_, comments) block = do
    H.Comment False ss _ <- comments
    let start  = H.srcSpanStartLine ss
        col    = H.srcSpanStartColumn ss
        end    = H.srcSpanEndLine ss
        block' = subBlock start end block
    guard $ start == end && col > 2
    -- Remember the start column of the comment
    return (H.srcSpanStartColumn ss, block')

eolCommentChecker :: Checker Int
eolCommentChecker = checkLines $ \c t ->
    if c > 2 && isSpace (T.index t (c - 3)) && isSpace (T.index t (c - 2))
        then Nothing
        else Just "Need two spaces between code and comment"
