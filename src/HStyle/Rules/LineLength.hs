{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.LineLength
    ( lineLengthRule
    , lineLengthChecker
    ) where

import qualified Data.Text as T

import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

lineLengthRule :: Int -> Rule
lineLengthRule max' = (selectLines, lineLengthChecker max', fixNothing)

lineLengthChecker :: Int -> Checker
lineLengthChecker max' = checkLines $ \line -> if T.length line > max'
    then Just $ "Exceeds max line length of " `T.append` T.pack (show max')
    else Nothing
