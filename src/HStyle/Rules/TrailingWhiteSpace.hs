{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.TrailingWhiteSpace
    ( trailingWhiteSpaceRule
    , trailingWhiteSpaceChecker
    , trailingWhiteSpaceFixer
    ) where

import Data.Char (isSpace)

import qualified Data.Text as T

import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

trailingWhiteSpaceRule :: Rule
trailingWhiteSpaceRule = Rule
    selectLines trailingWhiteSpaceChecker trailingWhiteSpaceFixer

trailingWhiteSpaceChecker :: Checker ()
trailingWhiteSpaceChecker = checkLines $ \() line ->
    if not (T.null line) && isSpace (T.last line)
        then Just "Trailing whitespace"
        else Nothing

trailingWhiteSpaceFixer :: Fixer ()
trailingWhiteSpaceFixer = fixLines $ const T.stripEnd
