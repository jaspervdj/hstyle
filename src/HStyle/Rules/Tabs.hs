-- | Check for tabs in files
{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.Tabs
    ( tabsRule
    , tabsChecker
    , tabsFixer
    ) where

import qualified Data.Text as T

import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

tabsRule :: Int -> Rule
tabsRule i = Rule selectLines tabsChecker (tabsFixer i)

tabsChecker :: Checker ()
tabsChecker = checkLines $ \() line -> case T.findIndex (== '\t') line of
    Nothing -> Nothing
    Just i  -> Just $ "\\t at column " `T.append` T.pack (show $ i + 1)

tabsFixer :: Int -> Fixer ()
tabsFixer numSpaces = fixLines fixer
  where
    spaces   = T.replicate numSpaces " "
    fixer () = T.intercalate spaces . T.split (== '\t')
