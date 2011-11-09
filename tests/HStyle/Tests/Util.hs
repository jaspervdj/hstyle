module HStyle.Tests.Util
    ( testRule
    ) where

import Data.Text (Text)

import HStyle
import HStyle.Rule

testRule :: Text -> Rule -> [(Int, Text)]
testRule text (Rule selector checker _) = do
    let Right (md, cm, block) = parseModule Nothing text
    (x, selection) <- selector (md, cm) block
    problem        <- checker x selection
    return problem
