module Main where

import HStyle.Main
import HStyle.Rules.AppSpacing
import HStyle.Rules.CaseAlignment
import HStyle.Rules.DataAlignment
import HStyle.Rules.LineLength
import HStyle.Rules.PatMatchAlignment
import HStyle.Rules.Tabs
import HStyle.Rules.TrailingWhiteSpace
import HStyle.Rules.TypeSigAlignment

main :: IO ()
main = mainWith
    [ appSpacingRule
    , caseAlignmentRule
    , dataAlignmentRule
    , lineLengthRule 80
    , patMatchAlignmentRule
    , tabsRule 4
    , trailingWhiteSpaceRule
    , typeSigAlignmentRule
    ]
