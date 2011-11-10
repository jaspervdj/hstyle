{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.DataAlignment
    ( dataAlignmentRule
    , dataSelector
    , dataAlignmentChecker
    ) where

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

dataAlignmentRule :: Rule
dataAlignmentRule = Rule dataSelector dataAlignmentChecker fixNothing

dataSelector :: Selector [Position]
dataSelector (md, _) _ =
    [ (map (positionFromScrSpanInfo . H.ann) constrs, rangeFromScrSpanInfo loc)
    | H.DataDecl loc _ _ _ constrs _ <- everything md
    ]

dataAlignmentChecker :: Checker [Position]
dataAlignmentChecker positions block range =
    if checkAlignmentHead (backwardAlignment range positions ["=", "|"] block)
        then []
        else [(fst range, "Improper alignment of '=', '|'")]
