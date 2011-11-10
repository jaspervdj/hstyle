{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.CaseAlignment
    ( caseAlignmentRule
    , caseSelector
    , caseAlignmentChecker
    ) where

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

caseAlignmentRule :: Rule
caseAlignmentRule = Rule caseSelector caseAlignmentChecker fixNothing

caseSelector :: Selector [Position]
caseSelector (md, _) _ = do
    -- Select a case statement
    (l, alts) <- [(l, alts) | H.Case l _ alts <- exps]

    -- Select all alternatives
    let positions =
            [ positionFromScrSpanInfo $ H.ann e
            | H.Alt _ _ ga _ <- alts
            , e <- case ga of
                H.UnGuardedAlt _ e  -> [e]
                H.GuardedAlts _ ga' -> [e | H.GuardedAlt _ _ e <- ga']
            ]

    return (positions, rangeFromScrSpanInfo l)
  where
    exps :: [H.Exp H.SrcSpanInfo]
    exps = everything md

caseAlignmentChecker :: Checker [Position]
caseAlignmentChecker positions block range =
    if checkAlignmentHead (backwardAlignment range positions ["->"] block)
        then []
        else [(fst range, "Improper alignment of ->")]
