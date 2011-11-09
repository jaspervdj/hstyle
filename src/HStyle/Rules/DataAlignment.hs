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

dataSelector :: Selector ()
dataSelector (md, _) _ =
    map (\ssi -> ((), rangeFromScrSpanInfo ssi)) $ datas =<< everything md
  where
    datas :: H.Decl H.SrcSpanInfo -> [H.SrcSpanInfo]
    datas decl = case decl of
        d@(H.DataDecl _ _ _ _ _ _) -> [H.ann d]
        _                          -> []

dataAlignmentChecker :: Checker ()
dataAlignmentChecker () range block = case checkAlignmentHead alignment of
    Just t  -> [(1, t)]
    Nothing -> []
  where
    alignment = alignmentOf ["{", ",", "}"] $ getRange range block
