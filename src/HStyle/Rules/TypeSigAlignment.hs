{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.TypeSigAlignment
    ( typeSigAlignmentRule
    , typeSigSelector
    , typeSigAlignmentChecker
    ) where

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

typeSigAlignmentRule :: Rule
typeSigAlignmentRule = Rule typeSigSelector typeSigAlignmentChecker fixNothing

typeSigSelector :: Selector ()
typeSigSelector (md, _) _ =
    map (\ssi -> ((), rangeFromScrSpanInfo ssi)) $ tss md
  where
    tss (H.Module _ _ _ _ decls) = [ssi | H.TypeSig ssi _ _ <- decls]
    tss _                        = []

typeSigAlignmentChecker :: Checker ()
typeSigAlignmentChecker () block range = case checkAlignmentHead alignment of
    Just t  -> [(fst range, t)]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] $ getRange block range
