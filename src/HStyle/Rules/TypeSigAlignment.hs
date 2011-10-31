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
typeSigAlignmentRule = (typeSigSelector, typeSigAlignmentChecker, fixNothing)

typeSigSelector :: Selector
typeSigSelector md block = map (flip fromSrcSpanInfo block) $ tss md
  where
    tss (H.Module _ _ _ _ decls) = [ssi | H.TypeSig ssi _ _ <- decls]
    tss _                        = []

typeSigAlignmentChecker :: Checker
typeSigAlignmentChecker block = case checkAlignmentHead alignment of
    Just t  -> [(1, t)]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] $ toLines block
