{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.CaseAlignment
    ( caseAlignmentRule
    , caseSelector
    , caseAlignmentChecker
    ) where

import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

caseAlignmentRule :: Rule
caseAlignmentRule = Rule caseSelector caseAlignmentChecker fixNothing

caseSelector :: Selector [Snippet]
caseSelector (md, _) block = do
    -- Select a case statement
    (l, alts) <- [(l, alts) | H.Case l _ alts <- exps]

    -- Select all alternatives
    let ls = [ gas
             | H.Alt _ _ ga _ <- alts
             , gas <- case ga of
                        H.UnGuardedAlt l' _ -> return l'
                        H.GuardedAlts _ ga' -> map H.ann ga'
             ]

    return (map snippet ls, fromSrcSpanInfo l block)
  where
    exps :: [H.Exp H.SrcSpanInfo]
    exps = everything md
    snippet :: H.SrcSpanInfo -> Snippet
    snippet = flip fromSrcSpanInfoSnippet block

caseAlignmentChecker :: Checker [Snippet]
caseAlignmentChecker snippets _ = case checkAlignmentHead alignment of
    Nothing -> []
    Just t  -> [(1, t)]
  where
    alignment = [[(c, "->")] | Snippet _ _ c <- snippets]
