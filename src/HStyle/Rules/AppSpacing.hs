-- | Checks spacing around function applications
{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.AppSpacing
    ( appSpacingRule
    , appSelector
    , appSpacingChecker
    ) where

import Data.Maybe (maybeToList)

import Data.Generics (everything)
import Data.Typeable (cast)
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

exps :: H.Module H.SrcSpanInfo -> [H.Exp H.SrcSpanInfo]
exps = everything (++) (maybeToList . cast)

appSpacingRule :: Rule
appSpacingRule = Rule appSelector appSpacingChecker fixNothing

appSelector :: Selector ()
appSelector (md, _) block =
    [ ((), fromSrcSpanInfo (H.ann e) block)
    | e@(H.InfixApp _ _ _ _) <- exps md
    ]

appSpacingChecker :: Checker ()
appSpacingChecker _ _ = [(1, "Problem.")]
