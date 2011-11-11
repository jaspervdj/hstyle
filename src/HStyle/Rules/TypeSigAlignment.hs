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

typeSigSelector :: Selector [Position]
typeSigSelector (md, _) _ =
    [ (map (positionFromScrSpanInfo . H.ann) types, rangeFromScrSpanInfo loc)
    | H.Module _ _ _ _ decls <- [md]
    , H.TypeSig loc _ typ    <- decls
    , let types = defunty typ
    ]
  where
    defunty (H.TyForall _ _ _ t) = defunty t
    defunty (H.TyFun _ t ts)     = t : defunty ts
    defunty t                    = [t]

typeSigAlignmentChecker :: Checker [Position]
typeSigAlignmentChecker pos block range =
    if checkAlignmentHead (backwardAlignment range pos ["::", "=>", "->"] block)
        then []
        else [(fst range, "Improper alignment of ::, =>, ->")]
