{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.PatMatchAlignment
    ( patMatchAlignmentRule
    ) where

import Control.Arrow ((***))

import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

patMatchAlignmentRule :: Rule
patMatchAlignmentRule =
    Rule patMatchSelector patMatchAlignmentChecker fixNothing

patMatchSelector :: Selector [Position]
patMatchSelector (md, _) block =
    -- TODO: fetch block?
    [ (map positionFromScrSpanInfo bds, (1, numLines block))
    | (_, bds) <- M.toList bindings
    ]
  where
    bindings :: M.Map (H.Name ()) [H.SrcSpanInfo]
    bindings = M.fromListWith (++) $ map (fmap (const ()) *** return)
        [ (name, loc)
        | -- Find declarations in the module
          decls <- case md of
            H.Module _ _ _ _ decls -> return decls
            _                      -> []
        , -- Find all function bindings
          H.FunBind _ matches <- decls
        , -- Take a match
          match <- matches
        , -- Select RHS
          (name, rhs) <- case match of
            H.Match _ name _ rhs _        -> return (name, rhs)
            H.InfixMatch _ _ name _ rhs _ -> return (name, rhs)
        , -- Select positions from the RHS
          loc <- case rhs of
            H.UnGuardedRhs _ e   -> return $ H.ann e
            H.GuardedRhss _ rhss -> [H.ann e | H.GuardedRhs _ _ e <- rhss]
        ]

patMatchAlignmentChecker :: Checker [Position]
patMatchAlignmentChecker positions block range =
    case checkAlignmentHead alignment of
        Nothing -> []
        Just t  -> [(1, t)]
  where
    alignment =
        [ [(c, "=")]
        | pos <- positions
        -- We get the power of the /expression/, but we actually want the "="
        -- signs to be aligned, so we have to search backwards, starting from
        -- the expression.
        , (_, c) <- maybeToList $ findBackwards range pos "=" block
        ]
