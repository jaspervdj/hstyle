{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.PatMatchAlignment
    ( patMatchAlignmentRule
    ) where

import Control.Arrow ((***))

import qualified Data.Map as M
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Checker
import HStyle.Fixer
import HStyle.Rule
import HStyle.Selector

patMatchAlignmentRule :: Rule
patMatchAlignmentRule =
    Rule patMatchSelector patMatchAlignmentChecker fixNothing

patMatchSelector :: Selector [Snippet]
patMatchSelector (md, _) block =
    -- TODO: fetch block?
    [ (map (flip fromSrcSpanInfoSnippet block) bds, block)
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

patMatchAlignmentChecker :: Checker [Snippet]
patMatchAlignmentChecker snippets _ = case checkAlignmentHead alignment of
    Nothing -> []
    Just t  -> [(1, t)]
  where
    alignment = [[(c, "=")] | Snippet _ _ c <- snippets]
