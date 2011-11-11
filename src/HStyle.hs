{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module HStyle
    ( FileState (..)
    , Options (..)
    , parseModule
    , checkStyle
    ) where

import Control.Monad (when)

import qualified Data.Text.IO as T

import HStyle.Block
import HStyle.Parse
import HStyle.Rule
import HStyle.Rules.AppSpacing
import HStyle.Rules.CaseAlignment
import HStyle.Rules.DataAlignment
import HStyle.Rules.LineLength
import HStyle.Rules.PatMatchAlignment
import HStyle.Rules.Tabs
import HStyle.Rules.TrailingWhiteSpace
import HStyle.Rules.TypeSigAlignment

checkStyle :: Options -> FilePath -> IO FileState
checkStyle options file = do
    text <- T.readFile file
    case parseModule (Just file) text of
        Left err          -> error err
        Right (md, block) -> do
            let fs = FileState file md block False True
                fm = mapM_ runRule
                        [ appSpacingRule
                        , caseAlignmentRule
                        , dataAlignmentRule
                        , lineLengthRule 78
                        , patMatchAlignmentRule
                        , tabsRule 4
                        , trailingWhiteSpaceRule
                        , typeSigAlignmentRule
                        ]
                (_, fs', ts) = runFileM fm options fs

            mapM_ T.putStrLn ts
            when (fileUpdated fs') $ T.writeFile file $ toText $ fileBlock fs'
            return fs'
