{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( FileState (..)
    , Options (..)
    , checkStyle
    ) where

import Control.Monad (foldM, when)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block
import HStyle.Rule
import HStyle.Rules.LineLength
import HStyle.Rules.Tabs
import HStyle.Rules.TrailingWhiteSpace
import HStyle.Rules.TypeSigAlignment

-- | Filter out lines which use CPP macros
unCPP :: String -> String
unCPP = unlines . map unCpp' . lines
  where
    unCpp' x
        | "#" `isPrefixOf` x = ""
        | otherwise          = x

checkStyle :: Options -> FilePath -> IO FileState
checkStyle options file = do
    contents <- readFile file
    let block     = fromText $ T.pack contents
        -- Determine the extensions used in the file, and update the parsing
        -- mode based upon those
        exts      = fromMaybe [] $ H.readExtensions contents
        mode      = H.defaultParseMode {H.extensions = exts}
        -- Special handling for CPP, haskell-src-exts can't deal with it
        contents' = if H.CPP `elem` exts then unCPP contents else contents
        fs        = FileState block False True
    case H.parseModuleWithComments mode contents' of
        H.ParseOk x -> do
            fs' <- foldM (runRule options file x) fs
                [ typeSigAlignmentRule
                , tabsRule 4
                , lineLengthRule 78
                , trailingWhiteSpaceRule
                ]
            when (fileUpdated fs') $ T.writeFile file $ toText $ fileBlock fs'
            return fs'
        err         -> do
            putStrLn $ "HStyle.checkStyle: could not parse " ++
                file ++ ": " ++ show err
            return fs
