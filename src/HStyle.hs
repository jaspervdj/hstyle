{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module HStyle
    ( FileState (..)
    , Options (..)
    , parseModule
    , checkStyle
    ) where

import Control.Monad (foldM, when)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block
import HStyle.Rule
import HStyle.Rules.AppSpacing
import HStyle.Rules.CaseAlignment
import HStyle.Rules.DataAlignment
import HStyle.Rules.EolComment
import HStyle.Rules.LineLength
import HStyle.Rules.PatMatchAlignment
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

-- | Abstraction over HSE's parsing
parseModule :: Maybe FilePath
            -> Text
            -> Either String (H.Module H.SrcSpanInfo, [H.Comment], Block)
parseModule mfp text =
    let fp       = fromMaybe "<unknown>" mfp
        string   = T.unpack text
        block    = fromText text
        -- Determine the extensions used in the file, and update the parsing
        -- mode based upon those
        exts     = fromMaybe [] $ H.readExtensions string
        mode     = H.defaultParseMode
            {H.extensions = exts, H.fixities = Nothing}
        -- Special handling for CPP, haskell-src-exts can't deal with it
        string'  = if H.CPP `elem` exts then unCPP string else string
    in case H.parseModuleWithComments mode string' of
        H.ParseOk (md, cm) -> Right (md, cm, block)
        err                -> Left $ "HStyle.parseModule: could not parse " ++
            fp ++ ": " ++ show err

checkStyle :: Options -> FilePath -> IO FileState
checkStyle options file = do
    text <- T.readFile file
    case parseModule (Just file) text of
        Left err              -> error err
        Right (md, cm, block) -> do
            fs' <- foldM (runRule options file (md, cm)) (newFileState block)
                [ appSpacingRule
                , caseAlignmentRule
                , dataAlignmentRule
                , eolCommentRule
                , lineLengthRule 78
                , patMatchAlignmentRule
                , tabsRule 4
                , trailingWhiteSpaceRule
                , typeSigAlignmentRule
                ]
            when (fileUpdated fs') $ T.writeFile file $ toText $ fileBlock fs'
            return fs'
  where
    newFileState block = FileState block False True
