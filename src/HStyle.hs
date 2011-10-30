{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( FileState (..)
    , Options (..)
    , checkStyle
    ) where

import Control.Monad (foldM, forM_, unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Selector
import HStyle.Checker
import HStyle.Fixer

-- | A selector and a check...
type Rule = (Selector, Checker, Fixer)

data FileState = FileState
    { -- | A block holding the file contents
      fileBlock   :: Block
    , -- | Flag indicating whether or not the in-memory representation differs
      -- from the file on disk
      fileUpdated :: Bool
    , -- | Flag indicating that all checks were OK
      fileOk      :: Bool
    } deriving (Show)

-- | Options for checking files
data Options = Options
    { -- | Attempt to fix files
      optionsFix   :: Bool
    , -- | Be quiet
      optionsQuiet :: Bool
    } deriving (Show)

-- | Represents fixing status
data Fix
    = DontFix      -- ^ User doesn't want to fix it
    | CouldntFix   -- ^ Our library is unable to fix it
    | Fixed        -- ^ Fixed, result
    deriving (Eq, Show)

runRule :: Options -> FilePath -> H.Module H.SrcSpanInfo -> FileState -> Rule
        -> IO FileState
runRule options file md fileState (selector, checker, fixer) =
    foldM (checkBlock options file checker fixer) fileState $
        selector md $ fileBlock fileState

checkBlock :: Options -> FilePath -> Checker -> Fixer -> FileState -> Block
           -> IO FileState
checkBlock options file checker fixer fs block = do
    -- Determine problems, and attempt to fix (lazily)
    let problems        = checker block
        (fix, block') = case (optionsFix options, fixer block) of
            (False, _)      -> (DontFix, block)
            (True, Nothing) -> (CouldntFix, block)
            (True, Just b)  -> (Fixed, b)

    -- Output our results for this check
    forM_ problems $ \(i, problem) -> do
        let line = absoluteLineNumber i (fileBlock fs)
        T.putStrLn $ T.pack file `T.append` ":" `T.append`
            T.pack (show line) `T.append` ": " `T.append` problem
        unless (optionsQuiet options) $ do
            T.putStrLn "    Found:"
            T.putStr   $ prettyBlock 4 block
            case fix of
                DontFix    -> return ()
                CouldntFix -> T.putStrLn "    (Couldn't automatically fix)"
                Fixed      -> do
                    T.putStrLn "    Fixed to:"
                    T.putStr $ prettyBlock 4 block'
            T.putStrLn ""

    -- Return updated file state
    return fs
        { fileBlock   = updateSubBlock block block' (fileBlock fs)
        , fileUpdated = fileUpdated fs || fix == Fixed
        , fileOk      = fileOk fs      && null problems
        }

fromSrcSpanInfo :: H.SrcSpanInfo -> Block -> Block
fromSrcSpanInfo ssi = subBlock start end
  where
    span' = H.srcInfoSpan ssi
    start = H.srcSpanStartLine span'
    end   = H.srcSpanEndLine span'

typeSigSelector :: Selector
typeSigSelector md block = map (flip fromSrcSpanInfo block) $ tss md
  where
    tss (H.Module _ _ _ _ decls) = [ssi | H.TypeSig ssi _ _ <- decls]
    tss _                        = []

typeSigCheck :: Checker
typeSigCheck block = case checkAlignmentHead alignment of
    Just t  -> [(1, t)]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] $ toLines block

tabsCheck :: Checker
tabsCheck = checkLines $ \line -> case T.findIndex (== '\t') line of
    Nothing -> Nothing
    Just i  -> Just $ "\\t at column " `T.append` T.pack (show $ i + 1)

lineLengthCheck :: Int -> Checker
lineLengthCheck max' = checkLines $ \line -> if T.length line > max'
    then Just $ "Exceeds max line length of " `T.append` T.pack (show max')
    else Nothing

trailingWhiteSpace :: Checker
trailingWhiteSpace = checkLines $ \line ->
    if not (T.null line) && isSpace (T.last line)
        then Just "Trailing whitespace"
        else Nothing

trailingWhiteSpaceFixer :: Fixer
trailingWhiteSpaceFixer = fixLines T.stripEnd

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
    case H.parseModuleWithMode mode contents' of
        H.ParseOk md -> do
            fs' <- foldM (runRule options file md) fs
                [ (typeSigSelector, typeSigCheck,       fixNothing)
                , (selectLines,     tabsCheck,          fixNothing)
                , (selectLines,     lineLengthCheck 78, fixNothing)
                , (selectLines,     trailingWhiteSpace, trailingWhiteSpaceFixer)
                ]
            when (fileUpdated fs') $ T.writeFile file $ toText $ fileBlock fs'
            return fs'
        err         -> do
            putStrLn $ "HStyle.checkStyle: could not parse " ++
                file ++ ": " ++ show err
            return fs
