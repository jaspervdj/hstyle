{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module HStyle.Rule
    ( Rule (..)
    , FileState (..)
    , Options (..)
    , runRule
    ) where

import Control.Monad (foldM, forM_, unless)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block
import HStyle.Selector
import HStyle.Checker
import HStyle.Fixer

-- | Compose the elements of a rule. Use ExistentialQuantification so the
-- internal state of a rule cannot be touched from the outside.
data Rule = forall a. Rule (Selector a) (Checker a) (Fixer a)

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

runRule :: Options -> FilePath
        -> (H.Module H.SrcSpanInfo, [H.Comment])
        -> FileState -> Rule
        -> IO FileState
runRule options file mdc fileState (Rule selector checker fixer) =
    foldM step fileState $ selector mdc $ fileBlock fileState
  where
    step fs (x, b) = checkBlock options file checker fixer fs x b

checkBlock :: Options -> FilePath -> Checker a -> Fixer a -> FileState
           -> a -> Block
           -> IO FileState
checkBlock options file checker fixer fs x block = do
    -- Determine problems, and attempt to fix (lazily)
    let problems      = checker x block
        (fix, block') = case (optionsFix options, fixer x block) of
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
