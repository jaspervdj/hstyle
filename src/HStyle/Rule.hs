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
           -> a -> Range
           -> IO FileState
checkBlock options file checker fixer fs x range = do
    -- Determine problems, and attempt to fix (lazily)
    let block         = fileBlock fs
        problems      = checker x block range
        (fix, block') = case (optionsFix options, fixer x block range) of
            (False, _)      -> (DontFix, block)
            (True, Nothing) -> (CouldntFix, block)
            (True, Just ls) -> (Fixed, updateRange range ls block)

    -- Output our results for this check
    forM_ problems $ \(line, problem) -> do
        -- let line = absoluteLineNumber i block
        T.putStrLn $ T.pack file `T.append` ":" `T.append`
            T.pack (show line) `T.append` ": " `T.append` problem
        unless (optionsQuiet options) $ do
            T.putStrLn "    Found:"
            T.putStr $ prettyRange 4 block range
            case fix of
                DontFix    -> return ()
                CouldntFix -> T.putStrLn "    (Couldn't automatically fix)"
                Fixed      -> T.putStrLn "    (Fixed)"
            T.putStrLn ""

    -- Return updated file state
    return fs
        { -- TODO: The problem is, that, when we update the code, we have to
          -- reparse the module, otherwise there's inconsistencies.
          fileBlock   = const block block'
        , fileUpdated = fileUpdated fs || fix == Fixed
        , fileOk      = fileOk fs      && null problems
        }
