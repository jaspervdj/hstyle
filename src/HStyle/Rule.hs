{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module HStyle.Rule
    ( Rule (..)
    , Options (..)
    , FileState (..)
    , FileM
    , runFileM
    , runRule
    ) where

import Control.Monad (forM_, unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Writer (WriterT, runWriterT, tell)

import Data.Text (Text)
import qualified Data.Text as T

import HStyle.Block
import HStyle.Checker
import HStyle.Fixer
import HStyle.Parse
import HStyle.Selector

-- | Compose the elements of a rule. Use ExistentialQuantification so the
-- internal state of a rule cannot be touched from the outside.
data Rule = forall a. Rule (Selector a) (Checker a) (Fixer a)

-- | Options for checking files
data Options = Options
    { -- | Attempt to fix files
      optionsFix      :: Bool
    , -- | Be quiet
      optionsQuiet    :: Bool
    } deriving (Show)

data FileState = FileState
    { -- | File we're fixing
      filePath    :: FilePath
    , -- | The module in the file
      fileModule  :: Module
    , -- | A block holding the file contents
      fileBlock   :: Block
    , -- | Flag indicating whether or not the in-memory representation differs
      -- from the file on disk
      fileUpdated :: Bool
    , -- | Flag indicating that all checks were OK
      fileOk      :: Bool
    } deriving (Show)

-- | We prefer to keep the file checking out of the IO monad.
type FileM = ReaderT Options (WriterT [Text] (State FileState))

runFileM :: FileM a -> Options -> FileState -> (a, FileState, [Text])
runFileM fm options fs =
    -- Peel of the monads one by one
    let w              = runReaderT fm options
        s              = runWriterT w
        ((x, ts), fs') = runState s fs
    in (x, fs', ts)

-- | Write some text followed by a newline
putLn :: Text -> FileM ()
putLn = tell . return

-- | Represents fixing status
data Fix
    = DontFix      -- ^ User doesn't want to fix it
    | CouldntFix   -- ^ Our library is unable to fix it
    | Fixed        -- ^ Fixed, result
    deriving (Eq, Show)

runRule :: Rule -> FileM ()
runRule (Rule selector checker fixer) = do
    fs <- get
    let selections = selector (fileModule fs) (fileBlock fs)
    forM_ selections $ uncurry $ checkBlock checker fixer

checkBlock :: Checker a -> Fixer a -> a -> Range -> FileM ()
checkBlock checker fixer x range = do
    -- Query monad states
    fs      <- get
    options <- ask

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
        putLn $ T.pack (filePath fs) `T.append` ":" `T.append`
            T.pack (show line) `T.append` ": " `T.append` problem
        unless (optionsQuiet options) $ do
            case fix of
                DontFix    -> return ()
                CouldntFix -> putLn "    (Couldn't automatically fix)"
                Fixed      -> putLn "    (Fixed)"
            putLn "    Found:"
            putLn $ prettyRange 4 block range

    -- Save updated file state
    put fs
        { -- TODO: The problem is, that, when we update the code, we have to
          -- reparse the module, otherwise there's inconsistencies.
          fileBlock   = const block block'
        , fileUpdated = fileUpdated fs || fix == Fixed
        , fileOk      = fileOk fs      && null problems
        }
