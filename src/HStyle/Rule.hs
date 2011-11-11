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
runRule rule@(Rule selector checker fixer) = do
    fs <- get
    check $ selector (fileModule fs) (fileBlock fs)
  where
    -- Check the files one by one. However, note that if we fixed a file, we
    -- need to re-run the current rule, because e.g. line number might have
    -- changed, so our selections will no longer be valid.
    check []                    = return ()
    check ((x, r) : selections) = do
        fix <- checkBlock checker fixer x r
        case fix of
            Fixed -> runRule rule
            _     -> check selections

checkBlock :: Checker a -> Fixer a -> a -> Range -> FileM Fix
checkBlock checker fixer x range = do
    -- Query monad states
    fs      <- get
    options <- ask

    -- Determine problems, and attempt to fix (lazily)
    let block         = fileBlock fs
        problems      = checker x block range
        needFix       = optionsFix options && not (null problems)
        (fix, block') = case (needFix, fixer x block range) of
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
                DontFix    -> putLn "    Found:"
                CouldntFix -> putLn "    Couldn't fix:"
                Fixed      -> putLn "    Fixed:"
            putLn $ prettyRange 4 block range

    -- If we fixed anything, re-parse the module. Parsing here should really,
    -- not fail, because if it does, we made the code unparseable with our own
    -- fix...
    let (module', _) = case fix of
            Fixed -> either error id $
                parseModule (Just $ filePath fs) (toText block')
            _     -> (fileModule fs, block')

    -- Save updated file state
    put fs
        { fileModule  = module'
        , fileBlock   = block'
        , fileUpdated = fileUpdated fs || fix == Fixed
        , fileOk      = fileOk fs      && null problems
        }

    -- Return fix resolution
    return fix
