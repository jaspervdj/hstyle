-- | This is a very simple project containing a few code style checks for use
-- in git hooks for the Snap Framework. Hopefully we'll get some more
-- sophisticated checks and automatic fixes implemented eventually.
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeExtension, (</>))

import System.Console.CmdArgs

import HStyle

-- | CmdArgs-enabled data-type
data HStyle = HStyle
    { fix   :: Bool
    , quiet :: Bool
    , files :: [FilePath]
    } deriving (Show, Data, Typeable)

-- | CmdArgs configuration
hstyle :: HStyle
hstyle = HStyle
    { fix   = def &= help "Automatically fix (some) problems"
    , quiet = def &= help "Print less output"
    , files = def &= args 
    }

-- | Convert CmdArgs configuration to cleaner datatype
toOptions :: HStyle -> Options
toOptions hs = Options
    { optionsFix   = fix hs
    , optionsQuiet = quiet hs
    }

-- | Recursively list the contents of a directory. Only returns regular files.
getFiles :: FilePath -> IO [FilePath]
getFiles path = do
    isDir <- doesDirectoryExist path
    if not isDir
        then return [path]
        else do
            contents <- filter proper <$> getDirectoryContents path
            concat <$> mapM (getFiles . (path </>)) contents
  where
    proper = not . (`elem` [".", ".."])

-- | Is a file haskell?
isCheckable :: FilePath -> Bool
isCheckable fp = case takeExtension fp of
    ".hs" -> True
    _     -> False

-- | Simple main that takes one command-line parameter of "check" or "fix" and
-- a list of files to be checked.
main :: IO ()
main = do
    config <- cmdArgs hstyle
    -- Expand all files
    files' <- filter isCheckable . concat <$> mapM getFiles (files config)
    ok <- all fileOk <$> mapM (checkStyle $ toOptions config) files'
    exitWith $ if ok then ExitSuccess else ExitFailure 1
