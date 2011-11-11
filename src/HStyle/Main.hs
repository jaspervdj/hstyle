{-# LANGUAGE DeriveDataTypeable #-}
module HStyle.Main
    ( mainWith
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeExtension, (</>))

import qualified Data.Text.IO as T
import System.Console.CmdArgs

import HStyle.Block
import HStyle.Parse
import HStyle.Rule

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

-- | Customizable main function
mainWith :: [Rule] -> IO ()
mainWith rules = do
    config <- cmdArgs hstyle
    -- Expand all files
    files' <- filter isCheckable . concat <$> mapM getFiles (files config)
    ok <- all fileOk <$> mapM (checkStyle rules $ toOptions config) files'
    exitWith $ if ok then ExitSuccess else ExitFailure 1

checkStyle :: [Rule] -> Options -> FilePath -> IO FileState
checkStyle rules options file = do
    text <- T.readFile file
    case parseModule (Just file) text of
        Left err          -> error err
        Right (md, block) -> do
            let fs           = FileState file md block False True
                fm           = mapM_ runRule rules
                (_, fs', ts) = runFileM fm options fs

            mapM_ T.putStrLn ts
            when (fileUpdated fs') $ T.writeFile file $ toText $ fileBlock fs'
            return fs'
