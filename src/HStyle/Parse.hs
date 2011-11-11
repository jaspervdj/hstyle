module HStyle.Parse
    ( Module
    , parseModule
    ) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Block

-- | Concrete module type
type Module = (H.Module H.SrcSpanInfo, [H.Comment])

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
            -> Either String (Module, Block)
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
        H.ParseOk md -> Right (md, block)
        err          -> Left $ "HStyle.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ show err
