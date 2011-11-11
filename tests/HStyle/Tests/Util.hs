{-# LANGUAGE OverloadedStrings #-}
module HStyle.Tests.Util
    ( testRule
    , testRuleAccept
    , testRuleReject
    ) where

import Test.HUnit (Assertion, (@?))
import Data.Text (Text)

import HStyle
import HStyle.Rule

-- | Check a module and return the updated filestate
testRule :: Rule -> Text -> FileState
testRule rule text =
    let Right (md, block) = parseModule Nothing text
        options           = Options True False
        fs                = FileState "<unknown>" md block False True
        (_, fs', _)       = runFileM (runRule rule) options fs
    in fs'

-- | Test if a rule accepts a certain module without problems
testRuleAccept :: Rule -> Text -> Assertion
testRuleAccept rule text = fileOk (testRule rule text) @? "testRuleOk"

-- | Test if a rule rejects a certain module (e.g., at least one problem)
testRuleReject :: Rule -> Text -> Assertion
testRuleReject rule text = not (fileOk $ testRule rule text) @? "testRuleReject"
