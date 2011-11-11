module HStyle.Tests.Util
    ( testRule
    , testRuleAccept
    , testRuleReject
    ) where

import Test.HUnit (Assertion, (@?))
import Data.Text (Text)

import HStyle
import HStyle.Rule

-- | Check a module and list the errors
testRule :: Rule -> Text -> [(Int, Text)]
testRule (Rule selector checker _) text = do
    let Right (md, block) = parseModule Nothing text
    (x, range) <- selector md block
    checker x block range

-- | Test if a rule accepts a certain module without problems
testRuleAccept :: Rule -> Text -> Assertion
testRuleAccept rule text = null (testRule rule text) @? "testRuleOk"

-- | Test if a rule rejects a certain module (e.g., at least one problem)
testRuleReject :: Rule -> Text -> Assertion
testRuleReject rule text = not (null $ testRule rule text) @? "testRuleReject"
