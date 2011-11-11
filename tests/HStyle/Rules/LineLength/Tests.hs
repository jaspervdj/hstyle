{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.LineLength.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)
import qualified Data.Text as T

import HStyle.Rule
import HStyle.Rules.LineLength
import HStyle.Tests.Util

-- | Instantiation for 78 characters
lineLengthRule' :: Rule
lineLengthRule' = lineLengthRule 78

tests :: Test
tests = testGroup "HStyle.Rules.LineLength.Tests"
    [ testCase "lineLength_01" lineLength_01
    , testCase "lineLength_02" lineLength_02
    ]

lineLength_01 :: Assertion
lineLength_01 = testRuleAccept lineLengthRule' $ T.replicate 78 "-"

lineLength_02 :: Assertion
lineLength_02 = testRuleReject lineLengthRule' $ T.replicate 79 "-"
