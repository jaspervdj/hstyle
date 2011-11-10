{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.DataAlignment.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import HStyle.Rules.DataAlignment
import HStyle.Tests.Util

tests :: Test
tests = testGroup "HStyle.Rules.DataAlignment.Tests"
    [ testCase "dataAlignment_01" dataAlignment_01
    , testCase "dataAlignment_02" dataAlignment_02
    , testCase "dataAlignment_03" dataAlignment_03
    ]

dataAlignment_01 :: Assertion
dataAlignment_01 = testRuleAccept dataAlignmentRule
    "data E = L | R"

dataAlignment_02 :: Assertion
dataAlignment_02 = testRuleAccept dataAlignmentRule
    "data E\n\
    \    = L\n\
    \    | R\n"

dataAlignment_03 :: Assertion
dataAlignment_03 = testRuleReject dataAlignmentRule
    "data E = L\n\
    \    | R\n"
