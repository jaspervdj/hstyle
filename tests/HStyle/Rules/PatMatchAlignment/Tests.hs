{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.PatMatchAlignment.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import HStyle.Rules.PatMatchAlignment
import HStyle.Tests.Util

tests :: Test
tests = testGroup "HStyle.Rules.Tabs.Tests"
    [ testCase "patMatchAlignmentChecker_01" patMatchAlignmentChecker_01
    ]

patMatchAlignmentChecker_01 :: Assertion
patMatchAlignmentChecker_01 = testRuleAccept patMatchAlignmentRule
    "fib 0 = 1\n\
    \fib n = fib (n - 1) (n - 2)\n\
    \"
