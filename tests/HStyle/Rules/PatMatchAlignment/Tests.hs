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
    [ testCase "patMatchAlignment_01" patMatchAlignment_01
    , testCase "patMatchAlignment_02" patMatchAlignment_02
    , testCase "patMatchAlignment_03" patMatchAlignment_03
    ]

patMatchAlignment_01 :: Assertion
patMatchAlignment_01 = testRuleAccept patMatchAlignmentRule
    "fib 0 = 1\n\
    \fib n = fib (n - 1) (n - 2)\n\
    \"

patMatchAlignment_02 :: Assertion
patMatchAlignment_02 = testRuleAccept patMatchAlignmentRule
    "fun 0           = 1\n\
    \fun n\n\
    \    | n > 2     = 8\n\
    \    | otherwise = 10\n\
    \"

patMatchAlignment_03 :: Assertion
patMatchAlignment_03 = testRuleReject patMatchAlignmentRule
    "fun 0           =  1\n\
    \fun n\n\
    \    | n > 2      = 8\n\
    \    | otherwise =  10\n\
    \"
