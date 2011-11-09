{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.AppSpacing.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import HStyle.Rules.AppSpacing
import HStyle.Tests.Util

tests :: Test
tests = testGroup "HStyle.Rules.AppSpacing.Tests"
    [ testCase "appSpacing_01" appSpacing_01
    , testCase "appSpacing_02" appSpacing_02
    , testCase "appSpacing_03" appSpacing_03
    , testCase "appSpacing_04" appSpacing_04
    ]

appSpacing_01 :: Assertion
appSpacing_01 = testRuleAccept appSpacingRule
    "foo n = n + 1\n\
    \f x = f (f x)\n"

appSpacing_02 :: Assertion
appSpacing_02 = testRuleReject appSpacingRule "foo n = n+1"

appSpacing_03 :: Assertion
appSpacing_03 = testRuleReject appSpacingRule "foo n = sin(n)"

appSpacing_04 :: Assertion
appSpacing_04 = testRuleReject appSpacingRule "foo x y = x`append` y"
