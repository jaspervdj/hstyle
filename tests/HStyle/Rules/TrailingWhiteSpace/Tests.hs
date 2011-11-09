{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.TrailingWhiteSpace.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import HStyle.Rules.TrailingWhiteSpace
import HStyle.Tests.Util

tests :: Test
tests = testGroup "HStyle.Rules.TrailingWhiteSpace.Tests"
    [ testCase "trailingWhiteSpace_01" trailingWhiteSpace_01
    , testCase "trailingWhiteSpace_02" trailingWhiteSpace_02
    ]

trailingWhiteSpace_01 :: Assertion
trailingWhiteSpace_01 = testRuleAccept trailingWhiteSpaceRule "\n\n"

trailingWhiteSpace_02 :: Assertion
trailingWhiteSpace_02 = testRuleReject trailingWhiteSpaceRule
    "foo :: Int -> Int\n\
    \foo n = n + 1 \n"
