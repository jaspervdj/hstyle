{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.CaseAlignment.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import HStyle.Rules.CaseAlignment
import HStyle.Tests.Util

tests :: Test
tests = testGroup "HStyle.Rules.CaseAlignment.Tests"
    [ testCase "caseAlignment_01" caseAlignment_01
    , testCase "caseAlignment_02" caseAlignment_02
    , testCase "caseAlignment_03" caseAlignment_03
    ]

caseAlignment_01 :: Assertion
caseAlignment_01 = testRuleAccept caseAlignmentRule
    "fib n = case n of\n\
    \    0 -> 1\n\
    \    _ -> fib (n - 1) (n - 2)\n"

caseAlignment_02 :: Assertion
caseAlignment_02 = testRuleAccept caseAlignmentRule
    "fun x = case x of\n\
    \    0             -> 1\n\
    \    n | n > 2     -> 8\n\
    \      | otherwise -> 10\n"

caseAlignment_03 :: Assertion
caseAlignment_03 = testRuleReject caseAlignmentRule
    "fun x = case x of\n\
    \    0 -> 1\n\
    \    n | n > 2 -> 8\n\
    \      | otherwise -> 10\n"
