{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.TypeSigAlignment.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import HStyle.Rules.TypeSigAlignment
import HStyle.Tests.Util

tests :: Test
tests = testGroup "HStyle.Rules.TypeSigAlignment.Tests"
    [ testCase "typeSigAlignment_01" typeSigAlignment_01
    , testCase "typeSigAlignment_02" typeSigAlignment_02
    , testCase "typeSigAlignment_03" typeSigAlignment_03
    ]

typeSigAlignment_01 :: Assertion
typeSigAlignment_01 = testRuleAccept typeSigAlignmentRule
    "foo :: Int -> Int"

typeSigAlignment_02 :: Assertion
typeSigAlignment_02 = testRuleAccept typeSigAlignmentRule
    "foo :: Monad m\n\
    \    => Int -> Int\n\
    \    -> Maybe Int\n"

typeSigAlignment_03 :: Assertion
typeSigAlignment_03 = testRuleReject typeSigAlignmentRule
    "foo :: Monad m\n\
    \    => Int -> Int\n\
    \   -> Maybe Int\n"
