{-# LANGUAGE OverloadedStrings #-}
module HStyle.Rules.Tabs.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=)) 

import HStyle.Block
import HStyle.Rules.Tabs

tests :: Test
tests = testGroup "HStyle.Rules.Tabs.Tests"
    [ testCase "tabsFixer_01" tabsFixer_01
    , testCase "tabsFixer_02" tabsFixer_02
    , testCase "tabsFixer_03" tabsFixer_03
    ]

tabsFixer_01 :: Assertion
tabsFixer_01 =
    tabsFixer 4 () (fromText "Hello world") (1, 1) @?= Just ["Hello world"]

tabsFixer_02 :: Assertion
tabsFixer_02 =
    tabsFixer 4 () (fromText "\t\t") (1, 1) @?= Just ["        "]

tabsFixer_03 :: Assertion
tabsFixer_03 = tabsFixer 4 () (fromText "\tif foo\tfi") (1, 1) @?=
    Just ["    if foo    fi"]
