{-# LANGUAGE OverloadedStrings #-}
module HStyle.Block.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))
import qualified Data.Text as T

import HStyle.Block

tests :: Test
tests = testGroup "HStyle.Block.Tests"
    [ testCase "subRange_01"      subRange_01
    , testCase "updateRange_01"   updateRange_01
    , testCase "findBackwards_01" findBackwards_01
    , testCase "findBackwards_02" findBackwards_02
    , testCase "findBackwards_03" findBackwards_03
    ]

subRange_01 :: Assertion
subRange_01 = getRange poem (2, 3) @?=
    ["A little man who wasn't there", "He wasn't there again today"]

updateRange_01 :: Assertion
updateRange_01 = toLines (updateRange (2, 3) new poem) @?=
    [ "Last night I saw upon the stair"
    , "A little man who wasn't there..."
    , "He wasn't there again today..."
    , "Oh, how I wish he'd go away"
    ]
  where
    new = map (`T.append` "...") $ getRange poem (2, 3)

findBackwards_01 :: Assertion
findBackwards_01 = findBackwards (2, 3) (3, 21) "a" poem @?= Just (3, 19)

findBackwards_02 :: Assertion
findBackwards_02 = findBackwards (1, 4) (3, 21) "xxx" poem @?= Nothing

findBackwards_03 :: Assertion
findBackwards_03 = findBackwards (1, 4) (4, 20) "there" poem @?= Just (3, 11)

poem :: Block
poem = fromText
    "Last night I saw upon the stair\n\
    \A little man who wasn't there\n\
    \He wasn't there again today\n\
    \Oh, how I wish he'd go away"
