module TestSafePrelude where

import Test.HUnit
import SafePrelude

testSafeHeadForEmptyList :: Test
testSafeHeadForEmptyList = TestCase $ assertEqual "Should Return Nothing for Empty List " Nothing (safeHead ([] :: [Int]))

testSafeHeadForNonEmptyList :: Test
h = safeHead ([1])
testSafeHeadForNonEmptyList = TestCase $ assertEqual "Should Return (Just Head) for Non Empty List" (Just 1) h

main :: IO Counts
main = runTestTT $ TestList [testSafeHeadForEmptyList, testSafeHeadForNonEmptyList]
