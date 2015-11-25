module TestParserModule where

import Test.HUnit
import ParserModule

testItemParser :: Test
testItemParser = TestCase $ assertEqual "Should Return Nothing for Empty List " Nothing (item [])

testItemParserForNonEmptyList :: Test
h = item ("welcome")
testItemParserForNonEmptyList = TestCase $ assertEqual "Should Return (Just Head) for Non Empty List" ("w", "elcome") h

{--
- failure "we"
- failure []
- failure ""
-
- item "welcome
- item ""
- item []
-
- return "One" $ "Two"
- --}

main :: IO Counts
main = runTestTT $ TestList [testItemParser, testItemParserForNonEmptyList]
