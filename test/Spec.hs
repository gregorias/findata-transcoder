import Test.HUnit (
  Test(..)
  , assertEqual
  , runTestTT)
import LedgerData (
  Currency(..)
  , Value(..)
  , Posting(..)
  , negateValue
  , postingToLedger
  )

main = runTestTT [testNegateValue, testShouldProducePosting]

testNegateValue :: Test
testNegateValue =
  TestCase $ assertEqual "" expected (negateValue original)
  where
    original = Value 1 PLN
    expected = Value (-1) PLN


testShouldProducePosting :: Test
testShouldProducePosting =
  TestCase $ assertEqual "" expected (postingToLedger posting)
  where
    posting = Posting
      "A"
      (Value 100 PLN)
      (Value 200 PLN)
      (Value 0.25 CHF)
    expected :: String
    expected = "  A  100 PLN @ 0.25 CHF = 200 PLN"
