import Test.HUnit (
  Test(..)
  , assertEqual
  , (~=?),(~:)
  , runTestTT)
import LedgerData (
  Currency(..)
  , Value(..)
  , Posting(..)
  , negateValue
  , postingToLedger
  )

main = runTestTT $ TestList [negatesValue, producesPosting]

negatesValue :: Test
negatesValue =
  "negates value" ~: expected ~=? (negateValue original)
  where
    original = Value 1 PLN
    expected = Value (-1) PLN


producesPosting :: Test
producesPosting =
  "produces posting" ~: expected ~=? (postingToLedger posting)
  where
    posting = Posting
      "A"
      (Value 100 PLN)
      (Value 200 PLN)
      (Value 0.25 CHF)
    expected :: String
    expected = "  A  100 PLN @ 0.25 CHF = 200 PLN"
