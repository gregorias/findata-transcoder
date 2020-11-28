import LedgerData
  ( Currency (..),
    Posting (..),
    Value (..),
    negateValue,
    postingToLedger,
    valueToLedger,
  )
import qualified Test.Hledger.Data.Extra as HDE
import Test.Hledupt.Bcge (bcgeTests)
import qualified Test.Hledupt.Bcge.Hint as BcgeHint
import Test.Hledupt.Data (dataTests)
import Test.Hledupt.Ib (ibTests)
import Test.Hledupt.Mbank (mbankTests)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  dataTests
  bcgeTests
  ibTests
  BcgeHint.tests
  mbankTests
  HDE.tests
  describe "LedgerData tests" $ do
    describe "negateValue" $ do
      it "negates a value" $ do
        negateValue (Value 1 PLN) `shouldBe` Value (-1) PLN

    describe "valueToLedger" $ do
      it "produces a value string" $ do
        valueToLedger (Value 100 PLN) `shouldBe` "100 PLN"

    describe "postingToLedger" $ do
      it "produces a posting" $ do
        postingToLedger posting `shouldBe` expected
  where
    posting =
      Posting
        "A"
        (Value 100 PLN)
        (Value 200 PLN)
        (Value 0.25 CHF)
    expected :: String
    expected = "  A  100 PLN @ 0.25 CHF = 200 PLN"
