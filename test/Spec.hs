import LedgerData
  ( Currency (..),
    Posting (..),
    Value (..),
    negateValue,
    postingToLedger,
    valueToLedger,
  )
import qualified Test.Hledger.Data.Extra as HDE
import qualified Test.Hledger.Data.Lens
import qualified Test.Hledger.Data.MarketPrice.Extra
import qualified Test.Hledger.Read.TestUtils
import Test.Hledupt.Bcge (bcgeTests)
import qualified Test.Hledupt.Bcge.Hint as BcgeHint
import qualified Test.Hledupt.Data
import qualified Test.Hledupt.Ib
import qualified Test.Hledupt.Ib.Csv.RawParse
import Test.Hledupt.Mbank (mbankTests)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import qualified Test.Text.Megaparsec.Char.Extra

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  bcgeTests
  BcgeHint.tests
  mbankTests
  HDE.tests
  Test.Hledger.Data.Lens.tests
  Test.Hledger.Data.MarketPrice.Extra.tests
  Test.Hledger.Read.TestUtils.tests
  Test.Hledupt.Data.dataTests
  Test.Hledupt.Ib.Csv.RawParse.tests
  Test.Hledupt.Ib.tests
  Test.Text.Megaparsec.Char.Extra.tests
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
