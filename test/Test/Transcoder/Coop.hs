module Test.Transcoder.Coop (
  tests,
) where

import Data.ByteString (fromStrict)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hledger.Read.TestUtils (transactionQQ)
import NeatInterpolation (trimming)
import Test.HUnit (assertFailure)
import Test.HUnit.Extra (assertRight)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Coop qualified as Coop
import Transcoder.Coop.Config qualified as Coop
import Prelude

readCoopConfig :: Text -> IO Coop.Config
readCoopConfig json =
  either
    (\e -> assertFailure $ "Could not decode the config. " <> T.unpack e)
    return
    (Coop.decodeConfig . fromStrict $ encodeUtf8 json)

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Coop" $ do
    describe "receiptToLedger Coop.emptyConfig" $ do
      it "convertsToATransaction" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE  -6.10 CHF
                    Expenses:Groceries  4.50 CHF
                    Expenses:Groceries:Chewing Gum  1.60 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyAssignsCategories" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-cat.txt"
        let expectedTr =
              [transactionQQ|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -0.17 CHF
                  Expenses:Household              0.11 CHF
                  Expenses:Groceries:Ready Meals  0.03 CHF
                  Expenses:Gesundheit             0.02 CHF
                  Expenses:Groceries:Chewing Gum  0.01 CHF|]

        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyHandlesARabatt" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-rabatt.txt"
        let expectedTr =
              [transactionQQ|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -44.35 CHF
                  Expenses:Household      9.40 CHF
                  Expenses:Groceries     37.35 CHF
                  Expenses:Other         -2.40 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "handles a correction" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-banana.txt"
        let expectedTr =
              [transactionQQ|
                2023/07/28 * Coop
                  ! Assets:Liquid:BCGE CC  -14.00 CHF
                  Expenses:Groceries        14.00 CHF|]
        tr <- assertRight $ Coop.receiptToLedger Coop.emptyConfig coop
        tr `shouldBe` expectedTr

      it "correctlyCountsAUnnotatedRabatt" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-spargel-rabatt.txt"
        let expectedTr =
              [transactionQQ|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE           -9.90 CHF
                  Expenses:Groceries             11.90 CHF
                  Expenses:Other                 -2.00 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyRecognizesMyCC" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-card.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE CC        -14.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly handles multiple payment methods with supercash" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-supercash.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    Expenses:Other                -7.00 CHF
                    ! Assets:Liquid:BCGE CC         -7.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly handles multiple payment methods with super points" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-superpunkte.txt"
        let expectedTr =
              [transactionQQ|
                  2022/04/16 * Coop
                    Expenses:Other                -0.50 CHF
                    ! Assets:Liquid:BCGE CC         -1.00 CHF
                    Expenses:Groceries:Chewing Gum  1.50 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly applies debtor postings" $ do
        config <-
          readCoopConfig
            [trimming|
                    {
                      "shared": [
                        {"product": "Stimorol",   "debtors": ["John Doe"]}
                      ]
                    }
                |]
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-debt.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE CC  -4.40 CHF
                    Expenses:Groceries  2.90 CHF
                    Expenses:Groceries:Chewing Gum  0.75 CHF
                    ! Assets:Debts:John Doe           0.75 CHF|]
        Coop.receiptToLedger config coop `shouldBe` Right expectedTr

      it "correctly assigns account number for a card" $ do
        config <-
          readCoopConfig
            [trimming|
              { "paymentCards": [
                {"lastFourDigits": "1123",
                 "account": "Assets:Liquid:Foo"}]}|]
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-bcge-debit.txt"
        let expectedTr =
              [transactionQQ|
                  2022/12/08 * Coop
                    ! Assets:Liquid:Foo  -3.75 CHF
                    Expenses:Groceries  3.75 CHF|]
        Coop.receiptToLedger config coop `shouldBe` Right expectedTr
