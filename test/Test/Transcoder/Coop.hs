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
      it "converts a receipt to a transaction" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE  -6.10 CHF
                    Expenses:Groceries  4.50 CHF ; Karma Sweet Potato Fries TK 450g
                    Expenses:Groceries:Chewing Gum  1.60 CHF ; Stimorol Peppermint 14g|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly assigns to sorted categories" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-cat.txt"
        let expectedTr =
              [transactionQQ|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -0.07 CHF
                  Expenses:Gesundheit             0.01 CHF ; Listerine Cool Mint milder Geschmack 500
                  Expenses:Gesundheit             0.01 CHF ; Nivea Sun Protect&Dry Touch LSF50 200ML
                  Expenses:Groceries:Chewing Gum  0.01 CHF ; Stimorol Peppermint 14g
                  Expenses:Groceries:Ready Meals  0.01 CHF ; Findus Egli Knusperli MSC TK 230g
                  Expenses:Groceries:Ready Meals  0.01 CHF ; ZENBU Nigiri & Maki 218G
                  Expenses:Groceries:Ready Meals  0.01 CHF ; ZENBU Salmon Poké 270G
                  Expenses:Household              0.01 CHF ; Brita Kartusche Maxtra+ 3St
                |]

        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly handles a rabatt" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-rabatt.txt"
        let expectedTr =
              [transactionQQ|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -15.35 CHF
                  Expenses:Groceries      2.95 CHF ; Lindt Excellence 90% Cacao 100G
                  Expenses:Groceries      2.95 CHF ; Lindt Excellence 90% Cacao 100G
                  Expenses:Groceries      2.95 CHF ; Lindt Excellence 90% Cacao 100G
                  Expenses:Groceries      2.95 CHF ; Lindt Excellence 90% Cacao 100G
                  Expenses:Groceries     -2.40 CHF ; Rabatt Lindt
                  Expenses:Household      5.95 CHF ; Dettol Desinfektionstücher 60Stk.
                |]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "handles a correction" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-banana.txt"
        let expectedTr =
              [transactionQQ|
                2023/07/28 * Coop
                  ! Assets:Liquid:BCGE CC  -14.00 CHF
                  Expenses:Groceries         4.25 CHF ; Betty Bossi Wrap Chicken Pesto 210G 1.0
                  Expenses:Groceries         2.30 CHF ; Max Havelaar Bananen offen PLU 100
                  Expenses:Groceries        14.95 CHF ; Naturaplan Bio Sal-mon Poké Bowl 320G
                  Expenses:Groceries       -14.95 CHF ; Naturaplan Bio Sal-mon Poké Bowl 320G
                  Expenses:Groceries         7.45 CHF ; Naturaplan Bio Sal-mon Poké Bowl 320G 1.0
                |]

        tr <- assertRight $ Coop.receiptToLedger Coop.emptyConfig coop
        tr `shouldBe` expectedTr

      it "correctly counts an unannotated rabatt" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-spargel-rabatt.txt"
        let expectedTr =
              [transactionQQ|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE           -9.90 CHF
                  Expenses:Groceries              5.95 CHF ; Rana Girasoli Spargel 250G
                  Expenses:Groceries              5.95 CHF ; Rana Girasoli Spargel 250G
                  Expenses:Groceries             -2.00 CHF ; Rana Girasoli Spargel 250G
                |]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly recognizes my credit card" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-card.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE CC        -14.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF ; ZENBU Nigiri & Maki 218G
              |]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly handles multiple payment methods with supercash" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-supercash.txt"
        let expectedTr =
              [transactionQQ|
                  2021/04/09 * Coop
                    Expenses:Other                -7.00 CHF
                    ! Assets:Liquid:BCGE CC         -7.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF ; ZENBU Nigiri & Maki 218G
              |]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly handles multiple payment methods with super points" $ do
        coop <- decodeUtf8 <$> BS.readFile "test/data/coop-superpunkte.txt"
        let expectedTr =
              [transactionQQ|
                  2022/04/16 * Coop
                    Expenses:Other                -0.50 CHF
                    ! Assets:Liquid:BCGE CC         -1.00 CHF
                    Expenses:Groceries:Chewing Gum  1.50 CHF ; Stimorol Peppermint 14g
                |]
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
                    Expenses:Groceries  1.10 CHF ; Bio Landbrötli 90G
                    Expenses:Groceries  1.80 CHF ; Naturaplan Bio Vollmilch UHT 1lt.
                    Expenses:Groceries:Chewing Gum  0.75 CHF ; Stimorol Peppermint 14g
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
                    Expenses:Groceries  3.75 CHF ; Die Butter Mödeli 250G|]
        Coop.receiptToLedger config coop `shouldBe` Right expectedTr
