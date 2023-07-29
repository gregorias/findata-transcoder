module Test.Transcoder.EasyRide (
  tests,
) where

import Data.ByteString qualified as BS
import Data.Text.Encoding (decodeUtf8)
import Hledger.Read.TestUtils (transactionQQ)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.EasyRide qualified as EasyRide
import Prelude

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.EasyRide" $ do
    describe "receiptToLedger" $ do
      it "converts a transaction (01)" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/easyride-01.txt"
        let expectedTr =
              [transactionQQ|
                  2021/07/11 * EasyRide
                    ! Assets:Liquid:BCGE CC  -2.30 CHF
                    Expenses:Transport        2.30 CHF
                    |]
        EasyRide.receiptToLedger receipt `shouldBe` Right expectedTr
      it "converts a transaction (02)" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/easyride-02.txt"
        let expectedTr =
              [transactionQQ|
                  2022/09/04 * EasyRide
                    ! Assets:Liquid:BCGE CC  -10.50 CHF
                    Expenses:Transport        10.50 CHF
                    |]
        EasyRide.receiptToLedger receipt `shouldBe` Right expectedTr
      it "converts a transaction (03)" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/easyride-03.txt"
        let expectedTr =
              [transactionQQ|
                  2023/01/01 * EasyRide
                    ! Assets:Liquid:BCGE CC  -5.20 CHF
                    Expenses:Transport        5.20 CHF
                    |]
        EasyRide.receiptToLedger receipt `shouldBe` Right expectedTr
