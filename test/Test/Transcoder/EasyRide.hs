module Test.Transcoder.EasyRide (
  tests,
) where

import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import NeatInterpolation (trimming)
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Transcoder.EasyRide as EasyRide
import Prelude

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.EasyRide" $ do
    describe "receiptToLedger" $ do
      it "converts a transaction (01)" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/easyride-01.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/07/11 * EasyRide
                    ! Assets:Liquid:BCGE CC  -2.30 CHF
                    Expenses:Transport        2.30 CHF
                    |]
        EasyRide.receiptToLedger receipt `shouldBe` Right expectedTr
      it "converts a transaction (02)" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/easyride-02.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2022/09/04 * EasyRide
                    ! Assets:Liquid:BCGE CC  -10.50 CHF
                    Expenses:Transport        10.50 CHF
                    |]
        EasyRide.receiptToLedger receipt `shouldBe` Right expectedTr
