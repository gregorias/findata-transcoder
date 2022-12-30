module Test.Transcoder.Galaxus (
  tests,
) where

import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import Hledger.Read.TestUtils (transactionQQ)
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Transcoder.Galaxus as Galaxus
import Prelude

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Galaxus" $ do
    describe "parseReceipt" $ do
      it "converts a receipt to a transaction" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/galaxus0.txt"
        let expectedTr =
              [transactionQQ|
                2022/02/09 * Digitec/Galaxus
                  ! Assets:Liquid:BCGE  -769.40 CHF
                  ! Todo                 729.00 CHF ; Samsung Tab S8 (11 ", 128 GB, Graphite)
                  ! Todo                  31.40 CHF ; Onyx Eingabestift Boox Pen Plus Blau
                  ! Todo                   9.00 CHF ; Mindermengenzuschlag
                  |]
        Galaxus.parseReceipt receipt `shouldBe` Right expectedTr

      it "converts a receipt with carriage returns to a transaction" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/galaxus1.txt"
        let expectedTr =
              [transactionQQ|
                2022/12/16 * Digitec/Galaxus
                  ! Assets:Liquid:BCGE CC  -202.00 CHF
                  ! Todo                     43.00 CHF ; Miocar Motorrad Abdeckung L
                  ! Todo                    159.00 CHF ; Swaytronic All in One Jump Starter 2.0 mit Autobahnvignette 2023 (18000 mAh, 600 A)|]
        Galaxus.parseReceipt receipt `shouldBe` Right expectedTr
