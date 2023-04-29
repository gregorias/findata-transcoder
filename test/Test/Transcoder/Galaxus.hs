module Test.Transcoder.Galaxus (
  tests,
) where

import Data.ByteString qualified as BS
import Data.Text.Encoding (decodeUtf8)
import Hledger.Read.TestUtils (transactionQQ)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Galaxus qualified as Galaxus
import Prelude

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Galaxus" $ do
    describe "parseReceipt" $ do
      it "converts a receipt (85231628) to a transaction" $ do
        receipt <- decodeUtf8 <$> BS.readFile "./test/data/Danke für deine Bestellung 85231628.galaxus"
        let expectedTr =
              [transactionQQ|
                2023/04/12 * Digitec/Galaxus
                  ! Assets:Liquid:BCGE CC  -134.06 CHF
                  ! Expenses:Todo                     39.20 CHF ; Burgerstein Omega 3 DHA (100 Stück, Tabletten)
                  ! Expenses:Todo                     47.20 CHF ; Burgerstein Omega-3 EPA (100 Stück, Kapseln)
                  ! Expenses:Todo                     47.30 CHF ; Amazon Schutzhülle (Amazon Kindle Paperwhite (2021))
                  Expenses:Other:CO2        0.36 CHF ; CO2-Kompensation
                  |]
        Galaxus.parseReceipt receipt `shouldBe` Right expectedTr

      it "converts a receipt (85397592) to a transaction" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/Danke für deine Bestellung 85397592.galaxus"
        let expectedTr =
              [transactionQQ|
                2023/04/15 * Digitec/Galaxus
                  ! Assets:Liquid:BCGE CC  -182.04 CHF
                  ! Expenses:Todo                    109.00 CHF ; Keychron K6 (US, Kabelgebunden, Kabellos)
                  ! Expenses:Todo                     69.90 CHF ; Keychron OEM Dye-Sub PBT Set Developer
                  Expenses:Other:CO2          3.14 CHF ; CO2-Kompensation
              |]
        Galaxus.parseReceipt receipt `shouldBe` Right expectedTr

      it "converts a receipt (85830771) to a transaction" $ do
        receipt <- decodeUtf8 <$> BS.readFile "test/data/Danke für deine Bestellung 85830771.galaxus"
        let expectedTr =
              [transactionQQ|
                2023/04/22 * Digitec/Galaxus
                  ! Assets:Liquid:BCGE CC  -110.82 CHF
                  ! Expenses:Todo                     23.10 CHF ; Glorious PC Gaming Race Cleaning Kit
                  ! Expenses:Todo                     84.70 CHF ; Glorious PC Gaming Race Lube Kit
                  Expenses:Other:CO2          3.02 CHF ; CO2-Kompensation
              |]
        Galaxus.parseReceipt receipt `shouldBe` Right expectedTr
