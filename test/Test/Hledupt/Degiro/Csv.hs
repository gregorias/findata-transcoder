module Test.Hledupt.Degiro.Csv (
  tests,
) where

import Data.Time (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Hledupt.Data.Cash (Cash (Cash))
import Hledupt.Data.CsvFile (CsvFile (CsvFile))
import Hledupt.Data.Currency (chf, eur)
import Hledupt.Data.Isin (isin)
import Hledupt.Degiro.Csv (
  DegiroCsvRecord (..),
  DegiroIsin (..),
  parseCsvStatement,
 )
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (
  expectationFailure,
  shouldBe,
  shouldContain,
 )

tests :: SpecWith ()
tests = do
  describe "Hledupt.Degiro.Csv" $ do
    describe "parseCsvStatement" $ do
      it "Parses a statement" $ do
        let csv =
              CsvFile . encodeUtf8 $
                [trimming|
              Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID
              10-05-2021,00:00,09-05-2021,,,Money Market fund price change (CHF),,CHF,-0.03,CHF,3.50,
              18-12-2020,00:00,17-12-2020,FUNDSHARE UCITS CHF CASH FUND,NL0011280581,Cash Market fund price change (CHF),,CHF,-0.01,CHF,131.23,
              02-09-2020,12:02,01-09-2020,FUNDSHARE UCITS CHF CASH FUND,NL0011280581,"Cash Market fund conversion: Sell 123.5678 at 0.981 CHF",,,,CHF,131.72,
              02-09-2020,08:24,01-09-2020,,,FX Debit,,CHF,0.01,CHF,131.72,
              02-09-2020,08:24,01-09-2020,,,FX Debit,0.9241,EUR,-0.01,EUR,-0.00,
              28-01-2020,14:05,27-01-2020,,,Deposit,,CHF,2500.00,CHF,2520.92,|]
        let nlIsin = [isin|NL0011280581|]
        parseCsvStatement csv
          `shouldBe` Right
            [ DegiroCsvRecord
                (fromGregorian 2021 5 10)
                (TimeOfDay 0 0 0)
                (fromGregorian 2021 5 9)
                ""
                Nothing
                "Money Market fund price change (CHF)"
                Nothing
                (Just $ Cash chf (-0.03))
                (Cash chf 3.50)
                ""
            , DegiroCsvRecord
                (fromGregorian 2020 12 18)
                (TimeOfDay 0 0 0)
                (fromGregorian 2020 12 17)
                "FUNDSHARE UCITS CHF CASH FUND"
                (Just $ DegiroIsin nlIsin)
                "Cash Market fund price change (CHF)"
                Nothing
                (Just $ Cash chf (-0.01))
                (Cash chf 131.23)
                ""
            , DegiroCsvRecord
                (fromGregorian 2020 9 2)
                (TimeOfDay 12 02 0)
                (fromGregorian 2020 9 1)
                "FUNDSHARE UCITS CHF CASH FUND"
                (Just $ DegiroIsin nlIsin)
                "Cash Market fund conversion: Sell 123.5678 at 0.981 CHF"
                Nothing
                Nothing
                (Cash chf 131.72)
                ""
            , DegiroCsvRecord
                (fromGregorian 2020 9 2)
                (TimeOfDay 8 24 0)
                (fromGregorian 2020 9 1)
                ""
                Nothing
                "FX Debit"
                Nothing
                (Just $ Cash chf 0.01)
                (Cash chf 131.72)
                ""
            , DegiroCsvRecord
                (fromGregorian 2020 9 2)
                (TimeOfDay 8 24 0)
                (fromGregorian 2020 9 1)
                ""
                Nothing
                "FX Debit"
                (Just 0.9241)
                (Just $ Cash eur (-0.01))
                (Cash eur 0)
                ""
            , DegiroCsvRecord
                (fromGregorian 2020 1 28)
                (TimeOfDay 14 5 0)
                (fromGregorian 2020 1 27)
                ""
                Nothing
                "Deposit"
                Nothing
                (Just $ Cash chf 2500)
                (Cash chf 2520.92)
                ""
            ]
      it "Parses an NLFLATEXACNT" $ do
        let csv =
              CsvFile . encodeUtf8 $
                [trimming|
                  Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID
                  07-02-2022,10:17,04-02-2022,FLATEX CHF BANKACCOUNT,NLFLATEXACNT,Degiro Cash Sweep Transfer,,CHF,2.63,CHF,245.29,|]
        parseCsvStatement csv
          `shouldBe` Right
            [ DegiroCsvRecord
                (fromGregorian 2022 2 7)
                (TimeOfDay 10 17 0)
                (fromGregorian 2022 2 4)
                "FLATEX CHF BANKACCOUNT"
                (Just Nlflatexacnt)
                "Degiro Cash Sweep Transfer"
                Nothing
                (Just $ Cash chf 2.63)
                (Cash chf 245.29)
                ""
            ]
      it "Gives an informative error message when the ISIN field is unexpected." $ do
        let csv =
              CsvFile . encodeUtf8 $
                [trimming|
                  Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID
                  07-02-2022,10:17,04-02-2022,FLATEX CHF BANKACCOUNT,WEIRDISIN,Degiro Cash Sweep Transfer,,CHF,2.63,CHF,245.29,|]
        let containsErrMsg (Right _) =
              expectationFailure "Expected a readable error message but got Right."
            containsErrMsg (Left errMsg) =
              toString errMsg `shouldContain` "Expected NLFLATEXACNT or an ISIN, but got: WEIRDISIN."
        containsErrMsg (parseCsvStatement csv)

-- "(Expected NLFLATEXACNT or an ISIN, but got: WEIRDISIN.)"
