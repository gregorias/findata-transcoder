{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Degiro.Csv
  ( tests,
  )
where

import Data.Time (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Hledupt.Data.Currency (Currency (..))
import Hledupt.Degiro.Csv
  ( DegiroCsvRecord (..),
    Money (Money),
    mkIsin,
    parseCsvStatement,
  )
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "parseCsvStatement" $ do
    it "Parses a statement" $ do
      let csv =
            "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
            \18-12-2020,00:00,17-12-2020,FUNDSHARE UCITS CHF CASH FUND,NL0011280581,Money Market fund price change (CHF),,CHF,-0.01,CHF,131.23,\n\
            \02-09-2020,08:24,01-09-2020,,,FX Debit,,CHF,0.01,CHF,131.72,\n\
            \02-09-2020,08:24,01-09-2020,,,FX Debit,0.9241,EUR,-0.01,EUR,-0.00,\n\
            \28-01-2020,14:05,27-01-2020,,,Deposit,,CHF,2500.00,CHF,2520.92,"
      Just nlIsin <- return $ mkIsin "NL0011280581"
      parseCsvStatement csv
        `shouldBe` Right
          [ DegiroCsvRecord
              (fromGregorian 2020 12 18)
              (TimeOfDay 0 0 0)
              (fromGregorian 2020 12 17)
              "FUNDSHARE UCITS CHF CASH FUND"
              (Just nlIsin)
              "Money Market fund price change (CHF)"
              Nothing
              (Money CHF (-0.01))
              (Money CHF 131.23)
              "",
            DegiroCsvRecord
              (fromGregorian 2020 9 2)
              (TimeOfDay 8 24 0)
              (fromGregorian 2020 9 1)
              ""
              Nothing
              "FX Debit"
              Nothing
              (Money CHF 0.01)
              (Money CHF 131.72)
              "",
            DegiroCsvRecord
              (fromGregorian 2020 9 2)
              (TimeOfDay 8 24 0)
              (fromGregorian 2020 9 1)
              ""
              Nothing
              "FX Debit"
              (Just 0.9241)
              (Money EUR (-0.01))
              (Money EUR 0)
              "",
            DegiroCsvRecord
              (fromGregorian 2020 1 28)
              (TimeOfDay 14 5 0)
              (fromGregorian 2020 1 27)
              ""
              Nothing
              "Deposit"
              Nothing
              (Money CHF 2500)
              (Money CHF 2520.92)
              ""
          ]
