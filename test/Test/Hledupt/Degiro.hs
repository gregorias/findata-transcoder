{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Degiro
  ( tests,
  )
where

import Data.List (isInfixOf)
import Data.Time (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Hledupt.Data.Cash (Cash (Cash))
import Hledupt.Data.Currency (Currency (..))
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Hledupt.Degiro (csvRecordsToLedger)
import Hledupt.Degiro.Csv
  ( DegiroCsvRecord (..),
    mkIsin,
  )
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Degiro" $ do
    describe "csvRecordsToLedger" $ do
      it "Returns an empty report for an empty CSV" $ do
        csvRecordsToLedger [] `shouldBe` Right (LedgerReport [] [])

      it "Filters out money market ops" $ do
        Just nlIsin <- return $ mkIsin "NL0011280581"
        csvRecordsToLedger
          [ DegiroCsvRecord
              (fromGregorian 2020 9 2)
              (TimeOfDay 12 02 0)
              (fromGregorian 2020 9 1)
              "FUNDSHARE UCITS CHF CASH FUND"
              (Just nlIsin)
              "Money Market fund conversion: Sell 123.5678 at 0.981 CHF"
              Nothing
              Nothing
              (Cash CHF 131.72)
              ""
          ]
          `shouldBe` Right (LedgerReport [] [])

      it "Returns a readable error when a record can't be processed." $ do
        csvRecordsToLedger
          [ DegiroCsvRecord
              (fromGregorian 2020 9 2)
              (TimeOfDay 12 02 0)
              (fromGregorian 2020 9 1)
              "Bogus commodity"
              Nothing
              "Bogus description"
              Nothing
              Nothing
              (Cash CHF 0)
              ""
          ]
          `shouldSatisfy` \case
            Right _ -> False
            Left errMsg ->
              ( "Hledupt.Degiro.csvRecordsToLedger could not process all elements.\n"
                  `isInfixOf` errMsg
                    && "One remaining row's description: Bogus description"
                  `isInfixOf` errMsg
              )
