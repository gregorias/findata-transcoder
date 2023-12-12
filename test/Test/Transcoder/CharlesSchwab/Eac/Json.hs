{-# LANGUAGE OverloadedLists #-}

module Test.Transcoder.CharlesSchwab.Eac.Json (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Time (fromGregorian)
import NeatInterpolation (trimming)
import Relude hiding (fromStrict)
import Test.HUnit.Extra (
  assertJust,
  assertRight,
 )
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab.DollarAmount (
  DollarAmount (..),
 )
import Transcoder.CharlesSchwab.Eac.Json (
  CsDay (..),
  CsDecimal (..),
  DepositJson (..),
  DepositString (..),
  SaleJson (..),
  SaleString (..),
  WireTransferJson (..),
  WireTransferString (..),
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab.Eac.Json" $ do
    describe "WireTransferJson" $ do
      describe "FromJSON" $ do
        it "parses from an object" $ do
          let json =
                fromStrict
                  $ encodeUtf8
                    [trimming|
                      {
                        "Date": "08/04/2023",
                        "Action": "Wire Transfer",
                        "Symbol": "GOOG",
                        "Quantity": null,
                        "Description": "Cash Disbursement",
                        "FeesAndCommissions": "",
                        "DisbursementElection": null,
                        "Amount": "-$$19,717.70",
                        "TransactionDetails": []
                      }|]
          wireTransfer <- assertJust $ Aeson.decode @WireTransferJson json
          wireTransfer
            `shouldBe` WireTransferJson
              { wtjDate = coerce (fromGregorian 2023 8 4)
              , wtjAction = WireTransferString
              , wtjSymbol = "GOOG"
              , wtjDescription = "Cash Disbursement"
              , wtjAmount = DollarAmount (-19717.7)
              }
  describe "SaleJson" $ do
    describe "FromJSON" $ do
      it "parses from an object" $ do
        let json =
              fromStrict
                $ encodeUtf8
                  [trimming|
                    {
                      "Date": "08/01/2023",
                      "Action": "Sale",
                      "Symbol": "GOOG",
                      "Quantity": "150.806",
                      "Description": "Share Sale",
                      "FeesAndCommissions": "$$0.18",
                      "DisbursementElection": null,
                      "Amount": "$$19,717.70",
                      "TransactionDetails": [
                        {
                          "Details": {
                            "Type": "RS",
                            "Shares": "0.806",
                            "SalePrice": "$$130.75",
                            "SubscriptionDate": "",
                            "SubscriptionFairMarketValue": "",
                            "PurchaseDate": "",
                            "PurchasePrice": "",
                            "PurchaseFairMarketValue": "",
                            "DispositionType": null,
                            "GrantId": "C1115699",
                            "VestDate": "05/25/2023",
                            "VestFairMarketValue": "$$121.64",
                            "GrossProceeds": "$$105.38"
                          }
                        }
                      ]
                    }|]
        sale <- assertRight $ Aeson.eitherDecode @SaleJson json
        sale
          `shouldBe` SaleJson
            { sjDate = coerce (fromGregorian 2023 8 1)
            , sjAction = SaleString
            , sjSymbol = "GOOG"
            , sjQuantity = CsDecimal 150.806
            , sjDescription = "Share Sale"
            , sjFeesAndCommissions = DollarAmount 0.18
            , sjAmount = DollarAmount 19717.7
            }
  describe "SaleJson" $ do
    describe "FromJSON" $ do
      it "parses from an object" $ do
        let json =
              encodeUtf8
                [trimming|
                {
                  "Date": "06/28/2023",
                  "Action": "Deposit",
                  "Symbol": "GOOG",
                  "Quantity": "61.22",
                  "Description": "RS",
                  "FeesAndCommissions": null,
                  "DisbursementElection": null,
                  "Amount": null,
                  "TransactionDetails": [
                    {
                      "Details": {
                        "AwardDate": "01/08/2020",
                        "AwardId": "C637744",
                        "VestDate": "06/25/2023",
                        "VestFairMarketValue": "$$123.02"
                      }
                    }
                  ]
                }
                  |]
        deposit <- assertRight $ Aeson.eitherDecodeStrict @DepositJson json
        deposit
          `shouldBe` DepositJson
            { djDate = coerce (fromGregorian 2023 6 28)
            , djAction = DepositString
            , djSymbol = "GOOG"
            , djQuantity = CsDecimal 61.22
            , djDescription = "RS"
            }
