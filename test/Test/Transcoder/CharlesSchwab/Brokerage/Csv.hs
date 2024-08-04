module Test.Transcoder.CharlesSchwab.Brokerage.Csv (
  tests,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Time (fromGregorian)
import NeatInterpolation (trimming)
import Relude
import Test.HUnit.Extra (assertRight)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab.Brokerage.Csv (
  BrokerageHistoryCsvRecord (BrokerageHistoryCsvRecord),
  parseBrokerageHistoryCsv,
 )
import Transcoder.CharlesSchwab.DollarAmount (
  DollarAmount (..),
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab.BrokerageCsv" $ do
    describe "parseBrokerageHistoryCsv" $ do
      it "parses a line with decimal quantities" $ do
        let stmt :: LBS.ByteString =
              encodeUtf8
                [trimming|
                "Date","Action","Symbol","Description","Quantity","Price","Fees & Comm","Amount"
                "06/27/2024","Stock Plan Activity","GOOG","ALPHABET INC. CLASS C","40.045","","",""
                "06/27/2024 as of 06/26/2024","Sell","GOOG","ALPHABET INC. CLASS C","40.045","$$185.2337","$$0.22","$$7417.46"|]
        csvRecords <- assertRight $ parseBrokerageHistoryCsv stmt
        csvRecords
          `shouldBe` [ BrokerageHistoryCsvRecord
                        (fromGregorian 2024 6 27)
                        "Stock Plan Activity"
                        "GOOG"
                        "ALPHABET INC. CLASS C"
                        (Just 40.045)
                        Nothing
                        Nothing
                        Nothing
                     , BrokerageHistoryCsvRecord
                        (fromGregorian 2024 6 27)
                        "Sell"
                        "GOOG"
                        "ALPHABET INC. CLASS C"
                        (Just 40.045)
                        (Just $ DollarAmount 185.2337)
                        (Just $ DollarAmount 0.22)
                        (Just $ DollarAmount 7417.46)
                     ]
