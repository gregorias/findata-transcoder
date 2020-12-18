{-# LANGUAGE OverloadedStrings #-}

module Test.Hledger.Data.MarketPrice.Extra (tests) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Data.MarketPrice.Extra
import Relude
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledger.Data.MarketPrice.Extra tests" $ do
    it "Correctly displays a market price" $ do
      showMarketPrice
        ( MarketPrice
            (fromGregorian 2020 11 26)
            "ACWF"
            "USD"
            (fromRational $ 3224 % 100)
        )
        `shouldBe` "P 2020-11-26 ACWF 32.24 USD\n"
