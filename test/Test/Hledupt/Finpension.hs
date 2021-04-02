{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Finpension (
  tests,
) where

import Hledupt.Finpension (transactionsToLedger)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Finpension" $ do
    describe "transactionsToLedger" $ do
      it "convertsTransactions" $ do
        let input =
              "Date;Category;\"Asset Name\";\"Number of Shares\";\"Asset Currency\";\"Currency Rate\";\"Asset Price in CHF\";\"Cash Flow\";Balance\n\
              \2021-03-01;Buy;\"CSIF (CH) III Equity World ex CH Blue - Pension Fund ZB\";2.503000;CHF;1.000000;1956.760000;-4897.770280;-15.475810\n\
              \2021-03-01;Buy;\"CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund DB\";0.354000;CHF;1.000000;1977.440000;-700.013760;4882.294470\n\
              \2021-03-01;Buy;\"CSIF (CH) Equity Emerging Markets Blue DB\";0.443000;CHF;1.000000;2319.340000;-1027.467620;5582.308230\n\
              \2021-03-01;Buy;\"CSIF (CH) Equity Switzerland Small & Mid Cap ZB\";0.055000;CHF;1.000000;2463.260000;-135.479300;6609.775850\n\
              \2021-03-01;Buy;\"CSIF (CH) Equity Switzerland Large Cap Blue ZB\";0.097000;CHF;1.000000;1420.050000;-137.744850;6745.255150\n\
              \2021-02-15;Deposit;;;CHF;1.000000;;6883.000000;6883.000000"
        transactionsToLedger input `shouldBe` Left "unimplemented"
