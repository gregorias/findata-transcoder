{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Finpension (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Hledupt.Finpension (funds, transactionsToLedger)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Finpension" $ do
    describe "transactionsToLedger" $ do
      it "convertsADepositTransaction" $ do
        let input =
              "Date;Category;\"Asset Name\";\"Number of Shares\";\"Asset Currency\";\"Currency Rate\";\"Asset Price in CHF\";\"Cash Flow\";Balance\n\
              \2021-02-15;Deposit;;;CHF;1.000000;;6883.000000;6883.000000"
        transactionsToLedger input
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2021/02/15 * Finpension Deposit\n\
                    \  Assets:Investments:Finpension:Cash  6883 CHF = 6883 CHF\n\
                    \  ! Todo"
                ]
                []
            )
      it "convertsTransactions" $ do
        let input =
              "Date;Category;\"Asset Name\";\"Number of Shares\";\"Asset Currency\";\"Currency Rate\";\"Asset Price in CHF\";\"Cash Flow\";Balance\n\
              \2021-03-01;Buy;\"CSIF (CH) III Equity World ex CH Blue - Pension Fund ZB\";2.503000;CHF;1.000000;1956.760000;-4897.770280;-15.475810\n\
              \2021-03-01;Buy;\"CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund DB\";0.354000;CHF;1.000000;1977.440000;-700.013760;4882.294470\n\
              \2021-03-01;Buy;\"CSIF (CH) Equity Emerging Markets Blue DB\";0.443000;CHF;1.000000;2319.340000;-1027.467620;5582.308230\n\
              \2021-03-01;Buy;\"CSIF (CH) Equity Switzerland Small & Mid Cap ZB\";0.055000;CHF;1.000000;2463.260000;-135.479300;6609.775850\n\
              \2021-03-01;Buy;\"CSIF (CH) Equity Switzerland Large Cap Blue ZB\";0.097000;CHF;1.000000;1420.050000;-137.744850;6745.255150\n\
              \2021-02-15;Deposit;;;CHF;1.000000;;6883.000000;6883.000000"
        transactionsToLedger input
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2021/02/15 * Finpension Deposit\n\
                    \  Assets:Investments:Finpension:Cash  6883 CHF = 6883 CHF\n\
                    \  ! Todo"
                , parseTransactionUnsafe
                    "2021/03/01 * Finpension Purchase\n\
                    \  Assets:Investments:Finpension:Cash  -137.74485 CHF = 6745.25515 CHF\n\
                    \  Assets:Investments:Finpension:CH Large Cap  0.097 \"CH0033782431\" @ 1420.05 CHF"
                , parseTransactionUnsafe
                    "2021/03/01 * Finpension Purchase\n\
                    \  Assets:Investments:Finpension:Cash  -135.4793 CHF = 6609.77585 CHF\n\
                    \  Assets:Investments:Finpension:CH Small & Mid Cap  0.055 \"CH0110869143\" @ 2463.26 CHF"
                , parseTransactionUnsafe
                    "2021/03/01 * Finpension Purchase\n\
                    \  Assets:Investments:Finpension:Cash  -1027.46762 CHF = 5582.30823 CHF\n\
                    \  Assets:Investments:Finpension:Emerging Markets  0.443 \"CH0214967314\" @ 2319.34 CHF"
                , parseTransactionUnsafe
                    "2021/03/01 * Finpension Purchase\n\
                    \  Assets:Investments:Finpension:Cash  -700.01376 CHF = 4882.29447 CHF\n\
                    \  Assets:Investments:Finpension:World ex CH Small Cap  0.354 \"CH0017844686\" @ 1977.44 CHF"
                , parseTransactionUnsafe
                    "2021/03/01 * Finpension Purchase\n\
                    \  Assets:Investments:Finpension:Cash  -4897.77028 CHF = -15.47581 CHF\n\
                    \  Assets:Investments:Finpension:World ex CH  2.503 \"CH0130458182\" @ 1956.76 CHF"
                ]
                []
            )
    describe "funds" $ do
      it "areCorrectlyConstructedAndNotUndefined" $ do
        funds `shouldSatisfy` (not . null)
