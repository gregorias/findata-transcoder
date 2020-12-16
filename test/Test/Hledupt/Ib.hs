{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib
  ( tests,
  )
where

import Data.List (isInfixOf)
import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Ib
import qualified Hledupt.Ib.Csv as IbCsv
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Ib" $ do
    parseTests

parseTests :: Hspec.SpecWith ()
parseTests = do
  describe "statementToIbData" $ do
    it "Translates dividends into transactions" $ do
      let stmt =
            IbCsv.Statement
              (fromGregorian 2020 12 8)
              []
              []
              [ IbCsv.Dividend
                  (fromGregorian 2020 1 1)
                  "VOO"
                  (fromRational $ 45 % 100)
                  (fromRational 450)
              ]
              []
      statementToIbData stmt
        `shouldBe` Right
          ( IbData
              { idStockPrices = [],
                idTransactions =
                  [ parseTransactionUnsafe
                      "2020/01/01 * VOO dividend @ 0.45 per share\n\
                      \  Assets:Liquid:IB:USD\n\
                      \  Assets:Illiquid:IB Withholding Tax  USD 0\n\
                      \  Income:Capital Gains  USD -450"
                  ],
                idStatus = Nothing
              }
          )

    it "Returns a meaningful error on unmatched taxes." $ do
      let stmt =
            IbCsv.Statement
              { IbCsv.sLastStatementDay = fromGregorian 2020 12 8,
                IbCsv.sPositions = [],
                IbCsv.sCashMovements = [],
                IbCsv.sDividends = [],
                IbCsv.sWithholdingTaxes =
                  [ IbCsv.WithholdingTax
                      { IbCsv.wtDate = fromGregorian 2020 1 1,
                        IbCsv.wtSymbol = "VOO",
                        IbCsv.wtTotalAmount = fromRational 1
                      }
                  ]
              }
      statementToIbData stmt
        `shouldSatisfy` \case
          Left errorMsg ->
            ( "Could not find a dividend match for VOO withholding tax \
              \from 2020-01-01. This could happen due to IB statement \
              \cut-off (fetch a broader statement) or wrong data \
              \assumptions."
                `isInfixOf` errorMsg
            )
          Right _ -> False

  describe "parse" $ do
    it "Parses a CSV" $ do
      let csv =
            "Statement,Header,Field Name,Field Value\n\
            \Statement,Data,BrokerName,Interactive Brokers\n\
            \Statement,Data,BrokerAddress,\n\
            \Statement,Data,Title,MTM Summary\n\
            \Statement,Data,Period,\"November 26, 2020\"\n\
            \Statement,Data,WhenGenerated,\"2020-11-28, 05:24:15 EST\"\n\
            \Account Information,Header,Field Name,Field Value\n\
            \Account Information,Data,Name,John Doe\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n\
            \Positions and Mark-to-Market Profit and Loss,Data,Stocks,USD,ACWF,ISHARES MSCI GLOBAL MULTIFAC,123,123,32.24,32.24,1001.01,1001.01,0,0,0,0,0\n\
            \Positions and Mark-to-Market Profit and Loss,Data,Total,USD,,,,,,,123,123,0,0,0,0,0\n\
            \Positions and Mark-to-Market Profit and Loss,Data,Forex,CHF,CHF, ,100.0011305,100.0011305,1,1,100.0011305,100.0011305,0,0,0,0,0\n\
            \Deposits & Withdrawals,Header,Currency,Settle Date,Description,Amount\n\
            \Deposits & Withdrawals,Data,CHF,2020-01-20,title,100.32\n\
            \Dividends,Header,Currency,Date,Description,Amount\n\
            \Dividends,Data,USD,2020-10-02,VOO(US9229083632) Cash Dividend USD 1.3085 per Share (Ordinary Dividend),1.31\n\
            \Dividends,Data,Total,,,1.31\n\
            \Withholding Tax,Header,Currency,Date,Description,Amount,Code\n\
            \Withholding Tax,Data,USD,2020-10-02,VOO(US9229083632) Cash Dividend USD 1.3085 per Share - US Tax,-0.25,\n\
            \Withholding Tax,Data,Total,,,-0.25,\n"
      parseCsv csv
        `shouldBe` Right
          ( IbData
              [ MarketPrice
                  (fromGregorian 2020 11 26)
                  "ACWF"
                  "USD"
                  (fromRational $ 3224 % 100)
              ]
              [ parseTransactionUnsafe
                  "2020/01/20 IB Deposit/Withdrawal\n\
                  \*  Assets:Liquid:IB:CHF  CHF 100.32\n\
                  \!  Todo",
                parseTransactionUnsafe
                  "2020/10/02 * VOO dividend @ 1.3085 per share\n\
                  \  Assets:Liquid:IB:USD\n\
                  \  Assets:Illiquid:IB Withholding Tax  USD 0.25\n\
                  \  Income:Capital Gains  USD -1.31"
              ]
              ( Just $
                  parseTransactionUnsafe
                    "2020/11/26 * IB Status\n\
                    \  Assets:Investments:IB:ACWF  0 ACWF = ACWF 123\n\
                    \  Assets:Liquid:IB:CHF  CHF 0 = CHF 100.0011305"
              )
          )

    it "Doesn't return much when there's no data" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"December 11, 2019 - December 4, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
      parseCsv csv
        `shouldBe` Right
          (IbData [] [] Nothing)

  describe "showIbData" $ do
    it "Formats IbData" $ do
      showIbData
        ( IbData
            [ MarketPrice
                (fromGregorian 2020 11 26)
                "ACWF"
                "USD"
                (fromRational $ 3224 % 100)
            ]
            [ parseTransactionUnsafe
                "2020/01/20 IB Deposit/Withdrawal\n\
                \*  Assets:Liquid:IB:CHF  CHF 100.32\n\
                \!  Todo"
            ]
            ( Just $
                parseTransactionUnsafe
                  "2020/11/26 IB Status\n\
                  \  Assets:Investments:IB:ACWF  0 ACWF = ACWF 123\n\
                  \  Assets:Liquid:IB:CHF  CHF 0 = CHF 100.0011305"
            )
        )
        `shouldBe` "2020-01-20 IB Deposit/Withdrawal\n\
                   \    * Assets:Liquid:IB:CHF    CHF 100.32\n\
                   \    ! Todo\n\
                   \\n\
                   \P 2020-11-26 ACWF 32.24 USD\n\
                   \\n\
                   \2020-11-26 IB Status\n\
                   \    Assets:Investments:IB:ACWF               0 = ACWF 123\n\
                   \    Assets:Liquid:IB:CHF                     0 = CHF 100.00\n\n"
