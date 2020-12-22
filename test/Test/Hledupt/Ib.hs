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
import Hledupt.Ib.Csv (ActivityStatement (asDividends), nullActivityStatement)
import qualified Hledupt.Ib.Csv as IbCsv
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Ib" $ do
    parseTests

parseTests :: Hspec.SpecWith ()
parseTests = do
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

  describe "statementActivityToIbData" $ do
    it "Translates dividends into transactions" $ do
      let stmt =
            (nullActivityStatement (fromGregorian 2020 12 8))
              { asDividends =
                  [ IbCsv.Dividend
                      (fromGregorian 2020 1 1)
                      "VOO"
                      (fromRational $ 45 % 100)
                      (fromRational 450)
                  ]
              }
      statementActivityToIbData stmt
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
            (IbCsv.nullActivityStatement (fromGregorian 2020 12 8))
              { IbCsv.asTaxes =
                  [ IbCsv.WithholdingTax
                      { IbCsv.wtDate = fromGregorian 2020 1 1,
                        IbCsv.wtSymbol = "VOO",
                        IbCsv.wtTotalAmount = fromRational 1
                      }
                  ]
              }
      statementActivityToIbData stmt
        `shouldSatisfy` \case
          Left errorMsg ->
            ( "Could not find a dividend match for VOO withholding tax \
              \from 2020-01-01. This could happen due to IB statement \
              \cut-off (fetch a broader statement) or wrong data \
              \assumptions."
                `isInfixOf` errorMsg
            )
          Right _ -> False

  describe "parseActivityCsv" $ do
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
            \Net Asset Value,Header,Asset Class,Prior Total,Current Long,Current Short,Current Total,Change\n\
            \Net Asset Value,Data,Cash ,1,1,0,1,1\n\
            \Net Asset Value,Data,Stock,1,1,0,1,1\n\
            \Net Asset Value,Data,Total,1,1,0,1,1\n\
            \Change in NAV,Header,Field Name,Field Value\n\
            \Change in NAV,Data,Starting Value,1\n\
            \Change in NAV,Data,Mark-to-Market,1\n\
            \Change in NAV,Data,Dividends,1\n\
            \Change in NAV,Data,Withholding Tax,-1\n\
            \Change in NAV,Data,Change in Dividend Accruals,-1\n\
            \Change in NAV,Data,Commissions,-1.58930478\n\
            \Change in NAV,Data,Other FX Translations,1\n\
            \Change in NAV,Data,Ending Value,1\n\
            \Cash Report,Header,Currency Summary,Currency,Total,Securities,Futures,Month to Date,Year to Date,\n\
            \Cash Report,Data,Starting Cash,Base Currency Summary,1.0,1.0,0,,,\n\
            \Cash Report,Data,Commissions,Base Currency Summary,-1.0,-1.0,0,0,-1.0,\n\
            \Cash Report,Data,Deposits,Base Currency Summary,0,0,0,0,1.0,\n\
            \Cash Report,Data,Withdrawals,Base Currency Summary,0,0,0,0,-1.0,\n\
            \Cash Report,Data,Dividends,Base Currency Summary,1.0,1.0,0,0,1.0,\n\
            \Cash Report,Data,Broker Interest Paid and Received,Base Currency Summary,0,0,0,0,1.0,\n\
            \Cash Report,Data,Net Trades (Sales),Base Currency Summary,0,0,0,0,1.0,\n\
            \Cash Report,Data,Net Trades (Purchase),Base Currency Summary,-1.0,-1.0,0,0,-1.0,\n\
            \Cash Report,Data,Withholding Tax,Base Currency Summary,-1.0,-1.0,0,0,-1.0,\n\
            \Cash Report,Data,Cash FX Translation Gain/Loss,Base Currency Summary,-1.0,-1.0,0,,,\n\
            \Cash Report,Data,Ending Cash,Base Currency Summary,1.0,1.0,0,,,\n\
            \Cash Report,Data,Ending Settled Cash,Base Currency Summary,1.0,1.0,0,,,\n\
            \Cash Report,Data,Starting Cash,CHF,1.0,1.0,0,,,\n\
            \Cash Report,Data,Commissions,CHF,0,0,0,0,-1.0,\n\
            \Cash Report,Data,Withdrawals,CHF,0,0,0,0,-1.0,\n\
            \Cash Report,Data,Net Trades (Sales),CHF,0,0,0,0,1.0,\n\
            \Cash Report,Data,Ending Cash,CHF,123.4,1.0,0,,,\n\
            \Cash Report,Data,Ending Settled Cash,CHF,1.0,1.0,0,,,\n\
            \Cash Report,Data,Starting Cash,USD,1.0,1.0,0,,,\n\
            \Cash Report,Data,Commissions,USD,-1.0,-1.0,0,0,-1.0,\n\
            \Cash Report,Data,Deposits,USD,0,0,0,0,1.0,\n\
            \Cash Report,Data,Dividends,USD,1.0,1.0,0,0,1.0,\n\
            \Cash Report,Data,Broker Interest Paid and Received,USD,0,0,0,0,1.0,\n\
            \Cash Report,Data,Net Trades (Purchase),USD,-1.0,-1.0,0,0,-1.0,\n\
            \Cash Report,Data,Withholding Tax,USD,-1.0,-1.0,0,0,-1.0,\n\
            \Cash Report,Data,Ending Cash,USD,567.8,1.0,0,,,\n\
            \Cash Report,Data,Ending Settled Cash,USD,1.0,1.0,0,,,\n\
            \Open Positions,Header,DataDiscriminator,Asset Category,Currency,Symbol,Quantity,Mult,Cost Price,Cost Basis,Close Price,Value,Unrealized P/L,Unrealized P/L %,Code\n\
            \Open Positions,Data,Summary,Stocks,USD,VOO,2,1,1.0,123.45,123.45,1.0,1.0,1.0,\n\
            \Open Positions,Total,,Stocks,USD,,,,,1.0,,1.0,1.0,,\n\
            \Open Positions,Total,,Stocks,CHF,,,,,1.0,,1.0,1.0,,\n\
            \Trades,Header,DataDiscriminator,Asset Category,Currency,Symbol,Date/Time,Quantity,T. Price,C. Price,Proceeds,Comm/Fee,Basis,Realized P/L,Realized P/L %,MTM P/L,Code\n\
            \Trades,Data,Order,Stocks,USD,VOO,\"2020-10-05, 09:52:53\",2,1.0,1.0,-1.0,-0.5,1.0,0,0,1.0,O;R\n\
            \Trades,SubTotal,,Stocks,USD,VOO,,2,,,-1.0,-0.5,1.0,0,1.0,1.0,\n\
            \Trades,Total,,Stocks,USD,,,,,,-1.0,-1.0,1.0,0,,1.0,\n\
            \Trades,Total,,Stocks,CHF,,,,,,-1.0,-1.0,1.0,0,,1.0,\n\
            \Trades,Header,DataDiscriminator,Asset Category,Currency,Symbol,Date/Time,Quantity,T. Price,,Proceeds,Comm in CHF,,,,MTM in CHF,Code\n\
            \Trades,Data,Order,Forex,CHF,USD.CHF,\"2020-01-15, 14:33:56\",\"-2,200,000\",0.96358,,2119800.76,-1.93502,,,,-11,\n\
            \Trades,SubTotal,,Forex,CHF,USD.CHF,,-2200000,,,2000000.76,-1.93502,,,,-11,\n\
            \Trades,Total,,Forex,CHF,,,,,,1.0,-1,,,,-1,\n\
            \Deposits & Withdrawals,Header,Currency,Settle Date,Description,Amount\n\
            \Deposits & Withdrawals,Data,CHF,2020-01-20,title,100.32\n\
            \Dividends,Header,Currency,Date,Description,Amount\n\
            \Dividends,Data,USD,2020-10-02,VOO(US9229083632) Cash Dividend USD 1.3085 per Share (Ordinary Dividend),1.31\n\
            \Dividends,Data,Total,,,1.31\n\
            \Withholding Tax,Header,Currency,Date,Description,Amount,Code\n\
            \Withholding Tax,Data,USD,2020-10-02,VOO(US9229083632) Cash Dividend USD 1.3085 per Share - US Tax,-0.25,\n\
            \Withholding Tax,Data,Total,,,-0.25,\n"
      parseActivityCsv csv
        `shouldBe` Right
          ( IbData
              [ MarketPrice
                  (fromGregorian 2020 11 26)
                  "VOO"
                  "USD"
                  (fromRational $ 12345 % 100)
              ]
              [ parseTransactionUnsafe
                  "2020/01/15 * USD.CHF\n\
                  \ Assets:Liquid:IB:USD  USD -2200000.00 @ 0.96358 CHF\n\
                  \ Assets:Liquid:IB:CHF  CHF 2119800.76\n\
                  \ Assets:Liquid:IB:CHF  CHF -1.93502\n\
                  \ Expenses:Financial Services  CHF 1.93502",
                parseTransactionUnsafe
                  "2020/01/20 IB Deposit/Withdrawal\n\
                  \*  Assets:Liquid:IB:CHF  CHF 100.32\n\
                  \!  Todo",
                parseTransactionUnsafe
                  "2020/10/02 * VOO dividend @ 1.3085 per share\n\
                  \  Assets:Liquid:IB:USD\n\
                  \  Assets:Illiquid:IB Withholding Tax  USD 0.25\n\
                  \  Income:Capital Gains  USD -1.31",
                parseTransactionUnsafe
                  "2020/10/05 * VOO trade\n\
                  \  Assets:Investments:IB:VOO  2 VOO\n\
                  \  Assets:Liquid:IB:USD  -1 USD\n\
                  \  Expenses:Financial Services  0.5 USD"
              ]
              ( Just $
                  parseTransactionUnsafe
                    "2020/11/26 * IB Status\n\
                    \  Assets:Liquid:IB:CHF  CHF 0 = CHF 123.40\n\
                    \  Assets:Liquid:IB:USD  USD 0 = USD 567.80\n\
                    \  Assets:Investments:IB:VOO  0 VOO = VOO 2"
              )
          )

    it "Doesn't return much when there's no data" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"December 11, 2019 - December 4, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
      parseActivityCsv csv
        `shouldBe` Right
          (IbData [] [] Nothing)
