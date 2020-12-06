{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib
  ( tests,
  )
where

import qualified Data.Csv as Csv
import Data.Either (isRight)
import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Ib
import qualified Hledupt.Ib.Csv as IbCsv
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Ib" $ do
    parseTests

parseTests :: Hspec.SpecWith ()
parseTests = do
  describe "parse" $ do
    it "parses a CSV" $ do
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
            \Deposits & Withdrawals,Data,CHF,2020-01-20,title,100.32"
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
                  \!  Todo"
              ]
              ( Just $
                  parseTransactionUnsafe
                    "2020/11/26 * IB Status\n\
                    \  Assets:Investments:IB:ACWF  0 ACWF = ACWF 123\n\
                    \  Assets:Liquid:IB:CHF  CHF 0 = CHF 100.0011305"
              )
          )

    it "parses the BOM character" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"November 26, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
      isRight (IbCsv.parse csv)

    it "parses a range date correctly" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"December 11, 2019 - December 4, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
      (IbCsv.sLastStatementDay <$> IbCsv.parse csv)
        `shouldBe` Right (fromGregorian 2020 12 4)

    it "Doesn't return much when there's no data" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"December 11, 2019 - December 4, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
      parseCsv csv
        `shouldBe` Right
          (IbData [] [] Nothing)

    it "Parses cash movements" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"December 11, 2019 - December 4, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n\
            \Deposits & Withdrawals,Header,Currency,Settle Date,Description,Amount\n\
            \Deposits & Withdrawals,Data,CHF,2020-01-20,title,100.32\n\
            \Deposits & Withdrawals,Data,Total,,,100.32"
      IbCsv.parse csv
        `shouldBe` Right
          ( IbCsv.Statement
              (fromGregorian 2020 12 4)
              []
              [ IbCsv.CashMovement
                  (fromGregorian 2020 1 20)
                  IbCsv.CHF
                  (fromRational (10032 % 100))
              ]
          )

  describe "CashMovement" $ do
    it "Parses CashMovement Lines" $ do
      let csv =
            "Header,Currency,Settle Date,Description,Amount\n\
            \Data,CHF,2020-01-20,title,100.32"
      fmap snd (Csv.decodeByName csv)
        `shouldBe` Right
          [ IbCsv.CashMovement
              (fromGregorian 2020 1 20)
              IbCsv.CHF
              (fromRational (10032 % 100))
          ]

  describe "showIbData" $ do
    it "formats IbData" $ do
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
