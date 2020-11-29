{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib
  ( tests,
  )
where

import qualified Data.Csv as Csv
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Ratio ((%))
import qualified Data.Vector as V
import qualified Hledupt.Ib as Ib
import Test.Hspec (describe, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Text.Megaparsec as MP

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Ib" $ do
    rawStatementParserTests
    positionCsvParserTests

rawStatementParserTests :: Hspec.SpecWith ()
rawStatementParserTests = do
  describe "rawStatementParser" $ do
    Hspec.it "parses IB statement file" $ do
      let exampleIbCsv =
            "Statement,Header,Field Name,Field Value\n\
            \Statement,Data,BrokerName,Interactive Brokers\n\
            \Statement,Data,WhenGenerated,\"2020-11-28, 05:24:15 EST\"\n\
            \Account Information,Header,Field Name,Field Value\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n\
            \Positions and Mark-to-Market Profit and Loss,Data,Total (All Assets),CHF,,,,,,,123.123,0,0,0,123.123"
          expectedAnswer =
            Ib.IbCsvs
              { Ib.statement =
                  "Header,Field Name,Field Value\n\
                  \Data,BrokerName,Interactive Brokers\n\
                  \Data,WhenGenerated,\"2020-11-28, 05:24:15 EST\"\n",
                Ib.positions =
                  "Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n\
                  \Data,Total (All Assets),CHF,,,,,,,123.123,0,0,0,123.123"
              }
      MP.parseMaybe Ib.rawStatementParser exampleIbCsv `shouldBe` Just expectedAnswer

positionCsvParserTests :: Hspec.SpecWith ()
positionCsvParserTests = do
  describe "Data.Csv.FromNamedRecord PositionRecord" $ do
    Hspec.it "parses a position CSV" $ do
      let exampleCsv =
            "Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n\
            \Data,Stocks,USD,ACWF,ISHARES MSCI,1234,1234,10.00,10.01,12340.00,12352.34,0,0,0,0,0\n\
            \Data,Forex,CHF,CHF, ,100.0011305,100.0011305,1,1,100.0011305,100.0011305,0,0,0,0,0"
      (Csv.decodeByName exampleCsv & fromRight (mempty, mempty) & snd & V.toList)
        `shouldBe` [ Ib.PositionRecord Ib.Stocks Ib.USD "ACWF" "ISHARES MSCI" (fromRational $ 1234 % 1) (fromRational $ 1001 % 100),
                     Ib.PositionRecord Ib.Forex Ib.CHF "CHF" " " (fromRational $ 100 + 11305 % 10000000) (fromRational 1)
                   ]
