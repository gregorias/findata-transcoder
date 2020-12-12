{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Hledupt.Ib.Csv.RawParse (tests) where

import Data.Either (isRight)
import Hledupt.Ib.Csv.RawParse
import Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.RawParse" $ do
    it "Finds dividend and withholding tax sections." $ do
      let csv =
            "Dividends,Header,Currency,Date,Description,Amount\n\
            \Withholding Tax,Header,Currency,Date,Description,Amount,Code"
          stmt = parse csv
      case stmt of
        Left _ -> expectationFailure "Could not parse the CSV"
        Right stmt ->
          ( cDividends stmt,
            cWithholdingTaxes stmt
          )
            `shouldBe` ( "Header,Currency,Date,Description,Amount\n",
                         "Header,Currency,Date,Description,Amount,Code"
                       )
    it "parses the BOM character" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"November 26, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
      parse csv `shouldSatisfy` isRight
