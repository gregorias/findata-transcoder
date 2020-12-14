{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Hledupt.Ib.Csv.RawParse (tests) where

import Data.Either (isRight)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Hledupt.Ib.Csv.RawParse
import Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.RawParse" $ do
    it "Finds and groups sections." $ do
      let statement =
            "Dividends,Header,Date\n\
            \Dividends,Data,2019-12-08\n\
            \Withholding Tax,Header,Currency\n\
            \Withholding Tax,Data,CHF"
      case parse statement of
        Left _ -> expectationFailure "Could not parse the CSV"
        Right ibCsvs ->
          ibCsvs
            `shouldBe` Map.fromList
              [ ("Dividends", "Header,Date\nData,2019-12-08\n"),
                ("Withholding Tax", "Header,Currency\nData,CHF")
              ]

    it "Parses the BOM character" $ do
      let csv =
            "\65279Statement,Header,Field Name,Field Value\n\
            \Statement,Data,Period,\"November 26, 2020\"\n\
            \Positions and Mark-to-Market Profit and Loss,Header\n"
      parse csv `shouldSatisfy` isRight

    it "Gives a readable error message" $
      case parse "mangledtext" of
        Left errorMsg ->
          errorMsg
            `shouldSatisfy` isInfixOf
              "Could not parse the IB CSV statement."
        Right _ ->
          expectationFailure
            "`parse` should not have parsed the mangled text"
