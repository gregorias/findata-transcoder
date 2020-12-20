{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib.Csv.RawParseNew (tests) where

import Data.List (isInfixOf)
import qualified Data.Map.Lazy as Map
import Hledupt.Ib.Csv.RawParseNew (Section (..), Statement (..), parse)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.RawParseNew" $ do
    it "Prints a readable message when lines can't be parsed." $ do
      parse ("mangled Line" :: String) `shouldSatisfy` \case
        Left errorMsg -> ("Could not parse IB CSV statement into individual structured lines.\n" `isInfixOf` errorMsg)
        Right _ -> False
    it "When a section doesn't start with a header, fails and gives an error message" $ do
      parse ("Sec,Data,1" :: String) `shouldSatisfy` \case
        Left errorMsg -> ("Could not parse the IB CSV statement.\n" `isInfixOf` errorMsg)
        Right _ -> False
    it "Parses a statement" $ do
      let stmt =
            "SecA,Header,Val\n\
            \SecA,Data,1\n\
            \SecA,Total,2\n\
            \SecA,Header,Val,Amt\n\
            \SecA,Total,2,3\n\
            \SecA,Notes,YOLO\n\
            \SecB,Header,Amt\n\
            \SecB,Data,42"
      parse stmt
        `shouldBe` Right
          ( Statement $
              Map.fromList
                [ ("SecA", Section $ "Val\n1\n" :| ["Val,Amt\n"]),
                  ("SecB", Section $ "Amt\n42\n" :| [])
                ]
          )
