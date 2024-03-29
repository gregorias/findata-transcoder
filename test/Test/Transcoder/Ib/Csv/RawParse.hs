module Test.Transcoder.Ib.Csv.RawParse (tests) where

import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Transcoder.Ib.Csv.RawParse (
  Section (..),
  Statement (..),
  parse,
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.Ib.Csv.RawParse" $ do
    it "Prints a readable message when lines can't be parsed." $ do
      parse "mangled Line" `shouldSatisfy` \case
        Left errorMsg -> ("Could not parse IB CSV statement:\n" `T.isInfixOf` errorMsg)
        Right _ -> False
    it "When a section doesn't start with a header, fails and gives an error message" $ do
      parse "Sec,Data,1" `shouldSatisfy` \case
        Left errorMsg -> ("Could not parse the IB CSV statement.\n" `T.isInfixOf` errorMsg)
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
                [ ("SecA", Section $ "Val\n1\n" :| ["Val,Amt\n"])
                , ("SecB", Section $ "Amt\n42\n" :| [])
                ]
          )
