module Test.Data.Time.Calendar.Extra (tests) where

import Data.Time.Calendar.Extra (monthP)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec (parseMaybe)
import Prelude

tests :: Spec
tests = do
  describe "Data.Time.Calendar.Extra" $ do
    describe "monthP" $ do
      it "Parses July" $ do
        parseMaybe @Void monthP "July" `shouldBe` Just 7
