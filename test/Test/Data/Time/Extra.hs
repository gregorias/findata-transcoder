module Test.Data.Time.Extra (tests) where

import Data.Time (fromGregorian)
import Data.Time.Extra (dayP)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec (parseMaybe)
import Prelude

tests :: Spec
tests = do
  describe "Data.Time.Extra" $ do
    describe "dayP" $ do
      it "Parses 2022-05-28" $ do
        parseMaybe (dayP @Void "%Y-%m-%d") "2022-05-28"
          `shouldBe` Just (fromGregorian 2022 5 28)
      it "Parses 05/28/2022" $ do
        parseMaybe (dayP @Void "%D") "05/28/22"
          `shouldBe` Just (fromGregorian 2022 5 28)
