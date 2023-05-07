module Test.Data.Time.Extra (tests) where

import Data.Time (fromGregorian)
import Data.Time.Extra (dayP, usFullDayP)
import Data.Void (Void)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Extra (parsePretty)
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
    describe "usFullDayP" $ do
      it "Parses 'May 4, 2023" $ do
        parseMaybe (usFullDayP @Void) "May 4, 2023"
          `shouldBe` Just (fromGregorian 2023 5 4)
      it "Throws an error on 'May 32, 2023" $ do
        let isExpected (Left text) = "Could not parse May 32, 2023 as a valid date." `T.isInfixOf` text
            isExpected (Right _) = False
        parsePretty (usFullDayP @Void) "test" "May 32, 2023"
          `shouldSatisfy` isExpected
