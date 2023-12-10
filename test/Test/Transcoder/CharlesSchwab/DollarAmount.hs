module Test.Transcoder.CharlesSchwab.DollarAmount (
  tests,
) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Relude
import Test.HUnit.Extra (assertLeft, textShouldContain)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab.DollarAmount (
  DollarAmount (..),
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab.DollarAmount" $ do
    describe "FromJSON" $ do
      it "parses a negative dollar amount" $ do
        Aeson.decode @DollarAmount "\"-$2000000\""
          `shouldBe` Just (DollarAmount (-2000000))

      it "parses \"$0.18\"" $ do
        Aeson.decode @DollarAmount "\"$0.18\""
          `shouldBe` Just (DollarAmount 0.18)

      it "gives an understandable message when parsing fails" $ do
        let eitherDecimal = Aeson.eitherDecode @DollarAmount "\"asdfasdf\""
        message <- assertLeft eitherDecimal
        T.pack message `textShouldContain` "Could not parse the dollar amount"
