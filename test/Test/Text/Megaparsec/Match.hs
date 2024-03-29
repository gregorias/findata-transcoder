module Test.Text.Megaparsec.Match (tests) where

import Relude
import Test.Hspec
import Text.Megaparsec.Match (matches, (=~))

tests :: SpecWith ()
tests = do
  describe "Text.Megaparsec.Match" $ do
    describe "matches" $ do
      it "Matches" $ do
        ("Oh, hello there" :: Text) `matches` "hello" `shouldBe` True
      it "Does not match" $ do
        ("Oh, hi there" :: Text) =~ "hello" `shouldBe` False
