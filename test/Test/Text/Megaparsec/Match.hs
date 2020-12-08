module Test.Text.Megaparsec.Match (tests) where

import Test.Hspec
import Text.Megaparsec.Match (matches, (~=))

tests :: SpecWith ()
tests = do
  describe "Text.Megaparsec.Match" $ do
    describe "matches" $ do
      it "Matches" $ do
        "Oh, hello there" `matches` "hello" `shouldBe` True
      it "Does not match" $ do
        "Oh, hi there" ~= "hello" `shouldBe` False
