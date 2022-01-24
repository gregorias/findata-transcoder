{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Text.Megaparsec.Char.Extra (tests) where

import Relude
import Test.Hspec (describe, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char.Extra (anyLineP, eolOrEof)

tests :: Hspec.SpecWith ()
tests = do
  describe "Text.Megaparsec.Char.Extra" $ do
    describe "eolOrEof" $ do
      Hspec.it "Returns \\r\\n on newline" $ do
        MP.parseMaybe eolOrEof "\r\n" `shouldBe` Just "\r\n"
      Hspec.it "Returns empty string on eof" $ do
        MP.parseMaybe eolOrEof "" `shouldBe` Just ""
      Hspec.it "Fails on a non-empty string" $ do
        MP.parseMaybe eolOrEof "hello" `shouldBe` Nothing
    describe "anyLineP" $ do
      Hspec.it "Returns a line with newline" $ do
        MP.parseMaybe anyLineP "abc\n" `shouldBe` Just "abc\n"
      Hspec.it "Returns a line without newline" $ do
        MP.parseMaybe anyLineP "abc" `shouldBe` Just "abc"
      Hspec.it "Fails on eof" $ do
        MP.parseMaybe anyLineP "" `shouldBe` Nothing
