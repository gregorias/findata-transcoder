{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Coop (
  tests,
) where

import qualified Hledupt.Coop as Coop
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Coop" $ do
    describe "receiptToLedger" $ do
      it "convertsToATransaction" $ do
        coop <- readFileText "test/data/coop.txt"
        Coop.receiptToLedger coop `shouldBe` Left "Unimplemented"
