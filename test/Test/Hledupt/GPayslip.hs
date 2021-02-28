{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.GPayslip (
  tests,
) where

import Relude
import Test.Hspec (describe)
import qualified Test.Hspec as Hspec

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.GPayslip" $ do
    describe "parsePayslip" $ do
      return ()
