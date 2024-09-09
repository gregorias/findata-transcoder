module Test.Transcoder.Coop.Receipt (tests) where

import Relude
import Test.HUnit.Extra (assertRight)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec.Extra qualified as MP
import Transcoder.Coop.Receipt qualified as Coop

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Coop.Receipt" $ do
    describe "entryLineP" $ do
      it "parses a correction entry line" $ do
        entry <- assertRight $ MP.parsePretty Coop.entryLineP "" "Naturaplan Bio Sal-mon Poké Bowl 320G -1.0 14.95 -14.95 0"
        entry
          `shouldBe` Coop.Entry
            { entryName = "Naturaplan Bio Sal-mon Poké Bowl 320G"
            , entryTotal = -14.95
            }
