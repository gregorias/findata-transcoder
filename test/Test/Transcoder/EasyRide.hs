module Test.Transcoder.EasyRide (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Transcoder.EasyRide as EasyRide

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.EasyRide" $ do
    describe "receiptToLedger" $ do
      it "convertsToATransaction" $ do
        receipt <- readFileText "test/data/easyride.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/07/11 * EasyRide
                    ! Assets:Liquid:BCGE CC  -2.30 CHF
                    Expenses:Transport        2.30 CHF
                    |]
        EasyRide.receiptToLedger receipt `shouldBe` Right expectedTr
