module Test.Transcoder.GooglePlay (tests) where

import Data.Text.IO.Extra (readFileUtf8)
import Hledger.Read.TestUtils (transactionQQ)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.GooglePlay qualified as GooglePlay
import Prelude

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.GooglePlay" $ do
    describe "parseReceipt" $ do
      it "converts a receipt to a transaction" $ do
        receipt <- readFileUtf8 "./test/data/google-play.txt"
        let expectedTr =
              [transactionQQ|
                2023/05/04 * Google Play
                  Expenses:Leisure:YouTube  CHF  15.90 ; YouTube Premium (YouTube)
                  ! Assets:Liquid:BCGE CC   CHF -15.90
              |]
        GooglePlay.parseReceipt receipt `shouldBe` Right expectedTr
