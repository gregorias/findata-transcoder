module Test.Transcoder.Finpension (tests) where

import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Transcoder.Finpension as Finpension

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Finpension" $ do
    it "converts Finpension total to a transaction" $ do
      let total = "6824.94\n"
          today = fromGregorian 2010 01 01
          expectedStatusTransaction =
            [transactionQQ|
               2010/01/01 * Finpension Status
                 Assets:Investments:Finpension  = CHF 6824.94
                 Equity:Finpension Capital Changes|]
      Finpension.parsePortfoliosTotal today total `shouldBe` Right expectedStatusTransaction
