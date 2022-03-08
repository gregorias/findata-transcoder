module Test.Hledger.Extra (
  tests,
) where

import Hledger.Extra (showTransaction)
import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as HSpec

tests :: HSpec.SpecWith ()
tests = do
  describe "Hledger.Extra" $ do
    it "shows a Transaction with extra lines" $ do
      let t =
            [transactionQQ|
                2022/03/08 Title
                  Bank  10 CHF|]
      showTransaction t
        `shouldBe` "2022-03-08 Title\n\
                   \    Bank       CHF 10.00\n"
