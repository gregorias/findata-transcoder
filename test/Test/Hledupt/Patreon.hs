module Test.Hledupt.Patreon (
  tests,
) where

import Hledger.Read.TestUtils (transactionQQ)
import qualified Hledupt.Patreon as Patreon
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Patreon" $ do
    describe "receiptToLedger" $ do
      it "converts to a transaction" $ do
        receipt <- readFileText "test/data/patreon.txt"
        let expectedTr =
              [transactionQQ|
                2021/09/01 * Patreon
                  ! Assets:Liquid:Revolut:USD  -12.00 USD
                  Expenses:Leisure:Patreon      9.00 USD ; TimeGhost
                  Expenses:Leisure:Patreon      3.00 USD ; Alexander Granin|]
        Patreon.receiptToLedger receipt `shouldBe` Right expectedTr
