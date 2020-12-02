module Test.Hledger.Data.Lens (tests) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import Data.Ratio ((%))
import Hledger.Data.Amount (num)
import Hledger.Data.Lens
import Hledger.Data.Types
  ( Amount (..),
    AmountStyle (..),
    BalanceAssertion (..),
    CommoditySymbol,
    MixedAmount (..),
    Posting (..),
    Side (..),
    Status (..),
    Transaction (..),
  )
import Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Hledger.Data.Lens" $ do
    describe "maAmount" $ do
      it "sets amount on an empty MixedAmount" $ do
        let one = fromRational 1
        L.set maMaybeAmount (Just $ num one) (Mixed [])
          `shouldBe` Mixed [num one]
