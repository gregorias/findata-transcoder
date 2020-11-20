module Test.Bcge where

import Bcge
  ( BcgeTransaction (..),
    csvLineToBcgeTransaction,
    parseStatementDate,
    saldoToLedger,
    statementDateParser,
  )
import Control.Lens (over, set, (.~), (^.))
import Data (fromUnitsAndCents)
import Data.Time.Calendar (fromGregorian)
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens
  ( aStyle,
    asCommoditySide,
    asCommoditySpaced,
    asPrecision,
    pAccount,
    pBalanceAssertion,
    tDescription,
    tStatus,
  )
import Hledger.Data.Posting (balassert, nullposting, post')
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types
  ( Amount (..),
    Posting (..),
    Quantity (..),
    Status (..),
    Transaction (..),
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Megaparsec (parseMaybe)

bcgeTests = do
  describe "Bcge tests" $ do
    describe "parseStatementDate" $ do
      it "parses statement date" $ do
        parseStatementDate "08.11.2020" `shouldBe` Just (fromGregorian 2020 11 8)
    describe "statementDateParser" $ do
      it "parses statement date" $ do
        parseMaybe statementDateParser "Kontoauszug bis: 08.11.2020 " `shouldBe` Just (fromGregorian 2020 11 8)
    describe "csvLinesToBcgeTransaction" $ do
      it "parses a valid CSV line" $ do
        csvLineToBcgeTransaction ("06.11.20", "Title", "-5.50")
          `shouldBe` Just
            ( BcgeTransaction
                (fromGregorian 2020 11 6)
                "Title"
                (fromUnitsAndCents (-5) 50)
            )
    describe "saldoToLedger" $ do
      it "transforms end balance to Ledger's transaction" $ do
        let balancePosting =
              set pAccount "Assets:Liquid:BCGE"
                . set pBalanceAssertion (balassert . HDE.makeCurrencyAmount "CHF" $ fromUnitsAndCents 1234 56)
                $ nullposting
            saldoTransaction =
              set tStatus Cleared
                . set tDescription "BCGE Status"
                $ transaction "2020/01/01" [balancePosting]
        saldoToLedger (fromGregorian 2020 1 1) (fromUnitsAndCents 1234 56)
          `shouldBe` saldoTransaction
