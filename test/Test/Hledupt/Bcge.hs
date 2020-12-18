{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Bcge (tests) where

import Control.Lens (set)
import Data.Time.Calendar (fromGregorian)
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens
  ( pAccount,
    pBalanceAssertion,
    tDescription,
    tStatus,
  )
import Hledger.Data.Posting (balassert, nullposting)
import qualified Hledger.Data.Posting as Hledger
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types
  ( Status (..),
  )
import Hledupt.Bcge
  ( BcgeTransaction (..),
    bcgeTransactionToLedger,
    csvLineToBcgeTransaction,
    parseStatementDate,
    saldoToLedger,
    statementDateParser,
  )
import qualified Hledupt.Bcge.Hint as Hint
import Hledupt.Data (fromUnitsAndCents)
import Relude
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Text.Megaparsec (parseMaybe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Bcge" $ do
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
                $ transaction (fromGregorian 2020 1 1) [balancePosting]
        saldoToLedger (fromGregorian 2020 1 1) (fromUnitsAndCents 1234 56)
          `shouldBe` saldoTransaction

    describe "bcgeTransactionToLedger" $ do
      it "converts transactions and applies hints" $
        let bcgeTr =
              BcgeTransaction
                (fromGregorian 2020 11 21)
                "Title"
                (fromUnitsAndCents 3 50)
            config =
              [ Hint.ConfigEntry "Title" $
                  Hint.TransactionHint "NewTitle" "Assets:Special"
              ]
            bcgePosting =
              Hledger.post
                "Assets:Liquid:BCGE"
                (HDE.makeCurrencyAmount "CHF" $ fromUnitsAndCents 3 50)
            counterPosting =
              Hledger.post
                "Assets:Special"
                (HDE.makeCurrencyAmount "CHF" $ fromUnitsAndCents (-3) 50)
            expectedTransaction =
              transaction (fromGregorian 2020 11 21) [bcgePosting, counterPosting]
                & set tDescription "NewTitle"
                  . set tStatus Cleared
         in bcgeTransactionToLedger (Just config) bcgeTr
              `shouldBe` expectedTransaction
