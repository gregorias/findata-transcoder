{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.CharlesSchwab.Ledger (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.CharlesSchwab.Csv (
  CsCsvRecord (CsCsvRecord),
  DollarAmount (..),
 )
import Hledupt.CharlesSchwab.Ledger (csvToLedger)
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.CharlesSchwab.Ledger" $ do
    describe "csvToLedger" $ do
      it "transforms a wire fund entry" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2021 1 19)
                  "Wire Funds"
                  ""
                  "WIRED FUNDS DISBURSED"
                  Nothing
                  Nothing
                  Nothing
                  (Just $ DollarAmount (fromRational $ -12345 % 100))
              ]
        csvToLedger entries
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2021/01/19 Wire Funds\n\
                    \  * Assets:Liquid:Charles Schwab:USD  -123.45 USD\n\
                    \  ! TODO"
                ]
                []
            )
