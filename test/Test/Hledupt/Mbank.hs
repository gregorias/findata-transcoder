{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Mbank
  ( mbankTests,
  )
where

import Data.Text (pack)
import Data.Time.Calendar (fromGregorian)
import Hledger.Data.Posting (balassert, nullposting, post')
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types
  ( Posting (..),
    Transaction (..),
  )
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Data (fromUnitsAndCents)
import Hledupt.Mbank
  ( MbankTransaction (..),
    mTrToLedger,
    mbankCsvParser,
    pln,
    valueParser,
  )
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Text.Megaparsec (parseMaybe)

mbankTests :: Hspec.SpecWith ()
mbankTests = do
  describe "Mbank tests" $ do
    describe "valueParser" $ do
      it "parses a valid monetary amount" $ do
        parseMaybe valueParser "10 100,10 PLN" `shouldBe` Just (fromUnitsAndCents 10100 10)

    describe "mbankCsvParser" $ do
      it "parses a valid CSV" $ do
        let mbankCsv =
              "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n"
                ++ "2020-10-28;\"Title\";\"eKonto 1111 ... 1111\";\"category\";-15,00 PLN;10 100,10 PLN;"
        let expectedMbankTransaction =
              MbankTransaction
                (fromGregorian 2020 10 28)
                "Title"
                (fromUnitsAndCents (-15) 0)
                (fromUnitsAndCents 10100 10)
        parseMaybe mbankCsvParser mbankCsv `shouldBe` Just [expectedMbankTransaction]

    describe "mTrToLedger" $ do
      it "transforms an mbank transaction" $ do
        let ledgerTr =
              parseTransactionUnsafe
                "2019/10/28 PRZELEW ŚRODKÓW\n\
                \  Assets:Liquid:mBank  PLN -15 = PLN 100949\n\
                \  Expenses:Other"
            mbankTr =
              MbankTransaction
                (fromGregorian 2019 10 28)
                "PRZELEW ŚRODKÓW"
                (fromUnitsAndCents (-15) 0)
                (fromUnitsAndCents 100949 0)
        mTrToLedger mbankTr `shouldBe` ledgerTr
