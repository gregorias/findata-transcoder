{-# LANGUAGE OverloadedLists #-}

module Test.Transcoder.Mbank (
  mbankTests,
) where

import Data.Time.Calendar (fromGregorian)
import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec qualified as Hspec
import Text.Megaparsec (parseMaybe)
import Transcoder.Data.MyDecimal (fromUnitsAndCents)
import Transcoder.Mbank (
  MbankTransaction (..),
  decodeMbankCsv,
  mTrToLedger,
  valueP,
 )

mbankTests :: Hspec.SpecWith ()
mbankTests = do
  describe "Mbank tests" $ do
    describe "valueP" $ do
      it "parses a valid monetary amount" $ do
        parseMaybe valueP "10 100,10 PLN" `shouldBe` Just (fromUnitsAndCents 10100 10)

    describe "decodeMbankCsv" $ do
      it "parses a valid CSV" $ do
        let mbankCsv =
              "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n"
                <> "2020-10-28;\"Title\";\"eKonto 1111 ... 1111\";\"category\";-15,00 PLN;10 100,10 PLN;"
        let expectedMbankTransaction =
              MbankTransaction
                (fromGregorian 2020 10 28)
                "Title"
                (fromUnitsAndCents (-15) 0)
                (fromUnitsAndCents 10100 10)
        decodeMbankCsv mbankCsv `shouldBe` Right [expectedMbankTransaction]

    describe "mTrToLedger" $ do
      it "transforms an mbank transaction" $ do
        let ledgerTr =
              [transactionQQ|
                  2019/10/28 PRZELEW ŚRODKÓW
                    Assets:Liquid:mBank  PLN -15 = PLN 100949
                    Expenses:Other|]
            mbankTr =
              MbankTransaction
                (fromGregorian 2019 10 28)
                "PRZELEW ŚRODKÓW"
                (fromUnitsAndCents (-15) 0)
                (fromUnitsAndCents 100949 0)
        mTrToLedger mbankTr `shouldBe` ledgerTr
