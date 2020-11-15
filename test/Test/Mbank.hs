module Test.Mbank (
  mbankTests)
where

import           Mbank                    (MbankTransaction (..),
                                           fromZloteAndGrosze, mTrToLedger,
                                           mbankCsvParser, pln, valueParser)

import           Data.Time.Calendar       (fromGregorian)
import           Text.Megaparsec          (parseMaybe)

import           Test.Hspec               (describe, it, shouldBe)

import           Data.Text                (pack)

import           Hledger.Data.Amount      (amountWithCommodity, num)
import           Hledger.Data.Posting     (balassert, nullposting, post')
import           Hledger.Data.Transaction (transaction)
import           Hledger.Data.Types       (Amount (..), Posting (..),
                                           Quantity (..), Transaction (..))

mbankTests = do
  describe "Mbank tests" $ do
    describe "valueParser" $ do
      it "parses a valid monetary amount" $ do
        parseMaybe valueParser "10 100,10 PLN" `shouldBe` Just (fromZloteAndGrosze 10100 10)

    describe "mbankCsvParser" $ do
      it "parses a valid CSV" $ do
        let mbankCsv = "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n"
                        ++ "2020-10-28;\"Title\";\"eKonto 1111 ... 1111\";\"category\";-15,00 PLN;10 100,10 PLN;"
        let expectedMbankTransaction = MbankTransaction
                                         (fromGregorian 2020 10 28)
                                         "Title"
                                         (fromZloteAndGrosze (-15) 0)
                                         (fromZloteAndGrosze 10100 10)
        parseMaybe mbankCsvParser mbankCsv `shouldBe` Just [expectedMbankTransaction]

    describe "mTrToLedger" $ do
      it "transforms an mbank transaction" $ do
        let ledgerTr = transaction
                         "2019/10/28" [
                                       post' (pack "Assets:Liquid:mBank") (pln (-15)) (balassert $ pln 100949),
                                       nullposting{paccount=pack "Expenses:Other"}]
            ledgerTrWithDescription = ledgerTr{tdescription=pack "PRZELEW ŚRODKÓW"}
            mbankTr  = MbankTransaction (fromGregorian 2019 10 28) "PRZELEW ŚRODKÓW" (fromZloteAndGrosze (-15) 0) (fromZloteAndGrosze 100949 0)
        mTrToLedger mbankTr `shouldBe` ledgerTrWithDescription
