module Test.Bcge.Hint (tests) where

import qualified Bcge.Hint as BH
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Text.Megaparsec (parseMaybe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Bcge.Hint tests" $ do
    it "Correctly parses and applies first matching hint" $ do
      BH.transactionTitleToHint config "TWINT MIGROS 123A"
        `shouldBe` Just (BH.TransactionHint "Migros" "Expenses:Groceries")
  where
    Just config =
      parseMaybe
        BH.configParser
        ( "keyword,title,counterAccount\n"
            ++ "MIGROS,Migros,Expenses:Groceries\n"
            ++ "TWINT,Twint,Expenses:Other\n"
        )
