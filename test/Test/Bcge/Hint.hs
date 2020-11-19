module Test.Bcge.Hint (tests) where

import           Test.Hspec      (describe, hspec, it, shouldBe)
import           Text.Megaparsec (parseMaybe)

import qualified Bcge.Hint       as BH

tests = do
  describe "Bcge.Hint tests" $ do
    it "Correctly parses and applies hints" $ do
      BH.transactionTitleToHint config "TWINT 123A"
        `shouldBe` Just (BH.TransactionHint "Twint" "Expenses:Groceries")
      where
        Just config = parseMaybe BH.configParser
          "keyword,title,counterAccount\nTWINT,Twint,Expenses:Groceries"
