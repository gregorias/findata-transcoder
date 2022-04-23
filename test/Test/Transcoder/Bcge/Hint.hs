module Test.Transcoder.Bcge.Hint (tests) where

import Relude
import Test.Hspec (describe, expectationFailure, it, shouldBe)
import qualified Test.Hspec as Hspec
import Text.Megaparsec (parseMaybe)
import qualified Transcoder.Bcge.Hint as BH

tests :: Hspec.SpecWith ()
tests = do
  describe "Bcge.Hint tests" $ do
    it "Correctly parses and applies first matching hint" $ do
      case maybeConfig of
        Just config ->
          BH.transactionTitleToHint config "TWINT MIGROS 123A"
            `shouldBe` Just
              (BH.TransactionHint "Migros" "Expenses:Groceries")
        Nothing -> expectationFailure "Could not parse config"
 where
  maybeConfig =
    parseMaybe
      BH.configParser
      ( "keyword,title,counterAccount\n"
          ++ "MIGROS,Migros,Expenses:Groceries\n"
          ++ "TWINT,Twint,Expenses:Other\n"
      )
