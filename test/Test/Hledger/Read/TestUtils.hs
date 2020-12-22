{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hledger.Read.TestUtils (tests) where

import qualified Control.Lens as L
import Data.Time (fromGregorian)
import Hledger
  ( Amount (aprice),
    AmountPrice (UnitPrice),
    missingamt,
  )
import Hledger.Data.Extra
  ( makeCommodityAmount,
    makeCurrencyAmount,
  )
import Hledger.Data.Lens (tDescription, tStatus)
import Hledger.Data.Posting
  ( balassert,
    nullposting,
    post,
  )
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types
  ( MixedAmount (..),
    Posting (..),
    Status (..),
  )
import Hledger.Read.TestUtils (parseTransactionUnsafe, postingParser)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec (parseMaybe)
import qualified Text.Megaparsec as MP

tests :: Hspec.SpecWith ()
tests = do
  describe "Test.Hledger.Read.TestUtils" $ do
    describe "postingParser" $ do
      it "Parses a posting transaction" $ do
        let p :: String = "  Expenses:Other"
            expectedP = post "Expenses:Other" missingamt
        parseMaybe postingParser p `shouldBe` Just expectedP
      it "Parses a posting transaction with spaces" $ do
        let p :: String = "  Assets:Bank With Spaces\n"
            expectedP = post "Assets:Bank With Spaces" missingamt
        parseMaybe postingParser p `shouldBe` Just expectedP
      it "Parses a cleared posting" $ do
        let p :: String = "*  Expenses:Other"
            expectedP =
              (post "Expenses:Other" missingamt)
                { pstatus = Cleared
                }
        parseMaybe postingParser p `shouldBe` Just expectedP

    describe "transactionParser" $ do
      it "Parses a moneyless transaction" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank With Spaces\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post "Assets:Bank With Spaces" missingamt,
                  post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "Parses a proper transaction with amount" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank With Spaces  SPY -15\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post
                    "Assets:Bank With Spaces"
                    (makeCommodityAmount "SPY" (-15)),
                  post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "Parses a proper transaction with balance" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank  = SPY 123\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ (post "Assets:Bank" missingamt)
                    { pbalanceassertion =
                        balassert $ makeCommodityAmount "SPY" 123
                    },
                  post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "Parses a proper transaction with amount & balance" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank  SPY 100 = SPY 123\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ nullposting
                    { paccount = "Assets:Bank",
                      pbalanceassertion =
                        balassert $
                          makeCommodityAmount "SPY" 123,
                      pamount = Mixed [makeCommodityAmount "SPY" 100]
                    },
                  post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "Parses a Forex posting with price information." $ do
        let posting = "  Assets:Bank  USD 100 @ CHF 0.9\n"
            expectedPosting =
              post
                "Assets:Bank"
                ( (makeCurrencyAmount "USD" 100)
                    { aprice = Just . UnitPrice $ makeCurrencyAmount "CHF" 0.9
                    }
                )
        MP.parseMaybe postingParser posting `shouldBe` Just expectedPosting
