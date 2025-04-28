{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Hledger.Read.TestUtils (tests) where

import Control.Lens qualified as L
import Data.Time (fromGregorian)
import Hledger (
  Amount (acost),
  AmountCost (UnitCost),
  amountSetFullPrecision,
  missingamt,
 )
import Hledger.Data.Extra (
  makeCommodityAmount,
  makeCurrencyAmount,
 )
import Hledger.Data.Lens (tDescription, tStatus)
import Hledger.Data.Posting (
  balassert,
  nullposting,
  post,
 )
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types (
  MixedAmount (..),
  MixedAmountKey (MixedAmountKeyNoCost),
  Posting (..),
  Status (..),
 )
import Hledger.Read.TestUtils (
  postingP,
  transactionQQ,
  transactionsQQ,
 )
import Relude
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec (parseMaybe)
import Transcoder.Data.Currency (usd)

tests :: Hspec.SpecWith ()
tests = do
  describe "Test.Hledger.Read.TestUtils" $ do
    describe "transactionQQ" $ do
      it "Parses a transaction" $ do
        let tr =
              [transactionQQ|
                     2019/10/28 * Title
                       Assets:Bank With Spaces  SPY -15
                       Expenses:Other|]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post
                    "Assets:Bank With Spaces"
                    (makeCommodityAmount "SPY" (-15))
                , post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        tr `shouldBe` expectedTr
    describe "transactionsQQ" $ do
      it "Parses transactions" $ do
        let trs =
              [transactionsQQ|
                     2019/10/28 * Title
                       Assets:Bank With Spaces  SPY -15
                       Expenses:Other
                     2019/10/28 * Title
                       Assets:Bank With Spaces  SPY -15
                       Expenses:Other
                       |]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post
                    "Assets:Bank With Spaces"
                    (makeCommodityAmount "SPY" (-15))
                , post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        trs `shouldBe` [expectedTr, expectedTr]

    describe "postingP" $ do
      it "Parses a posting transaction" $ do
        let p = "  Expenses:Other"
            expectedP = post "Expenses:Other" missingamt
        parseMaybe postingP p `shouldBe` Just expectedP
      it "Parses a posting transaction with spaces" $ do
        let p = "  Assets:Bank With Spaces\n"
            expectedP = post "Assets:Bank With Spaces" missingamt
        parseMaybe postingP p `shouldBe` Just expectedP
      it "Parses a cleared posting" $ do
        let p = "*  Expenses:Other"
            expectedP =
              (post "Expenses:Other" missingamt)
                { pstatus = Cleared
                }
        parseMaybe postingP p `shouldBe` Just expectedP
      it "Parses a posting with a quoted commodity" $ do
        let p = "  Bank  2 \"ISINNUM123\""
            expectedP = post "Bank" (makeCommodityAmount "ISINNUM123" 2)
        parseMaybe postingP p `shouldBe` Just expectedP

    describe "transactionParser" $ do
      it "Parses a moneyless transaction" $ do
        let tr =
              [transactionQQ|
               2019/10/28 * Title
                 Assets:Bank With Spaces
                 Expenses:Other|]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post "Assets:Bank With Spaces" missingamt
                , post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        tr `shouldBe` expectedTr
      it "Parses a proper transaction with amount" $ do
        let tr =
              [transactionQQ|
              2019/10/28 * Title
                Assets:Bank With Spaces  SPY -15
                Expenses:Other|]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post
                    "Assets:Bank With Spaces"
                    (makeCommodityAmount "SPY" (-15))
                , post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        tr `shouldBe` expectedTr
      it "Parses a proper transaction with spaceless amount" $ do
        let tr =
              [transactionQQ|
              2019/10/28 Title
                Bank  -15SPY|]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ post
                    "Bank"
                    (makeCommodityAmount "SPY" (-15))
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
        tr `shouldBe` expectedTr
      it "Parses a proper transaction with balance" $ do
        let tr =
              [transactionQQ|
              2019/10/28 * Title
                Assets:Bank  = SPY 123
                Expenses:Other|]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ (post "Assets:Bank" missingamt)
                    { pbalanceassertion =
                        balassert $ makeCommodityAmount "SPY" 123
                    }
                , post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        tr `shouldBe` expectedTr
      it "Parses a proper transaction with amount & balance" $ do
        let tr =
              [transactionQQ|
                2019/10/28 * Title
                 Assets:Bank  SPY 100 = SPY 123
                 Expenses:Other|]
            expectedTrBase =
              transaction
                (fromGregorian 2019 10 28)
                [ nullposting
                    { paccount = "Assets:Bank"
                    , pbalanceassertion =
                        balassert
                          $ makeCommodityAmount "SPY" 123
                    , pamount = Mixed $ fromList [(MixedAmountKeyNoCost "SPY", makeCommodityAmount "SPY" 100)]
                    }
                , post "Expenses:Other" missingamt
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        tr `shouldBe` expectedTr
      it "Parses a Forex posting with price information." $ do
        let posting = "  Assets:Bank  USD 100 @ CHF 0.9513\n"
            expectedPosting =
              post
                "Assets:Bank"
                ( (makeCurrencyAmount usd 100)
                    { acost =
                        Just
                          . UnitCost
                          $ makeCommodityAmount "CHF" 0.9513
                          & amountSetFullPrecision
                    }
                )
        parseMaybe postingP posting `shouldBe` Just expectedPosting
      it "Parses a posting comment" $ do
        let posting = "  Assets:Bank  ; comment\n"
            expected :: Posting =
              (post "Assets:Bank" missingamt){pcomment = "comment"}
        parseMaybe postingP posting `shouldBe` Just expected
