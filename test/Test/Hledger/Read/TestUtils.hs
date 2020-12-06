{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledger.Read.TestUtils (tests) where

import qualified Control.Lens as L
import Data.Function ((&))
import Hledger.Data.Extra
  ( makeCommodityAmount,
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
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Text.Megaparsec (parseMaybe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Test.Hledger.Read.TestUtils" $ do
    describe "postingParser" $ do
      it "parses a posting transaction" $ do
        let p = "  Expenses:Other"
            expectedP = nullposting {paccount = "Expenses:Other"}
        parseMaybe postingParser p `shouldBe` Just expectedP
      it "parses a posting transaction with spaces" $ do
        let p = "  Assets:Bank With Spaces\n"
            expectedP = nullposting {paccount = "Assets:Bank With Spaces"}
        parseMaybe postingParser p `shouldBe` Just expectedP
      it "parses a cleared posting" $ do
        let p = "*  Expenses:Other"
            expectedP =
              nullposting
                { paccount = "Expenses:Other",
                  pstatus = Cleared
                }
        parseMaybe postingParser p `shouldBe` Just expectedP

    describe "transactionParser" $ do
      it "parses a moneyless transaction" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank With Spaces\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                "2019/10/28"
                [ nullposting {paccount = "Assets:Bank With Spaces"},
                  nullposting {paccount = "Expenses:Other"}
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "parses a proper transaction with amount" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank With Spaces  SPY -15\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                "2019/10/28"
                [ post
                    "Assets:Bank With Spaces"
                    (makeCommodityAmount "SPY" (-15)),
                  -- TODO
                  --(balassert $ pln 100949),
                  nullposting {paccount = "Expenses:Other"}
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "parses a proper transaction with balance" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank  = SPY 123\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                "2019/10/28"
                [ nullposting
                    { paccount = "Assets:Bank",
                      pbalanceassertion =
                        balassert $ makeCommodityAmount "SPY" 123
                    },
                  nullposting {paccount = "Expenses:Other"}
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
      it "parses a proper transaction with amount & balance" $ do
        let tr =
              "2019/10/28 * Title\n\
              \  Assets:Bank  SPY 100 = SPY 123\n\
              \  Expenses:Other"
            expectedTrBase =
              transaction
                "2019/10/28"
                [ nullposting
                    { paccount = "Assets:Bank",
                      pbalanceassertion =
                        balassert $
                          makeCommodityAmount "SPY" 123,
                      pamount = Mixed [makeCommodityAmount "SPY" 100]
                    },
                  nullposting {paccount = "Expenses:Other"}
                ]
            expectedTr =
              expectedTrBase
                & L.set tDescription "Title"
                & L.set tStatus Cleared
        parseTransactionUnsafe tr `shouldBe` expectedTr
