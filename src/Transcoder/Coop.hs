{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Transcoder.Coop (
  receiptToLedger,
) where

import Control.Lens (
  has,
 )
import Control.Lens qualified as L
import Control.Lens.Regex.Text (regex)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Decimal qualified as D
import Data.Foldable1 (foldl1')
import Data.HashMap.Strict qualified as HashMap
import Data.Tuple.Extra (fst3)
import Hledger (
  AccountName,
  Posting,
  Status (Cleared, Pending, Unmarked),
  Transaction,
  transaction,
 )
import Hledger qualified as Ledger
import Hledger.Data.Extra (makePosting)
import Hledger.Data.Extra qualified as HDE
import Hledger.Data.Lens (pStatus, tDescription, tStatus)
import Relude
import Relude.Extra (groupBy)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Coop.Config (Config (..), PaymentCard, SharedExpenseRules, assignAccount, getDebtors)
import Transcoder.Coop.Receipt (
  CreditCardPaymentMethod (CreditCardPaymentMethod),
  Entry (..),
  Payment (..),
  PaymentMethod (..),
  Receipt (..),
  receiptP,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.Wallet (bcgeAccount, bcgeCCAccount, expenses, (<:>))
import Transcoder.Wallet qualified as Wallet

-- | Converts a Coop receipt in text to a Ledger transaction.
receiptToLedger :: Config -> Text -> Either Text Transaction
receiptToLedger config receiptText = do
  receipt <- parsePretty receiptP "a Coop receipt" receiptText
  return $ receiptToTransaction config receipt

-- | Assigns a ledger account to a payment method.
paymentMethodToAccount ::
  -- | My payment cards with their associated ledger accounts.
  [PaymentCard] ->
  -- | The payment method used in a particular transaction.
  PaymentMethod ->
  -- | The ledger account to which the payment should be posted.
  AccountName
paymentMethodToAccount _ TWINT = bcgeAccount
paymentMethodToAccount
  cards
  (CreditCard (CreditCardPaymentMethod accountNumber)) = fromMaybe bcgeCCAccount targetAccount
   where
    targetAccount = assignAccount cards accountNumber
paymentMethodToAccount _ Supercash = Wallet.expensesOther
paymentMethodToAccount _ Superpunkte = Wallet.expensesOther

entryNameToExpenseCategory :: Text -> Text
entryNameToExpenseCategory entry =
  fromMaybe
    "Groceries"
    (listToMaybe (mapMaybe (\(rgx, cat) -> if has rgx entry then Just cat else Nothing) itemToExpenseCategoryPairs))
 where
  household = "Household"
  groceries = "Groceries"
  coffee = groceries <:> "Coffee"
  readyMeals = groceries <:> "Ready Meals"
  alcohol = groceries <:> "Alcohol"
  health = Wallet.health
  itemToExpenseCategoryPairs =
    [ ([regex|Stimorol|], groceries <:> "Chewing Gum")
    , ([regex|V6|], groceries <:> "Chewing Gum")
    , ([regex|Acqua Panna|], coffee)
    , ([regex|Destillat|], coffee)
    , ([regex|Naturaplan Espresso Havelaar Bohnen|], coffee)
    , ([regex|Emmi Caffè Latte|], coffee <:> "Latte")
    , ([regex|Salmon Poké|], readyMeals)
    , ([regex|ZENBU|], readyMeals)
    , ([regex|Findus Egli|], readyMeals)
    , ([regex|BeringerFounders|], alcohol)
    , ([regex|Cahors|], alcohol)
    , ([regex|Dôle|], alcohol)
    , ([regex|Gamay|], alcohol)
    , ([regex|Moscato|], alcohol)
    , ([regex|Nero|], alcohol)
    , ([regex|Wein|], alcohol)
    , ([regex|[tT]eelicht|], household)
    , ([regex|WC Frisch|], household)
    , ([regex|Haushalt|], household)
    , ([regex|Schwamm|], household)
    , ([regex|Desinfektionstücher|], household)
    , ([regex|^À Table!|], household)
    , ([regex|^Ariel|], household)
    , ([regex|^Persil|], household)
    , ([regex|^Flup|], household)
    , ([regex|^Dr.Beckmann|], household)
    , ([regex|^Sun|], household)
    , ([regex|^Palmolive Ultra|], household)
    , ([regex|Reinigung|], household)
    , ([regex|Compo-Bag|], household)
    , ([regex|Brita|], household)
    , ([regex|ZEISS|], household)
    , ([regex|Finish|], household)
    , ([regex|Tesa|], household)
    , ([regex|CR2032|], household)
    , ([regex|Dübel|], household)
    , ([regex|Colgate|], health)
    , ([regex|Listerine|], health)
    , ([regex|Nivea|], health)
    , ([regex|Mücken|], household <:> "Mückenschutz")
    ]

paymentToPosting :: [PaymentCard] -> Payment -> Posting
paymentToPosting cards Payment{paymentMethod = method, paymentTotal = total} =
  Ledger.post
    (paymentMethodToAccount cards method)
    (HDE.makeCurrencyAmount chf (-total))
    & L.set pStatus (postingStatus method)
 where
  postingStatus Supercash = Unmarked
  postingStatus Superpunkte = Unmarked
  postingStatus TWINT = Pending
  postingStatus (CreditCard _) = Pending

type EntryName = Text

-- | Splits a single receipt entry into an expense category and optional debtor accounts.
entryToPostings :: SharedExpenseRules -> Entry -> ((AccountName, Decimal, EntryName), [(AccountName, Decimal, EntryName)])
entryToPostings rules (Entry{entryName = name, entryTotal = total}) =
  ((expenseCategory, mainAlloc, name), zip3 debtors debtAllocs (repeat name))
 where
  expenseCategory = expenses <:> entryNameToExpenseCategory name
  debtors = getDebtors rules name
  twoDigitTotal = realFracToDecimal 2 total
  (mainAlloc : debtAllocs) = D.normalizeDecimal <$> D.allocate twoDigitTotal (replicate (1 + length debtors) 1)

receiptToTransaction :: Config -> Receipt -> Transaction
receiptToTransaction config (Receipt day entries _total payments) =
  transaction day postings
    & L.set tDescription "Coop"
    . L.set tStatus Cleared
 where
  postings = toList paymentPostings <> categoryItems <> debtItems
  paymentPostings = paymentToPosting (paymentCards config) <$> payments
  ( myExpenses :: [(AccountName, Decimal, EntryName)]
    , debtors :: [(AccountName, Decimal, EntryName)]
    ) =
      map (entryToPostings (shared config)) entries & unzip & fmap concat

  debtorToItems :: HashMap Text (NonEmpty (Decimal, EntryName))
  debtorToItems = (\(_, amount, name) -> (amount, name)) <<$>> groupBy fst3 debtors

  debtorToPostingDetails :: HashMap Text (Decimal, Text)
  debtorToPostingDetails =
    foldl1'
      ( \(amount1, comment1) (amount2, comment2) ->
          (amount1 + amount2, comment1 <> ", " <> comment2)
      )
      <$> debtorToItems

  sortedExpenses =
    sortBy
      ( \(catLeft, _, entryLeft) (catRight, _, entryRight) ->
          compare (catLeft, entryLeft) (catRight, entryRight)
      )
      myExpenses
  categoryItems =
    map
      ( \(expenseCategory, amount, entryName) ->
          makePosting Nothing expenseCategory (HDE.makeCurrencyAmount chf amount) (HDE.Comment entryName)
      )
      sortedExpenses
  debtItems =
    map
      ( \(debtor, (total, entryList)) ->
          makePosting
            Pending
            debtor
            (HDE.makeCurrencyAmount chf total)
            (HDE.Comment entryList)
      )
      (HashMap.toList debtorToPostingDetails)
