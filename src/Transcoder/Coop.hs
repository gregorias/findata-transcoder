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
import Data.HashMap.Strict qualified as HashMap
import Hledger (
  AccountName,
  Posting,
  Status (Cleared, Pending, Unmarked),
  Transaction,
  transaction,
 )
import Hledger qualified as Ledger
import Hledger.Data.Extra qualified as HDE
import Hledger.Data.Lens (pStatus, tDescription, tStatus)
import Relude
import Relude.Extra (groupBy)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Coop.Config (Config (..), PaymentCard, assignAccount, getDebtors)
import Transcoder.Coop.Receipt (
  CreditCardPaymentMethod (CreditCardPaymentMethod),
  Entry (..),
  Payment (..),
  PaymentMethod (..),
  Rabatt (..),
  Receipt (..),
  receiptP,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.Wallet (bcgeAccount, bcgeCCAccount, expenses, (<:>))
import Transcoder.Wallet qualified as Wallet

paymentMethodToAccount :: [PaymentCard] -> PaymentMethod -> Text
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
    ( listToMaybe (mapMaybe (\(rgx, cat) -> if has rgx entry then Just cat else Nothing) itemToExpenseCategoryPairs)
    )
 where
  household = "Household"
  coffee = "Groceries" <:> "Coffee"
  readyMeals = "Groceries" <:> "Ready Meals"
  alcohol = "Groceries" <:> "Alcohol"
  health = "Gesundheit"
  itemToExpenseCategoryPairs =
    [ ([regex|Stimorol|], "Groceries:Chewing Gum")
    , ([regex|V6|], "Groceries:Chewing Gum")
    , ([regex|Acqua Panna|], coffee)
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

-- | Splits a single receipt entry into an expense category and optional debtor accounts.
entryToPostings :: Config -> Entry -> ((AccountName, Decimal), [(AccountName, Decimal)])
entryToPostings (Config{shared = rules}) (Entry{entryName = name, entryTotal = total}) =
  ((expenseCategory, mainAlloc), zip debtors debtAllocs)
 where
  expenseCategory = expenses <:> entryNameToExpenseCategory name
  debtors = getDebtors rules name
  twoDigitTotal = realFracToDecimal 2 total
  (mainAlloc : debtAllocs) = D.normalizeDecimal <$> D.allocate twoDigitTotal (replicate (1 + length debtors) 1)

receiptToTransaction :: Config -> Receipt -> Transaction
receiptToTransaction config (Receipt day entries rabatt _total payments) =
  transaction day postings
    & L.set tDescription "Coop"
    . L.set tStatus Cleared
 where
  postings = toList paymentPostings <> catItems <> debtItems <> rabattPosting
  paymentPostings = paymentToPosting (paymentCards config) <$> payments
  (myExpenses :: [(AccountName, Decimal)], debtors :: [(AccountName, Decimal)]) =
    map (entryToPostings config) entries & unzip & fmap concat
  catToTotals :: HashMap Text (NonEmpty Decimal) = snd <<$>> groupBy fst myExpenses
  debtToTotals :: HashMap Text (NonEmpty Decimal) = snd <<$>> groupBy fst debtors
  [catToTotal, debtToTotal] = fmap sum <$> [catToTotals, debtToTotals]
  catItems =
    map
      ( \(cat, total') ->
          Ledger.post
            cat
            (HDE.makeCurrencyAmount chf total')
      )
      (HashMap.toList catToTotal)
  debtItems =
    map
      ( \(cat, total') ->
          Ledger.post
            cat
            (HDE.makeCurrencyAmount chf total')
            & L.set pStatus Pending
      )
      (HashMap.toList debtToTotal)
  rabattToPosting (Rabatt rabattVal) =
    Ledger.post
      "Expenses:Other"
      (HDE.makeCurrencyAmount chf rabattVal)
  rabattPosting = rabattToPosting <$> maybeToList rabatt

receiptToLedger :: Config -> Text -> Either Text Transaction
receiptToLedger config receiptText = do
  receipt <- parsePretty receiptP "a Coop receipt" receiptText
  return $ receiptToTransaction config receipt
