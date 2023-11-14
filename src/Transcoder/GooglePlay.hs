{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Transcoder.GooglePlay (parseReceipt) where

import Control.Monad.Combinators (manyTill_)
import Data.Cash (Cash (..), cashP)
import Data.Cash qualified as Cash
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Extra (usFullDayP)
import Hledger (AccountName, Posting, Status (Cleared, Pending), Transaction)
import Hledger.Data.Extra (Comment (..), ToAmount (toAmount), makePosting, makeTransaction)
import Relude
import Text.Megaparsec (label)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (crlf)
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Extra (anyLineP)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Wallet (bcgeCCAccount, expenses, expensesYtPremium, liquidAssets, (<:>))

data Receipt = Receipt
  { receiptDay :: !Day
  , receiptItems :: ![Item]
  , receiptTotal :: !Cash
  , receiptPaymentMethod :: !PaymentMethod
  }
  deriving stock (Show)

newtype PaymentMethod = MasterCard Text
  deriving stock (Eq, Show)

-- A receipt item.
data Item = Item
  { itemDescription :: !Text
  , itemCost :: !Cash
  }
  deriving stock (Show)

type Parser = MP.Parsec Void Text

preambleP :: Parser Day
preambleP = label "preamble" $ do
  void $ manyTill_ anyLineP $ MP.string "Order date: "
  day <- usFullDayP <* anyLineP
  void $ MP.string "Your account" >> anyLineP
  void $ MP.many crlf
  return day

costP :: Parser Cash
costP = label "cost" $ do
  cost <- MP.try cashP
  void $ optional $ MP.string "/month"
  return cost

itemP :: Parser Item
itemP = do
  (description, cost) <- manyTill_ (MP.noneOf ['\r', '\n']) costP
  void . optional . MP.try $ MP.space >> MP.string "Auto-renewing subscription" >> crlf
  return $ Item (T.strip . toText $ description) cost

itemSectionP :: Parser [Item]
itemSectionP = do
  void $ MP.string "Item Price" >> many crlf
  many itemP <* many crlf

totalP :: Parser Cash
totalP = do
  total <- MP.string "Total: " *> costP <* crlf
  void . optional . MP.try $ MP.space >> MP.string "(Includes VAT of" >> anyLineP
  return total

paymentMethodP :: Parser PaymentMethod
paymentMethodP = label "payment method" $ do
  void $ MP.string "Payment method:" >> MP.space
  digits :: Text <- toText <$> (MP.string "Mastercard-" >> MP.count 4 MP.digitChar)
  return $ MasterCard digits

receiptP :: Parser Receipt
receiptP = do
  day <- preambleP
  (_, items) <- manyTill_ anyLineP itemSectionP
  void $ many crlf
  total <- totalP
  void $ many crlf
  paymentMethod <- paymentMethodP
  return
    $ Receipt
      { receiptDay = day
      , receiptItems = items
      , receiptTotal = total
      , receiptPaymentMethod = paymentMethod
      }

itemToPosting :: Item -> Posting
itemToPosting Item{itemDescription = itemDescriptionValue, itemCost = cost} =
  makePosting
    Nothing
    (getAccountCategoryFromDescription itemDescriptionValue)
    (toAmount <$> Just cost)
    (Comment itemDescriptionValue)
 where
  getAccountCategoryFromDescription :: Text -> AccountName
  getAccountCategoryFromDescription description
    | "YouTube Premium" `T.isPrefixOf` description = expensesYtPremium
    | otherwise = expenses <:> "Todo"

receiptToTransaction :: Receipt -> Transaction
receiptToTransaction
  Receipt
    { receiptDay = day
    , receiptItems = items
    , receiptTotal = total
    , receiptPaymentMethod = paymentMethod
    } =
    makeTransaction
      day
      (Just Cleared)
      "Google Play"
      ((itemToPosting <$> items) <> [totalPosting])
   where
    getPayingAccount :: PaymentMethod -> AccountName
    getPayingAccount (MasterCard digits)
      | digits == "7817" = bcgeCCAccount
      | otherwise = liquidAssets <:> "Todo"
    totalPosting =
      makePosting
        (Just Pending)
        (getPayingAccount paymentMethod)
        (Just . toAmount $ Cash.negate total)
        NoComment

parseReceipt :: Text -> Either Text Transaction
parseReceipt receiptTxt = do
  receipt <- parsePretty receiptP "Google Play receipt" receiptTxt
  return $ receiptToTransaction receipt
