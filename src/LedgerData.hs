{-# LANGUAGE TemplateHaskell #-}

module LedgerData
  ( Currency (..),
    Value (..),
    amount,
    currency,
    negateValue,
    Posting (..),
    account,
    value,
    balance,
    exchangeRate,
    valueToLedger,
    postingToLedger,
    Transaction (..),
    date,
    description,
    postings,
    comment,
  )
where

import Control.Lens (makeLenses, over, (^.))
import Data.Decimal (Decimal)
import Data.Function ((&))
import Data.Time.Calendar (Day)

data Currency = USD | EUR | CHF | PLN deriving (Eq, Show)

data Value = Value {_amount :: Decimal, _currency :: Currency}
  deriving (Eq, Show)

makeLenses ''Value

negateValue :: Value -> Value
negateValue v = over amount negate v

data Posting = Posting
  { _account :: String,
    _value :: Value,
    _balance :: Value,
    _exchangeRate :: Value
  }

makeLenses ''Posting

data Transaction = Transaction
  { _date :: Day,
    _description :: String,
    _postings :: [Posting],
    _comment :: String
  }

makeLenses ''Transaction

valueToLedger :: Value -> String
valueToLedger (Value amountArg currencyArg) = show amountArg ++ " " ++ show currencyArg

postingToLedger :: Posting -> String
postingToLedger p =
  "  "
    ++ (p ^. account)
    ++ "  "
    ++ (p ^. value & valueToLedger)
    ++ " @ "
    ++ (p ^. exchangeRate & valueToLedger)
    ++ " = "
    ++ (p ^. balance & valueToLedger)
