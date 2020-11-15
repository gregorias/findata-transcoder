{-# LANGUAGE UnicodeSyntax #-}
module Bcge(
  BcgeTransaction(..),
)
where

import           Data.Time.Calendar (Day)
import           Text.Megaparsec    (Parsec, eof, many, noneOf, oneOf, parse,
                                     (<|>))

-- | BCGE's transaction data fetched from their website.
data BcgeTransaction = BcgeTransaction{mTrDate        :: Day}
                                         deriving (Eq, Show)

-- Parser functionality (CSV String → [BcgeTransaction])

type MbankParser = Parsec () String

bcgeCsvToLedger :: String → String
bcgeCsvToLedger inputCsv = undefined
