{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This module parses Degiro CSV statement
module Hledupt.Degiro.Csv
  ( -- * Parsing
    parseCsvStatement,

    -- * Types
    DegiroCsvRecord (..),
    Money (..),
    Currency (..),
    Isin,
    mkIsin,
  )
where

import Control.Applicative.Combinators (count)
import Control.Lens.Internal.ByteString (unpackStrict8)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv ((.!))
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Time
  ( Day,
    TimeOfDay (TimeOfDay),
    defaultTimeLocale,
    parseTimeM,
  )
import Data.Vector (Vector)
import Hledupt.Data (decimalParser)
import Relude
import Text.Megaparsec (single)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (letterChar, numberChar)

newtype DegiroDay = DegiroDay
  { unDegiroDay :: Day
  }

instance Csv.FromField DegiroDay where
  parseField field =
    DegiroDay
      <$> parseTimeM True defaultTimeLocale "%d-%m-%Y" (unpackStrict8 field)

newtype Isin = Isin String
  deriving newtype (Eq, Show)

isinP :: MP.Parsec Void String Isin
isinP = do
  landCode <- count 2 letterChar
  nsin <- count 10 numberChar
  return $ Isin (landCode ++ nsin)

mkIsin :: String -> Maybe Isin
mkIsin = MP.parseMaybe (isinP <* MP.eof)

timeP :: MP.Parsec Void String TimeOfDay
timeP = do
  Just hours <- readMaybe <$> count 2 numberChar
  void $ single ':'
  Just minutes <- readMaybe <$> count 2 numberChar
  return $ TimeOfDay hours minutes 0

data Currency = EUR | CHF
  deriving stock (Eq, Show)

instance Csv.FromField Currency where
  parseField "CHF" = return CHF
  parseField "EUR" = return EUR
  parseField field = fail $ "Could not parse the currency: " ++ unpackStrict8 field ++ "."

data Money = Money
  { moneyCurrency :: Currency,
    moneyAmount :: Decimal
  }
  deriving stock (Eq, Show)

data DegiroCsvRecord = DegiroCsvRecord
  { dcrDate :: Day,
    dcrTime :: TimeOfDay,
    dcrValueDate :: Day,
    dcrProduct :: Text,
    dcrIsin :: Maybe Isin,
    dcrDescription :: Text,
    dcrFx :: Maybe Decimal,
    dcrChange :: Money,
    dcrBalance :: Money,
    dcrOrderId :: Text
  }
  deriving stock (Eq, Show)

instance Csv.FromRecord DegiroCsvRecord where
  parseRecord rec = do
    date <- unDegiroDay <$> rec .! 0
    Just time <- MP.parseMaybe timeP <$> rec .! 1
    valueDate <- unDegiroDay <$> rec .! 2
    productStr <- rec .! 3
    isinStr <- rec .! 4
    maybeIsin <-
      if null isinStr
        then return Nothing
        else do
          Just isin <- return $ mkIsin isinStr
          return $ Just isin
    desc <- rec .! 5
    fxStr :: String <- rec .! 6
    maybeFx <-
      if null fxStr
        then return Nothing
        else do
          Just fx <- return $ MP.parseMaybe @Void decimalParser fxStr
          return $ Just fx
    changeCurrency <- rec .! 7
    Just changeAmount <-
      MP.parseMaybe @Void decimalParser
        <$> (rec .! 8 :: Csv.Parser String)
    balanceCurrency <- rec .! 9
    Just balanceAmount <-
      MP.parseMaybe @Void decimalParser
        <$> (rec .! 10 :: Csv.Parser String)
    orderId <- rec .! 11
    return $
      DegiroCsvRecord
        date
        time
        valueDate
        productStr
        maybeIsin
        desc
        maybeFx
        (Money changeCurrency changeAmount)
        (Money balanceCurrency balanceAmount)
        orderId

parseCsvStatement :: LBS.ByteString -> Either String (Vector DegiroCsvRecord)
parseCsvStatement = Csv.decode Csv.HasHeader
