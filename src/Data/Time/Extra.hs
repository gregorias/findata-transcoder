{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Time.Extra (
  dayP,
  usFullDayP,
) where

import Control.Applicative (some, (<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (fromGregorianValid)
import Data.Time.Calendar.Extra (monthP)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude (ToString (toString))
import Text.Megaparsec (MonadParsec (label, try), Parsec, match, single)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (digitChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude

instance Lift Day where
  liftTyped d =
    let (y, yod) = toOrdinalDate d
     in [||fromOrdinalDate y yod||]

-- | Parses strings like "DD/MM/YYYY" or "YY-MM-DD" into a day.
dayP ::
  (Ord e) =>
  -- | Format string (see 'parseTimeM')
  String ->
  Parsec e Text Day
dayP fmt = label "date" $ do
  (dayStr, _) <-
    match . try $ do
      void $ some digitChar
      sepP
      void $ some digitChar
      sepP
      void $ some digitChar
  parseTimeM False defaultTimeLocale fmt (toString dayStr)
 where
  sepP :: (Ord e) => Parsec e Text ()
  sepP = void $ string "-" <|> string "/" <|> string "."

-- | Parsers strings like "May 4, 2023" into a day.
usFullDayP :: (Ord e) => Parsec e Text Day
usFullDayP = label "date in US format" $ do
  (dateString, (month, day, year)) <- MP.match $
    try $ do
      month <- monthP
      void $ single ' '
      day <- decimal
      void $ single ',' >> single ' '
      year <- decimal
      return (month, day, year)
  let maybeDay = fromGregorianValid year month day
  maybe
    (fail $ "Could not parse " <> toString dateString <> " as a valid date.")
    return
    maybeDay
