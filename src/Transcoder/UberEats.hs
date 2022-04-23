module Transcoder.UberEats (
  parseBill,
) where

import qualified Control.Lens as L
import Data.Cash (
  Cash,
  cashP,
 )
import qualified Data.Cash as Cash
import Data.Time (
  Day,
  fromGregorianValid,
 )
import Hledger (
  AccountName,
  Status (Cleared, Pending),
  Transaction,
  post,
 )
import Hledger.Data.Extra (
  ToTransaction (toTransaction),
  makeCashAmount,
  makeTransaction,
 )
import Hledger.Data.Lens (pStatus)
import Relude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Transcoder.Wallet (
  bcgeCCAccount,
  expenses,
  (<:>),
 )

type Parser = MP.Parsec Void Text

data Source = SourceBcgeCc

toAccount :: Source -> AccountName
toAccount SourceBcgeCc = bcgeCCAccount

data Bill = Bill
  { billSource :: !Source
  , billDay :: !Day
  , billAmount :: !Cash
  }

instance ToTransaction Bill where
  toTransaction bill =
    makeTransaction
      (billDay bill)
      (Just Cleared)
      "Uber Eats"
      [ post (toAccount $ billSource bill) (makeCashAmount . Cash.negate . billAmount $ bill)
          & L.set pStatus Pending
      , post (expenses <:> "Take Away") (makeCashAmount . billAmount $ bill)
      ]

sourceP :: Parser Source
sourceP = MP.string "Mastercard ••••" >> replicateM_ 4 MP.digitChar >> return SourceBcgeCc

dateP :: Parser Day
dateP = do
  (dateText, (month, day, year)) <-
    MP.match
      ( do
          month <- MP.decimal
          void $ MP.single '/'
          day <- MP.decimal
          void $ MP.single '/'
          year <- MP.decimal
          return (month, day, year)
      )
  maybe
    (fail . toString $ "Could not parse " <> dateText <> " as a date.\n")
    return
    (fromGregorianValid (2000 + year) month day)

hourP :: Parser ()
hourP =
  void $
    MP.some MP.digitChar
      >> MP.single ':'
      >> MP.some MP.digitChar
      >> MP.single ' '
      >> MP.choice (MP.string <$> ["AM", "PM"])

billP :: Parser Bill
billP = do
  void $ MP.string "Payments"
  void MP.anySingle -- non-breakable space
  src <- sourceP
  parsedDay <- dateP
  void MP.anySingle -- space
  hourP
  Bill src parsedDay <$> cashP

parseBill :: Text -> Either Text Transaction
parseBill = parsePretty billP "the Uber Eats bill" >=> (return . toTransaction)
