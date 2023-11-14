module Transcoder.UberEats (
  parseBill,
) where

import Control.Lens qualified as L
import Data.Cash (
  Cash,
  cashP,
 )
import Data.Cash qualified as Cash
import Data.Time (
  Day,
 )
import Data.Time.Extra (dayP)
import Hledger (
  AccountName,
  Status (Cleared, Pending),
  Transaction,
  post,
 )
import Hledger.Data.Extra (
  ToAmount (toAmount),
  ToTransaction (toTransaction),
  makeTransaction,
 )
import Hledger.Data.Lens (pStatus)
import Relude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
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
      [ post (toAccount $ billSource bill) (toAmount . Cash.negate . billAmount $ bill)
          & L.set pStatus Pending
      , post (expenses <:> "Take Away") (toAmount . billAmount $ bill)
      ]

sourceP :: Parser Source
sourceP = MP.string "Mastercard ••••" >> replicateM_ 4 MP.digitChar >> return SourceBcgeCc

dateP :: Parser Day
dateP = dayP "%-m/%-d/%y"

hourP :: Parser ()
hourP =
  void
    $ MP.some MP.digitChar
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
