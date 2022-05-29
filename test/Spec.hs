module Spec (main) where

import Relude
import qualified Test.Data.Csv.Extra
import qualified Test.Data.Time.Extra
import qualified Test.Hledger.Data.Extra as HDE
import qualified Test.Hledger.Data.Lens
import qualified Test.Hledger.Data.MarketPrice.Extra
import qualified Test.Hledger.Extra
import qualified Test.Hledger.Read.TestUtils
import Test.Hspec (SpecWith, hspec)
import qualified Test.Text.Megaparsec.Char.Extra
import qualified Test.Text.Megaparsec.Match
import qualified Test.Transcoder.Bcge
import qualified Test.Transcoder.Bcge.Hint as BcgeHint
import qualified Test.Transcoder.BcgeCC
import qualified Test.Transcoder.CharlesSchwab.Csv
import qualified Test.Transcoder.CharlesSchwab.Ledger
import qualified Test.Transcoder.Coop
import qualified Test.Transcoder.Coop.Config
import qualified Test.Transcoder.Data.LedgerReport
import qualified Test.Transcoder.Data.MyDecimal
import qualified Test.Transcoder.Degiro.AccountStatement
import qualified Test.Transcoder.Degiro.Csv
import qualified Test.Transcoder.Degiro.Portfolio
import qualified Test.Transcoder.EasyRide
import qualified Test.Transcoder.Finpension.Transactions
import qualified Test.Transcoder.GPayslip
import qualified Test.Transcoder.GPayslip.PdfToText
import qualified Test.Transcoder.Galaxus
import qualified Test.Transcoder.Ib
import qualified Test.Transcoder.Ib.Csv.ActivityStatementParse
import qualified Test.Transcoder.Ib.Csv.RawParse
import Test.Transcoder.Mbank (mbankTests)
import qualified Test.Transcoder.Patreon
import qualified Test.Transcoder.Revolut
import qualified Test.Transcoder.Splitwise
import qualified Test.Transcoder.UberEats

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  BcgeHint.tests
  mbankTests
  HDE.tests
  Test.Data.Csv.Extra.tests
  Test.Data.Time.Extra.tests
  Test.Hledger.Data.Lens.tests
  Test.Hledger.Data.MarketPrice.Extra.tests
  Test.Hledger.Extra.tests
  Test.Hledger.Read.TestUtils.tests
  Test.Transcoder.Bcge.tests
  Test.Transcoder.BcgeCC.tests
  Test.Transcoder.CharlesSchwab.Csv.tests
  Test.Transcoder.CharlesSchwab.Ledger.tests
  Test.Transcoder.Coop.tests
  Test.Transcoder.Coop.Config.tests
  Test.Transcoder.Data.LedgerReport.tests
  Test.Transcoder.Data.MyDecimal.tests
  Test.Transcoder.Degiro.AccountStatement.tests
  Test.Transcoder.Degiro.Csv.tests
  Test.Transcoder.Degiro.Portfolio.tests
  Test.Transcoder.EasyRide.tests
  Test.Transcoder.Finpension.Transactions.tests
  Test.Transcoder.Galaxus.tests
  Test.Transcoder.GPayslip.PdfToText.tests
  Test.Transcoder.GPayslip.tests
  Test.Transcoder.Ib.Csv.ActivityStatementParse.tests
  Test.Transcoder.Ib.Csv.RawParse.tests
  Test.Transcoder.Ib.tests
  Test.Transcoder.Patreon.tests
  Test.Transcoder.Revolut.tests
  Test.Transcoder.Splitwise.tests
  Test.Transcoder.UberEats.tests
  Test.Text.Megaparsec.Char.Extra.tests
  Test.Text.Megaparsec.Match.tests
