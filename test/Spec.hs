module Spec (main) where

import Relude
import Test.Control.Applicative.Combinators.Extra qualified
import Test.Data.Csv.Extra qualified
import Test.Data.Time.Calendar.Extra qualified
import Test.Data.Time.Extra qualified
import Test.Hledger.Data.Extra qualified as HDE
import Test.Hledger.Data.Lens qualified
import Test.Hledger.Data.MarketPrice.Extra qualified
import Test.Hledger.Extra qualified
import Test.Hledger.Read.TestUtils qualified
import Test.Hspec (SpecWith, hspec)
import Test.Numeric.PositiveNatural qualified
import Test.Text.Megaparsec.Char.Extra qualified
import Test.Text.Megaparsec.Match qualified
import Test.Transcoder.Bcge qualified
import Test.Transcoder.Bcge.Hint qualified as BcgeHint
import Test.Transcoder.BcgeCC qualified
import Test.Transcoder.CharlesSchwab qualified
import Test.Transcoder.CharlesSchwab.Brokerage.Csv qualified
import Test.Transcoder.CharlesSchwab.Ledger qualified
import Test.Transcoder.Coop qualified
import Test.Transcoder.Coop.Config qualified
import Test.Transcoder.Coop.Receipt qualified
import Test.Transcoder.Data.LedgerReport qualified
import Test.Transcoder.Data.MyDecimal qualified
import Test.Transcoder.Degiro.AccountStatement qualified
import Test.Transcoder.Degiro.Csv qualified
import Test.Transcoder.Degiro.Portfolio qualified
import Test.Transcoder.EasyRide qualified
import Test.Transcoder.Finpension qualified
import Test.Transcoder.GPayslip qualified
import Test.Transcoder.GPayslip.PdfToText qualified
import Test.Transcoder.Galaxus qualified
import Test.Transcoder.GooglePlay qualified
import Test.Transcoder.Ib qualified
import Test.Transcoder.Ib.Csv.ActivityStatementParse qualified
import Test.Transcoder.Ib.Csv.RawParse qualified
import Test.Transcoder.Mbank (mbankTests)
import Test.Transcoder.Patreon qualified
import Test.Transcoder.Revolut qualified
import Test.Transcoder.Splitwise qualified
import Test.Transcoder.UberEats qualified

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  BcgeHint.tests
  mbankTests
  HDE.tests
  Test.Control.Applicative.Combinators.Extra.tests
  Test.Data.Csv.Extra.tests
  Test.Data.Time.Calendar.Extra.tests
  Test.Data.Time.Extra.tests
  Test.Hledger.Data.Lens.tests
  Test.Hledger.Data.MarketPrice.Extra.tests
  Test.Hledger.Extra.tests
  Test.Hledger.Read.TestUtils.tests
  Test.Numeric.PositiveNatural.tests
  Test.Transcoder.Bcge.tests
  Test.Transcoder.BcgeCC.tests
  Test.Transcoder.CharlesSchwab.tests
  Test.Transcoder.CharlesSchwab.Brokerage.Csv.tests
  Test.Transcoder.CharlesSchwab.Ledger.tests
  Test.Transcoder.Coop.tests
  Test.Transcoder.Coop.Config.tests
  Test.Transcoder.Coop.Receipt.tests
  Test.Transcoder.Data.LedgerReport.tests
  Test.Transcoder.Data.MyDecimal.tests
  Test.Transcoder.Degiro.AccountStatement.tests
  Test.Transcoder.Degiro.Csv.tests
  Test.Transcoder.Degiro.Portfolio.tests
  Test.Transcoder.EasyRide.tests
  Test.Transcoder.Finpension.tests
  Test.Transcoder.Galaxus.tests
  Test.Transcoder.GPayslip.PdfToText.tests
  Test.Transcoder.GPayslip.tests
  Test.Transcoder.GooglePlay.tests
  Test.Transcoder.Ib.Csv.ActivityStatementParse.tests
  Test.Transcoder.Ib.Csv.RawParse.tests
  Test.Transcoder.Ib.tests
  Test.Transcoder.Patreon.tests
  Test.Transcoder.Revolut.tests
  Test.Transcoder.Splitwise.tests
  Test.Transcoder.UberEats.tests
  Test.Text.Megaparsec.Char.Extra.tests
  Test.Text.Megaparsec.Match.tests
