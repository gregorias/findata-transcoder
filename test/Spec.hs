module Spec (main) where

import Relude
import qualified Test.Data.Csv.Extra
import qualified Test.Hledger.Data.Extra as HDE
import qualified Test.Hledger.Data.Lens
import qualified Test.Hledger.Data.MarketPrice.Extra
import qualified Test.Hledger.Read.TestUtils
import qualified Test.Hledupt.Bcge
import qualified Test.Hledupt.Bcge.Hint as BcgeHint
import qualified Test.Hledupt.BcgeCC
import qualified Test.Hledupt.CharlesSchwab.Csv
import qualified Test.Hledupt.CharlesSchwab.Ledger
import qualified Test.Hledupt.Coop
import qualified Test.Hledupt.Coop.Config
import qualified Test.Hledupt.Data.LedgerReport
import qualified Test.Hledupt.Data.MyDecimal
import qualified Test.Hledupt.Degiro.AccountStatement
import qualified Test.Hledupt.Degiro.Csv
import qualified Test.Hledupt.Degiro.Portfolio
import qualified Test.Hledupt.EasyRide
import qualified Test.Hledupt.Finpension
import qualified Test.Hledupt.GPayslip
import qualified Test.Hledupt.Ib
import qualified Test.Hledupt.Ib.Csv.ActivityStatementParse
import qualified Test.Hledupt.Ib.Csv.RawParse
import Test.Hledupt.Mbank (mbankTests)
import qualified Test.Hledupt.Patreon
import qualified Test.Hledupt.Revolut
import qualified Test.Hledupt.Splitwise
import qualified Test.Hledupt.UberEats
import Test.Hspec (SpecWith, hspec)
import qualified Test.Text.Megaparsec.Char.Extra
import qualified Test.Text.Megaparsec.Match

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  BcgeHint.tests
  mbankTests
  HDE.tests
  Test.Data.Csv.Extra.tests
  Test.Hledger.Data.Lens.tests
  Test.Hledger.Data.MarketPrice.Extra.tests
  Test.Hledger.Read.TestUtils.tests
  Test.Hledupt.Bcge.tests
  Test.Hledupt.BcgeCC.tests
  Test.Hledupt.CharlesSchwab.Csv.tests
  Test.Hledupt.CharlesSchwab.Ledger.tests
  Test.Hledupt.Coop.tests
  Test.Hledupt.Coop.Config.tests
  Test.Hledupt.Data.LedgerReport.tests
  Test.Hledupt.Data.MyDecimal.tests
  Test.Hledupt.Degiro.AccountStatement.tests
  Test.Hledupt.Degiro.Csv.tests
  Test.Hledupt.Degiro.Portfolio.tests
  Test.Hledupt.EasyRide.tests
  Test.Hledupt.Finpension.tests
  Test.Hledupt.GPayslip.tests
  Test.Hledupt.Ib.Csv.ActivityStatementParse.tests
  Test.Hledupt.Ib.Csv.RawParse.tests
  Test.Hledupt.Ib.tests
  Test.Hledupt.Patreon.tests
  Test.Hledupt.Revolut.tests
  Test.Hledupt.Splitwise.tests
  Test.Hledupt.UberEats.tests
  Test.Text.Megaparsec.Char.Extra.tests
  Test.Text.Megaparsec.Match.tests
