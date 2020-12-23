import Relude
import qualified Test.Data.Csv.Extra
import qualified Test.Hledger.Data.Extra as HDE
import qualified Test.Hledger.Data.Lens
import qualified Test.Hledger.Data.MarketPrice.Extra
import qualified Test.Hledger.Read.TestUtils
import qualified Test.Hledupt.Bcge
import qualified Test.Hledupt.Bcge.Hint as BcgeHint
import qualified Test.Hledupt.Data
import qualified Test.Hledupt.Degiro
import qualified Test.Hledupt.Degiro.Csv
import qualified Test.Hledupt.Ib
import qualified Test.Hledupt.Ib.Csv.ActivityStatementParse
import qualified Test.Hledupt.Ib.Csv.RawParse
import Test.Hledupt.Mbank (mbankTests)
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
  Test.Hledupt.Data.dataTests
  Test.Hledupt.Degiro.tests
  Test.Hledupt.Degiro.Csv.tests
  Test.Hledupt.Ib.Csv.ActivityStatementParse.tests
  Test.Hledupt.Ib.Csv.RawParse.tests
  Test.Hledupt.Ib.tests
  Test.Text.Megaparsec.Char.Extra.tests
  Test.Text.Megaparsec.Match.tests
