module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime, utctDay)
import Hledger (Transaction)
import Hledger.Extra (showTransaction)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  long,
  metavar,
  progDesc,
  strOption,
  subparser,
 )
import Relude
import qualified Text.Megaparsec as MP
import Transcoder.Bcge (bcgeCsvToLedger)
import qualified Transcoder.Bcge.Hint as BcgeHint
import qualified Transcoder.BcgeCC as BcgeCC
import qualified Transcoder.CharlesSchwab as CharlesSchwab
import qualified Transcoder.Coop as Coop
import qualified Transcoder.Coop.Config as Coop
import Transcoder.Data.CsvFile (CsvFile (..))
import Transcoder.Data.LedgerReport (LedgerReport (..), showLedgerReport)
import qualified Transcoder.Degiro.AccountStatement as DegiroAccount (
  csvStatementToLedger,
 )
import qualified Transcoder.Degiro.Portfolio as DegiroPortfolio (
  csvStatementToLedger,
 )
import qualified Transcoder.EasyRide as EasyRide
import qualified Transcoder.Finpension as Finpension
import Transcoder.GPayslip (payslipTextToLedger)
import qualified Transcoder.Galaxus as Galaxus
import Transcoder.Ib as Ib (parseActivityCsv)
import Transcoder.Mbank (mbankCsvToLedger)
import qualified Transcoder.Patreon as Patreon
import qualified Transcoder.Revolut as Revolut
import qualified Transcoder.Splitwise as Splitwise
import qualified Transcoder.UberEats as UberEats

ledgerTrsToReport :: [Transaction] -> LedgerReport
ledgerTrsToReport = flip LedgerReport []

-- | Class of types that convert text representing valid Ledger transactions.
class ToLedgerText a where
  toLedgerText :: a -> Text

instance ToLedgerText Transaction where
  toLedgerText :: Transaction -> Text
  toLedgerText = showTransaction

instance ToLedgerText [Transaction] where
  toLedgerText :: [Transaction] -> Text
  toLedgerText = toLedgerText . ledgerTrsToReport

instance ToLedgerText LedgerReport where
  toLedgerText :: LedgerReport -> Text
  toLedgerText = showLedgerReport

-- | Reads from stdin and runs the provided parser.
parseBank :: (ToLedgerText l) => (LBS.ByteString -> Either Text l) -> IO ()
parseBank parser = do
  input <- LBS.getContents
  case parser input of
    Left err -> do
      T.hPutStr stderr err
      exitFailure
    Right output -> T.putStr . toLedgerText $ output

bankCommand :: Parser (IO ()) -> String -> ParserInfo (IO ())
bankCommand bankP = info (bankP <**> helper) . progDesc

bcgeCommand :: ParserInfo (IO ())
bcgeCommand = bankCommand (parseBcge <$> hintsFileOption) "Parses BCGE's CSV file and outputs ledupt data"
 where
  hintsFileOption = optional $ strOption (long "hints-file" <> metavar "FILE")

  parseBcgeHints :: FilePath -> IO (Maybe BcgeHint.Config)
  parseBcgeHints hintsFilePath = do
    contents <- decodeUtf8 <$> readFileLBS hintsFilePath
    return $ MP.parseMaybe BcgeHint.configParser contents

  parseBcge :: Maybe FilePath -> IO ()
  parseBcge maybeHintsFilePath = do
    hints :: Maybe BcgeHint.Config <-
      join
        <$> mapM parseBcgeHints maybeHintsFilePath
    parseBank $ bcgeCsvToLedger hints . decodeUtf8

bcgeCcCommand :: ParserInfo (IO ())
bcgeCcCommand =
  bankCommand
    (pure $ parseBank $ BcgeCC.rechnungToLedger . decodeUtf8)
    "Parses a text dump from a BCGE CC bill and outputs Ledger data."

coopCommand :: ParserInfo (IO ())
coopCommand =
  info
    ((parse <$> configFileOption) <**> helper)
    (progDesc "Parses a text dump from a Coop receipt and outputs Ledger data.")
 where
  configFileOption = optional $ strOption (long "config" <> metavar "FILE")
  parse coopConfigFilePath = do
    config <- case coopConfigFilePath of
      Nothing -> return Coop.emptyConfig
      Just filePath -> do
        eitherErrOrConfig <- Coop.decodeConfig <$> readFileLBS filePath
        case eitherErrOrConfig of
          Left err -> do
            T.hPutStr stderr err
            exitFailure
          Right output -> return output

    receipt <- T.getContents
    case Coop.receiptToLedger config receipt of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

charlesSchwabCommand :: ParserInfo (IO ())
charlesSchwabCommand = bankCommand (pure $ parseBank CharlesSchwab.csvToLedger) "Parses Charles Schwabs' CSV and outputs Ledger data."

degiroAccountCommand :: ParserInfo (IO ())
degiroAccountCommand =
  bankCommand
    (pure $ parseBank $ DegiroAccount.csvStatementToLedger . CsvFile)
    "Parses Degiro's account statement CSV and outputs Ledger data."

degiroPortfolioCommand :: ParserInfo (IO ())
degiroPortfolioCommand =
  bankCommand
    (pure parse)
    "Parses Degiro's portfolio CSV and outputs Ledger data"
 where
  parse = do
    today <- utctDay <$> getCurrentTime
    parseBank $ DegiroPortfolio.csvStatementToLedger today . CsvFile

easyRideCommand :: ParserInfo (IO ())
easyRideCommand =
  bankCommand
    (pure $ parseBank $ EasyRide.receiptToLedger . decodeUtf8)
    "Parses a text dump from an EasyRide receipt and outputs Ledger data."

finpensionPortfoliosTotalCommand :: ParserInfo (IO ())
finpensionPortfoliosTotalCommand = bankCommand (pure parse) "Parses Finpension's portoflio total and outputs a Ledger transaction."
 where
  parse = do
    today <- utctDay <$> getCurrentTime
    parseBank $ Finpension.parsePortfoliosTotal today . decodeUtf8

galaxusCommand :: ParserInfo (IO ())
galaxusCommand =
  bankCommand
    (pure $ parseBank $ Galaxus.parseReceipt . decodeUtf8)
    "Parses Galaxus' receipt and outputs a Ledger transaction."

gpayslipCommand :: ParserInfo (IO ())
gpayslipCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses a text dump from a Google Payslip and outputs Ledger data.")
 where
  parse = parseBank (payslipTextToLedger . decodeUtf8)

ibActivityCommand :: ParserInfo (IO ())
ibActivityCommand =
  bankCommand
    (pure $ parseBank (Ib.parseActivityCsv . decodeUtf8))
    "Parses IB's Activity Statement file and outputs a ledger."

mBankCommand :: ParserInfo (IO ())
mBankCommand =
  bankCommand
    (pure (parseBank mbankCsvToLedger))
    "Parses mBank's CSV file and outputs ledupt data."

patreonCommand :: ParserInfo (IO ())
patreonCommand =
  bankCommand
    (pure $ parseBank $ Patreon.receiptToLedger . decodeUtf8)
    "Parses a text dump from a Patreon receipt and outputs Ledger data."

revolutCommand :: ParserInfo (IO ())
revolutCommand =
  bankCommand
    (pure (parseBank Revolut.parseCsvToLedger))
    "Parses Revolut's CSV file and outputs ledger data."

splitwiseCommand :: ParserInfo (IO ())
splitwiseCommand =
  bankCommand
    (pure parseSplitwise)
    "Parses Splitwise's CSV file and outputs ledger data."
 where
  parseSplitwise :: IO ()
  parseSplitwise = do
    today <- utctDay <$> getCurrentTime
    parseBank (Splitwise.statementToLedger today . CsvFile)

uberEatsCommand :: ParserInfo (IO ())
uberEatsCommand =
  bankCommand
    (pure (parseBank $ UberEats.parseBill . decodeUtf8))
    "Parses Uber Eats' payment line and outputs a Ledger transaction."

commands :: Parser (IO ())
commands =
  subparser
    ( command "parse-bcge" bcgeCommand
        <> command "parse-bcge-cc" bcgeCcCommand
        <> command "parse-coop" coopCommand
        <> command "parse-cs" charlesSchwabCommand
        <> command "parse-degiro-account" degiroAccountCommand
        <> command "parse-degiro-portfolio" degiroPortfolioCommand
        <> command "parse-easy-ride" easyRideCommand
        <> command "parse-finpension" finpensionPortfoliosTotalCommand
        <> command "parse-galaxus" galaxusCommand
        <> command "parse-gpayslip" gpayslipCommand
        <> command "parse-ib-activity" ibActivityCommand
        <> command "parse-mbank" mBankCommand
        <> command "parse-patreon" patreonCommand
        <> command "parse-revolut" revolutCommand
        <> command "parse-splitwise" splitwiseCommand
        <> command "parse-uber-eats" uberEatsCommand
    )

main :: IO ()
main = join $ execParser programInfo
 where
  programInfo :: ParserInfo (IO ())
  programInfo =
    info
      (commands <**> helper)
      ( fullDesc
          <> header "findata-transcoder"
          <> progDesc "Parses financial data into a ledger-like text file."
      )
