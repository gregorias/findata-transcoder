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
import qualified Transcoder.Finpension.Transactions as Finpension
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

-- | Reads from stdin and runs the provided parser.
parseBank :: (LBS.ByteString -> Either Text LedgerReport) -> IO ()
parseBank parser = do
  input <- LBS.getContents
  case parser input of
    Left err -> do
      T.hPutStr stderr err
      exitFailure
    Right output -> T.putStr . showLedgerReport $ output

parseBcgeHints :: FilePath -> IO (Maybe BcgeHint.Config)
parseBcgeHints hintsFilePath = do
  contents <- readFile hintsFilePath
  return $ MP.parseMaybe BcgeHint.configParser contents

parseBcge :: Maybe FilePath -> IO ()
parseBcge maybeHintsFilePath = do
  hints :: Maybe BcgeHint.Config <-
    join
      <$> mapM parseBcgeHints maybeHintsFilePath
  parseBank $ bcgeCsvToLedger hints . decodeUtf8

bcgeCommand :: ParserInfo (IO ())
bcgeCommand =
  info
    ((parseBcge <$> hintsFileOption) <**> helper)
    (progDesc "Parses BCGE's CSV file and outputs ledupt data")
 where
  hintsFileOption = optional $ strOption (long "hints-file" <> metavar "FILE")

parseBcgeCC :: IO ()
parseBcgeCC = do
  rechnung <- T.getContents
  case BcgeCC.rechnungToLedger rechnung of
    Left err -> do
      T.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . ledgerTrsToReport $ output

bcgeCcCommand :: ParserInfo (IO ())
bcgeCcCommand =
  info
    (pure parseBcgeCC <**> helper)
    (progDesc "Parses a text dump from a BCGE CC bill and outputs Ledger data.")

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
charlesSchwabCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses Charles Schwabs' CSV and outputs Ledger data.")
 where
  parse = parseBank CharlesSchwab.csvToLedger

degiroAccountCommand :: ParserInfo (IO ())
degiroAccountCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses Degiro's account statement CSV and outputs Ledger data.")
 where
  parse = parseBank $ DegiroAccount.csvStatementToLedger . CsvFile

degiroPortfolioCommand :: ParserInfo (IO ())
degiroPortfolioCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses Degiro's portfolio CSV and outputs Ledger data")
 where
  parse = do
    today <- utctDay <$> getCurrentTime
    parseBank $ DegiroPortfolio.csvStatementToLedger today . CsvFile

easyRideCommand :: ParserInfo (IO ())
easyRideCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses a text dump from an EasyRide receipt and outputs Ledger data.")
 where
  parse = do
    receipt <- T.getContents
    case EasyRide.receiptToLedger receipt of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

finpensionTransactionsCommand :: ParserInfo (IO ())
finpensionTransactionsCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses Finpension's transaction CSV and outputs Ledger data.")
 where
  parse = parseBank $ fmap ledgerTrsToReport . Finpension.transactionsToLedger . CsvFile

galaxusCommand :: ParserInfo (IO ())
galaxusCommand =
  info
    (pure parseGalaxus <**> helper)
    (progDesc "Parses Galaxus' receipt and outputs a Ledger transaction.")
 where
  parseGalaxus = do
    receipt <- T.getContents
    case Galaxus.parseReceipt receipt of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showTransaction $ output

gpayslipCommand :: ParserInfo (IO ())
gpayslipCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses a text dump from a Google Payslip and outputs Ledger data.")
 where
  parse = do
    payslip <- T.getContents
    case payslipTextToLedger payslip of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showLedgerReport . (ledgerTrsToReport . one) $ output

ibActivityCommand :: ParserInfo (IO ())
ibActivityCommand =
  info
    (pure parse <**> helper)
    (progDesc "Parses IB's Activity Statement file and outputs a ledger.")
 where
  parse = parseBank (Ib.parseActivityCsv . decodeUtf8)

mBankCommand :: ParserInfo (IO ())
mBankCommand =
  info
    (pure parseMBank <**> helper)
    (progDesc "Parses mBank's CSV file and outputs ledupt data.")
 where
  parseMBank :: IO ()
  parseMBank = parseBank $ mbankCsvToLedger . decodeUtf8

patreonCommand :: ParserInfo (IO ())
patreonCommand =
  info
    (pure parsePatreon <**> helper)
    (progDesc "Parses a text dump from a Patreon receipt and outputs Ledger data.")
 where
  parsePatreon :: IO ()
  parsePatreon = do
    receipt <- T.getContents
    case Patreon.receiptToLedger receipt of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

revolutCommand :: ParserInfo (IO ())
revolutCommand =
  info
    (pure parseRevolut <**> helper)
    (progDesc "Parses Revolut's CSV file and outputs ledger data.")
 where
  parseRevolut :: IO ()
  parseRevolut =
    parseBank Revolut.parseCsvToLedger

splitwiseCommand :: ParserInfo (IO ())
splitwiseCommand =
  info
    (pure parseSplitwise <**> helper)
    (progDesc "Parses Splitwise's CSV file and outputs ledger data.")
 where
  parseSplitwise :: IO ()
  parseSplitwise = do
    statement <- LBS.getContents
    today <- utctDay <$> getCurrentTime
    case Splitwise.statementToLedger today (CsvFile statement) of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showLedgerReport . (ledgerTrsToReport . one) $ output

uberEatsCommand :: ParserInfo (IO ())
uberEatsCommand =
  info
    (pure parseUberEats <**> helper)
    (progDesc "Parses Uber Eats' payment line and outputs a Ledger transaction.")
 where
  parseUberEats :: IO ()
  parseUberEats = do
    bill <- T.getContents
    case UberEats.parseBill bill of
      Left err -> do
        T.hPutStr stderr err
        exitFailure
      Right output -> putText . showTransaction $ output

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
        <> command "parse-finpension-transactions" finpensionTransactionsCommand
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
