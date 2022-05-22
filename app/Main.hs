module Main (main) where

import Console.Options (
  FlagFrag (FlagDescription, FlagLong),
  FlagParam,
  FlagParser (FlagOptional),
  OptionDesc,
  action,
  command,
  defaultMain,
  description,
  flagParam,
  programDescription,
  programName,
  programVersion,
 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Version (makeVersion)
import Hledger (Transaction)
import Hledger.Extra (showTransaction)
import Relude
import qualified Text.Megaparsec as MP
import Transcoder.Bcge (bcgeCsvToLedger)
import qualified Transcoder.Bcge.Hint as BcgeHint
import qualified Transcoder.BcgeCC as BcgeCC
import qualified Transcoder.CharlesSchwab as CharlesSchwab (csvToLedger)
import qualified Transcoder.Coop as Coop
import qualified Transcoder.Coop.Config as Coop
import Transcoder.Data.CsvFile (CsvFile (..))
import Transcoder.Data.LedgerReport (
  LedgerReport (..),
  showLedgerReport,
 )
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

filenameParser :: String -> Either String String
filenameParser "" = Left "The provided output filename is empty."
filenameParser s = Right s

hintsFileFlagName :: String
hintsFileFlagName = "hints_file"

ledgerTrsToReport :: [Transaction] -> LedgerReport
ledgerTrsToReport = flip LedgerReport []

type HintsFileFlag = FlagParam (Maybe FilePath)

parseBcgeHints :: FilePath -> IO (Maybe BcgeHint.Config)
parseBcgeHints hintsFilePath = do
  contents <- readFile hintsFilePath
  return $ MP.parseMaybe BcgeHint.configParser contents

parseBcgeAction :: HintsFileFlag -> OptionDesc (IO ()) ()
parseBcgeAction hintsFileFlag = action $
  \toParam -> do
    (hintsFilePath :: Maybe FilePath) <- return $ join (toParam hintsFileFlag)
    liftIO $ parseBcge hintsFilePath

parseBank :: (LBS.ByteString -> Either Text LedgerReport) -> IO ()
parseBank parser = do
  input <- LBS.getContents
  case parser input of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> Text.putStr . showLedgerReport $ output

parseBcge :: Maybe FilePath -> IO ()
parseBcge maybeHintsFilePath = do
  hints :: Maybe BcgeHint.Config <-
    join
      <$> mapM parseBcgeHints maybeHintsFilePath
  parseBank $ bcgeCsvToLedger hints . decodeUtf8

parseCharlesSchwab :: IO ()
parseCharlesSchwab =
  parseBank CharlesSchwab.csvToLedger

parseDegiroAccount :: IO ()
parseDegiroAccount =
  parseBank $
    DegiroAccount.csvStatementToLedger . CsvFile

parseDegiroPortfolio :: IO ()
parseDegiroPortfolio = do
  today <- utctDay <$> getCurrentTime
  parseBank $
    DegiroPortfolio.csvStatementToLedger today . CsvFile

parseFinpensionTransactions :: IO ()
parseFinpensionTransactions =
  parseBank $ fmap ledgerTrsToReport . Finpension.transactionsToLedger . CsvFile

parseIbActivity :: IO ()
parseIbActivity =
  parseBank
    ( Ib.parseActivityCsv
        . decodeUtf8
    )

parseMbank :: IO ()
parseMbank =
  parseBank $
    mbankCsvToLedger . decodeUtf8

parseRevolut :: IO ()
parseRevolut = parseBank Revolut.parseCsvToLedger

parseSplitwise :: IO ()
parseSplitwise = do
  statement <- LBS.getContents
  today <- utctDay <$> getCurrentTime
  case Splitwise.statementToLedger today (CsvFile statement) of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (ledgerTrsToReport . one) $ output

parseGPayslip :: IO ()
parseGPayslip = do
  payslip <- Text.getContents
  case payslipTextToLedger payslip of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (ledgerTrsToReport . one) $ output

parseBcgeCC :: IO ()
parseBcgeCC = do
  rechnung <- Text.getContents
  case BcgeCC.rechnungToLedger rechnung of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . ledgerTrsToReport $ output

parseCoop :: Maybe FilePath -> IO ()
parseCoop coopConfigFilePath = do
  config <- case coopConfigFilePath of
    Nothing -> return Coop.emptyConfig
    Just filePath -> do
      eitherErrOrConfig <- Coop.decodeConfig <$> readFileLBS filePath
      case eitherErrOrConfig of
        Left err -> do
          Text.hPutStr stderr err
          exitFailure
        Right output -> return output

  receipt <- Text.getContents
  case Coop.receiptToLedger config receipt of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

parseEasyRide :: IO ()
parseEasyRide = do
  receipt <- Text.getContents
  case EasyRide.receiptToLedger receipt of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

parseGalaxus :: IO ()
parseGalaxus = do
  receipt <- Text.getContents
  case Galaxus.parseReceipt receipt of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showTransaction $ output

parsePatreon :: IO ()
parsePatreon = do
  receipt <- Text.getContents
  case Patreon.receiptToLedger receipt of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

parseUberEats :: IO ()
parseUberEats = do
  bill <- Text.getContents
  case UberEats.parseBill bill of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showTransaction $ output

{- HLINT ignore ignoreAction -}
ignoreAction :: r -> OptionDesc r ()
ignoreAction r = action $ \_ -> r

main :: IO ()
main = defaultMain $ do
  programName "findata-transcoder"
  programVersion $ makeVersion [0, 2, 0, 0]
  programDescription "A program to parse financial data into a ledger-like text file"
  command "parse-bcge" $ do
    description "Parses BCGE's CSV file and outputs ledupt data"
    hintsFileFlag <- flagParam (FlagLong hintsFileFlagName) (FlagOptional Nothing (fmap Just . filenameParser))
    parseBcgeAction hintsFileFlag
  command "parse-bcge-cc" $ do
    description "Parses a text dump from a BCGE CC bill and outputs Ledger data"
    ignoreAction parseBcgeCC
  command "parse-coop" $ do
    description "Parses a text dump from a Coop receipt and outputs Ledger data"
    coopConfigFlag <- flagParam (FlagDescription "A JSON config" <> FlagLong "config") (FlagOptional Nothing (fmap Just . filenameParser))
    action $ \toParam -> do
      (coopConfigFilePath :: Maybe FilePath) <- return $ join (toParam coopConfigFlag)
      parseCoop coopConfigFilePath
  command "parse-cs" $ do
    description "Parses Charles Schwabs' CSV and outputs Ledger data"
    ignoreAction parseCharlesSchwab
  command "parse-degiro-account" $ do
    description "Parses Degiro's account statement CSV and outputs Ledger data"
    ignoreAction parseDegiroAccount
  command "parse-degiro-portfolio" $ do
    description "Parses Degiro's portfolio CSV and outputs Ledger data"
    ignoreAction parseDegiroPortfolio
  command "parse-easy-ride" $ do
    description "Parses a text dump from an EasyRide receipt and outputs Ledger data"
    ignoreAction parseEasyRide
  command "parse-finpension-transactions" $ do
    description "Parses Finpensions' transaction CSV and outputs Ledger data"
    ignoreAction parseFinpensionTransactions
  command "parse-galaxus" $ do
    description "Parses Galaxus' receipt and outputs a Ledger transaction."
    ignoreAction parseGalaxus
  command "parse-gpayslip" $ do
    description "Parses a text dump from a Google Payslip and outputs Ledger data"
    ignoreAction parseGPayslip
  command "parse-ib-activity" $ do
    description "Parses IB's Activity Statement file and outputs a ledger"
    ignoreAction parseIbActivity
  command "parse-mbank" $ do
    description "Parses mBank's CSV file and outputs ledupt data"
    ignoreAction parseMbank
  command "parse-patreon" $ do
    description "Parses a text dump from a Patreon receipt and outputs Ledger data"
    ignoreAction parsePatreon
  command "parse-revolut" $ do
    description "Parses Revolut's CSV file and outputs ledger data"
    ignoreAction parseRevolut
  command "parse-splitwise" $ do
    description "Parses Splitwise's CSV file and outputs ledger data"
    ignoreAction parseSplitwise
  command "parse-uber-eats" $ do
    description "Parses Uber Eats' payment line and outputs a Ledger transaction."
    ignoreAction parseUberEats
