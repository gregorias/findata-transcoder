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
import Hledupt.Bcge (bcgeCsvToLedger)
import qualified Hledupt.Bcge.Hint as BcgeHint
import qualified Hledupt.BcgeCC as BcgeCC
import qualified Hledupt.CharlesSchwab as CharlesSchwab (csvToLedger)
import qualified Hledupt.Coop as Coop
import qualified Hledupt.Coop.Config as Coop
import Hledupt.Data.CsvFile (CsvFile (..))
import Hledupt.Data.LedgerReport (
  LedgerReport (..),
  showLedgerReport,
 )
import qualified Hledupt.Degiro.AccountStatement as DegiroAccount (
  csvStatementToLedger,
 )
import qualified Hledupt.Degiro.Portfolio as DegiroPortfolio (
  csvStatementToLedger,
 )
import qualified Hledupt.EasyRide as EasyRide
import qualified Hledupt.Finpension as Finpension
import Hledupt.GPayslip (payslipTextToLedger)
import Hledupt.Ib as Ib (parseActivityCsv)
import Hledupt.Mbank (mbankCsvToLedger)
import qualified Hledupt.Patreon as Patreon
import qualified Hledupt.Revolut as Revolut
import Relude
import qualified Text.Megaparsec as MP

filenameParser :: String -> Either String String
filenameParser "" = Left "The provided output filename is empty."
filenameParser s = Right s

hintsFileFlagName :: String
hintsFileFlagName = "hints_file"

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

parseDegiroAccountStatement :: IO ()
parseDegiroAccountStatement =
  parseBank $
    DegiroAccount.csvStatementToLedger . CsvFile

parseDegiroPortfolio :: IO ()
parseDegiroPortfolio = do
  today <- utctDay <$> getCurrentTime
  parseBank $
    DegiroPortfolio.csvStatementToLedger today . CsvFile

parseFinpensionTransactions :: IO ()
parseFinpensionTransactions =
  parseBank $
    Finpension.transactionsToLedger . CsvFile

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

parseGPayslip :: IO ()
parseGPayslip = do
  payslip <- Text.getContents
  case payslipTextToLedger payslip of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

parseBcgeCC :: IO ()
parseBcgeCC = do
  rechnung <- Text.getContents
  case BcgeCC.rechnungToLedger rechnung of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (`LedgerReport` []) $ output

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

parsePatreon :: IO ()
parsePatreon = do
  receipt <- Text.getContents
  case Patreon.receiptToLedger receipt of
    Left err -> do
      Text.hPutStr stderr err
      exitFailure
    Right output -> putText . showLedgerReport . (flip LedgerReport [] . one) $ output

{- HLINT ignore ignoreAction -}
ignoreAction :: r -> OptionDesc r ()
ignoreAction r = action $ \_ -> r

main :: IO ()
main = defaultMain $ do
  programName "hledupt"
  programVersion $ makeVersion [0, 1, 0, 0]
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
  command "parse-degiro-account-statement" $ do
    description "Parses Degiro's account statement CSV and outputs Ledger data"
    ignoreAction parseDegiroAccountStatement
  command "parse-degiro-portfolio" $ do
    description "Parses Degiro's portfolio CSV and outputs Ledger data"
    ignoreAction parseDegiroPortfolio
  command "parse-easy-ride" $ do
    description "Parses a text dump from an EasyRide receipt and outputs Ledger data"
    ignoreAction parseEasyRide
  command "parse-finpension-transactions" $ do
    description "Parses Finpensions' transaction CSV and outputs Ledger data"
    ignoreAction parseFinpensionTransactions
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
