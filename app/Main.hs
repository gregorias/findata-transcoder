module Main (main) where

import Console.Options (
  FlagFrag (FlagLong),
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
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 as UTF8 (toString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Version (makeVersion)
import Hledupt.Bcge (bcgeCsvToLedger)
import qualified Hledupt.Bcge.Hint as BcgeHint
import Hledupt.Data.CsvFile (CsvFile (..))
import Hledupt.Data.LedgerReport (
  LedgerReport,
  showLedgerReport,
 )
import qualified Hledupt.Degiro.AccountStatement as DegiroAccount (
  csvStatementToLedger,
 )
import qualified Hledupt.Degiro.Portfolio as DegiroPortfolio (
  csvStatementToLedger,
 )
import Hledupt.Ib as Ib (parseActivityCsv)
import Hledupt.Mbank (mbankCsvToLedger)
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
  parseBank $ bcgeCsvToLedger hints . UTF8.toString

parseDegiroAccountStatement :: IO ()
parseDegiroAccountStatement =
  parseBank $
    DegiroAccount.csvStatementToLedger . CsvFile

parseDegiroPortfolio :: IO ()
parseDegiroPortfolio = do
  today <- utctDay <$> getCurrentTime
  parseBank $
    DegiroPortfolio.csvStatementToLedger today . CsvFile

parseIbActivity :: IO ()
parseIbActivity =
  parseBank
    ( L.over L._Left Text.pack
        . Ib.parseActivityCsv
        . UTF8.toString
    )

parseMbank :: IO ()
parseMbank =
  parseBank $
    mbankCsvToLedger . UTF8.toString

-- HLint would like to change \_ -> r to const r which leads to
-- ambiguous type variable error. I can't fix the error in GHC 8.10.2:
-- `const @_ @(Flag Bool -> Bool)` gives an error.
{-# ANN ignoreAction "HLint: ignore" #-}
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
  command "parse-degiro-account-statement" $ do
    description "Parses Degiro's account statement CSV and outputs Ledger data"
    ignoreAction parseDegiroAccountStatement
  command "parse-degiro-portfolio" $ do
    description "Parses Degiro's portfolio CSV and outputs Ledger data"
    ignoreAction parseDegiroPortfolio
  command "parse-ib-activity" $ do
    description "Parses IB's Activity Statement file and outputs ledupt data"
    ignoreAction parseIbActivity
  command "parse-mbank" $ do
    description "Parses mBank's CSV file and outputs ledupt data"
    ignoreAction parseMbank
