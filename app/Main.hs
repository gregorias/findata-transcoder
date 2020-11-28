{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Console.Options
  ( FlagFrag (FlagLong),
    FlagParam,
    FlagParser (FlagOptional, FlagRequired),
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
import Control.Monad (join)
import Control.Monad.Except
  ( ExceptT,
    runExceptT,
    throwError,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Version (makeVersion)
import Hledupt.Bcge (bcgeCsvToLedger)
import qualified Hledupt.Bcge.Hint as BcgeHint
import Hledupt.Ib (ibCsvToLedger)
import Hledupt.Mbank (mbankCsvToLedger)
import Main.Utf8 (withUtf8)
import System.Exit (exitFailure)
import qualified Text.Megaparsec as MP

filenameParser :: String -> Either String String
filenameParser "" = Left "The provided output filename is empty."
filenameParser s = Right s

maybeToExcept ::
  (Monad m) =>
  Maybe a ->
  e ->
  ExceptT e m a
maybeToExcept ma e =
  case ma of
    Nothing -> throwError e
    Just a -> return a

printError :: ExceptT String IO () -> IO ()
printError me = do
  eitherError <- runExceptT me
  case eitherError of
    Left errorMsg -> do
      putStrLn errorMsg
      exitFailure
    Right _ -> return ()

inputFileFlagName :: String
inputFileFlagName = "input_file"

hintsFileFlagName :: String
hintsFileFlagName = "hints_file"

type LedgerParser = String -> String

type IOParser = FilePath -> IO ()

parseBankIO :: LedgerParser -> IOParser
parseBankIO ledgerParser inputFilePath = withUtf8 $ do
  inputFile <- readFile inputFilePath
  putStr $ ledgerParser inputFile

parseBankAction :: LedgerParser -> FlagParam FilePath -> OptionDesc (IO ()) ()
parseBankAction ledgerParser inputFileFlag = action $
  \toParam -> printError $ do
    (inputFilePath :: FilePath) <- maybeToExcept (toParam inputFileFlag) ("Provide " ++ inputFileFlagName)
    liftIO $ parseBankIO ledgerParser inputFilePath

type InputFileFlag = FlagParam FilePath

type HintsFileFlag = FlagParam (Maybe FilePath)

data BcgeFlags = BcgeFlags
  { bcgeFlagsInputFile :: InputFileFlag,
    bcgeFlagsHintsFile :: HintsFileFlag
  }

data BcgeOptions = BcgeOptions
  { bcgeOptionsInputFile :: FilePath,
    bcgeOptionsHintsFile :: Maybe FilePath
  }

parseBcgeHints :: FilePath -> IO BcgeHint.Config
parseBcgeHints hintsFilePath = do
  contents <- readFile hintsFilePath
  let Just config = MP.parseMaybe BcgeHint.configParser contents
  return config

parseBcgeIO :: BcgeOptions -> IO ()
parseBcgeIO bcgeOptions = withUtf8 $ do
  inputFile <- readFile $ bcgeOptionsInputFile bcgeOptions
  let maybeHintsFilePath = bcgeOptionsHintsFile bcgeOptions
  hints :: Maybe BcgeHint.Config <- mapM parseBcgeHints maybeHintsFilePath
  putStr $ bcgeCsvToLedger hints inputFile

parseBcgeAction :: BcgeFlags -> OptionDesc (IO ()) ()
parseBcgeAction bcgeFlags = action $
  \toParam -> printError $ do
    (inputFilePath :: FilePath) <- maybeToExcept (toParam $ bcgeFlagsInputFile bcgeFlags) ("Provide " ++ inputFileFlagName)
    (hintsFilePath :: Maybe FilePath) <-
      return $ join (toParam $ bcgeFlagsHintsFile bcgeFlags :: Maybe (Maybe FilePath))
    liftIO $ parseBcgeIO (BcgeOptions inputFilePath hintsFilePath)

main :: IO ()
main = defaultMain $ do
  programName "hledupt"
  programVersion $ makeVersion [0, 1, 0, 0]
  programDescription "A program to parse financial data into a ledger-like text file"
  command "parse-bcge" $ do
    description "Parses BCGE's CSV file and outputs ledupt data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    hintsFileFlag <- flagParam (FlagLong hintsFileFlagName) (FlagOptional Nothing (fmap Just . filenameParser))
    parseBcgeAction $ BcgeFlags inputFileFlag hintsFileFlag
  command "parse-mbank" $ do
    description "Parses IB's CSV file and outputs ledupt data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    parseBankAction ibCsvToLedger inputFileFlag
  command "parse-mbank" $ do
    description "Parses mBank's CSV file and outputs ledupt data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    parseBankAction mbankCsvToLedger inputFileFlag
