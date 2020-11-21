{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Bcge (bcgeCsvToLedger)
import qualified Bcge.Hint as BcgeHint
import Console.Options
  ( FlagFrag (FlagLong, FlagShort),
    FlagParam,
    FlagParser (FlagOptional, FlagRequired),
    OptionDesc,
    action,
    command,
    defaultMain,
    description,
    flag,
    flagParam,
    programDescription,
    programName,
    programVersion,
    remainingArguments,
  )
import Control.Monad (join)
import Control.Monad.Except
  ( ExceptT,
    catchError,
    runExceptT,
    throwError,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Version (makeVersion)
import Main.Utf8 (withUtf8)
import Mbank (mbankCsvToLedger)
import System.Exit (exitFailure)
import System.IO (hGetContents)
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
    Left error -> do
      putStrLn error
      exitFailure
    Right _ -> return ()

inputFileFlagName = "input_file"

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
    description "Parses BCGE's CSV file and outputs debug data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    hintsFileFlag <- flagParam (FlagLong hintsFileFlagName) (FlagOptional Nothing (fmap Just . filenameParser))
    parseBcgeAction $ BcgeFlags inputFileFlag hintsFileFlag
  command "parse-mbank" $ do
    description "Parses mBank's CSV file and outputs debug data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    parseBankAction mbankCsvToLedger inputFileFlag
