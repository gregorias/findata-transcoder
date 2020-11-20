{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Bcge (bcgeCsvToLedger)
import Console.Options
  ( FlagFrag (FlagLong, FlagShort),
    FlagParam,
    FlagParser (FlagRequired),
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

main :: IO ()
main = defaultMain $ do
  programName "hledupt"
  programVersion $ makeVersion [0, 1, 0, 0]
  programDescription "A program to parse financial data into a ledger-like text file"
  command "parse-bcge" $ do
    description "Parses BCGE's CSV file and outputs debug data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    parseBankAction (bcgeCsvToLedger Nothing) inputFileFlag
  command "parse-mbank" $ do
    description "Parses mBank's CSV file and outputs debug data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    parseBankAction mbankCsvToLedger inputFileFlag
