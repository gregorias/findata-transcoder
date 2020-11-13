{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Console.Options
  ( FlagFrag (FlagLong, FlagShort),
    FlagParser (FlagRequired),
    FlagParam,
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
import Mbank (parseMbankCsv)
import Lib
import System.Exit (exitFailure)
import System.IO (
  hGetContents,)

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

parseMbankIO :: FilePath -> IO ()
parseMbankIO inputFilePath = do
  inputFile <- readFile inputFilePath
  putStr $ parseMbankCsv inputFile


parseMbankAction :: FlagParam FilePath -> OptionDesc (IO ()) ()
parseMbankAction inputFileFlag = action
  $ \toParam -> printError $ do
      (inputFilePath :: FilePath) <- maybeToExcept (toParam inputFileFlag) ("Provide " ++ inputFileFlagName)
      liftIO $ parseMbankIO inputFilePath


main :: IO ()
main = defaultMain $ do
  programName "hledupt"
  programVersion $ makeVersion [0, 1, 0, 0]
  programDescription "A program to parse financial data into a ledger-like text file"
  command "parse-mbank" $ do
    description "Parses mBank's CSV file and outputs debug data"
    inputFileFlag <- flagParam (FlagLong inputFileFlagName) (FlagRequired filenameParser)
    parseMbankAction inputFileFlag
