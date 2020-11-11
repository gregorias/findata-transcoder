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
import Mbank (serializeTransactionsToLedgerDat)
import Lib
import System.Exit (exitFailure)

outputFileParser :: String -> Either String String
outputFileParser "" = Left "The provided output file is empty."
outputFileParser s = Right s

outputFileFlagName = "output_file"

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

cliAction :: FlagParam String -> String -> OptionDesc (IO ()) ()
cliAction outputFileFlag ledgerDat = action
    $ \toParam -> printError $ do
      (outputFile :: String) <- maybeToExcept (toParam outputFileFlag) ("Provide " ++ outputFileFlagName)
      liftIO . putStrLn $ "Opening " ++ outputFile ++ " for writing."
      liftIO $ writeFile outputFile ledgerDat


main :: IO ()
main = defaultMain $ do
  programName "hledupt"
  programVersion $ makeVersion [0, 1, 0, 0]
  programDescription "A program to fetch financial data into a ledger-like text file"
  outputFileFlag <- flagParam (FlagLong outputFileFlagName) (FlagRequired outputFileParser)
  command "pull-mbank" $ cliAction outputFileFlag (serializeTransactionsToLedgerDat [])
  command "pull-ib" $ cliAction outputFileFlag (serializeTransactionsToLedgerDat [])
