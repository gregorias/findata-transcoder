{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module does a first-pass parsing of an IB statement CSV
-- into individual CSVs.
module Hledupt.Ib.Csv.RawParse
  ( Statement (..),
    Section (..),
    Csv (..),
    parse,
  )
where

import qualified Control.Lens as L
import Data.List.NonEmpty (some1)
import qualified Data.Map as Map
import Relude
import Text.Megaparsec
  ( MonadParsec (eof, label, lookAhead, takeWhileP, try),
    ParseErrorBundle (bundleErrors),
    Parsec,
    Stream,
    VisualStream,
    anySingle,
    errorBundlePretty,
    manyTill,
    parseErrorPretty,
    satisfy,
    single,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Extra (bom)

newtype Statement = Statement
  { unStatement :: Map.Map String Section
  }
  deriving newtype (Eq, Show)

newtype Section = Section
  { unSection :: NonEmpty Csv
  }
  deriving newtype (Eq, Show)

-- TODO IsList also doesn't work

newtype Csv = Csv
  { unCsv :: String
  }
  deriving newtype (IsString, Eq, Show)

data IbCsvLine = IbCsvLine
  { iclSection :: String,
    iclHeader :: String,
    iclRest :: String
  }
  deriving (Eq, Ord)

data Hord = Header | Data
  deriving (Eq, Ord, Show)

data IbCsvHordLine = IbCsvHordLine
  { ichlSection :: String,
    ichlHeader :: Hord,
    ichlRest :: String
  }
  deriving (Eq, Ord)

toMaybeHordLine :: IbCsvLine -> Maybe IbCsvHordLine
toMaybeHordLine
  IbCsvLine
    { iclSection = sec,
      iclHeader = header,
      iclRest = rest
    }
    | header == "Header" = Just $ IbCsvHordLine sec Header rest
    | header == "Data" = Just $ IbCsvHordLine sec Data rest
    | otherwise = Nothing

showIbCsvHordLine :: IbCsvHordLine -> [Char]
showIbCsvHordLine
  IbCsvHordLine
    { ichlSection = sec,
      ichlHeader = header,
      ichlRest = rest
    } =
    sec ++ "," ++ show header ++ "," ++ rest ++ "\n"

newtype IbCsvLines = IbCsvLines
  { unIbCsvLines :: [IbCsvHordLine]
  }

-- TODO Why doesn't deriving newtype Stream work?

instance Stream IbCsvLines where
  type Token IbCsvLines = IbCsvHordLine
  type Tokens IbCsvLines = [IbCsvHordLine]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (IbCsvLines []) = Nothing
  take1_ (IbCsvLines (t : ts)) = Just (t, IbCsvLines ts)
  takeN_ n icl@(IbCsvLines s)
    | n <= 0 = Just ([], icl)
    | null s = Nothing
    | otherwise = Just (L.over L._2 IbCsvLines $ splitAt n s)
  takeWhile_ f = L.over L._2 IbCsvLines . MP.takeWhile_ f . unIbCsvLines

instance VisualStream IbCsvLines where
  showTokens Proxy (l :| ls) = concatMap showIbCsvHordLine (l : ls)
  tokensLength proxy = length . MP.showTokens proxy

ibCsvLineParser :: Parsec Void String IbCsvLine
ibCsvLineParser = do
  section <- takeWhileP (Just "not a comma") (/= ',')
  void $ single ','
  header <- takeWhileP (Just "not a comma") (/= ',')
  void $ single ','
  rest <- manyTill anySingle (void newline <|> eof)
  return $ IbCsvLine section header (rest ++ "\n")

rawStatementParser :: Parsec Void String [IbCsvLine]
rawStatementParser = do
  void $ optional bom
  icls <- many ibCsvLineParser
  eof
  return icls

headerLine :: Parsec Void IbCsvLines (String, String)
headerLine = label "header line" $
  try $ do
    (IbCsvHordLine section Header rest) <- anySingle
    return (section, rest)

headerLineWithSection :: String -> Parsec Void IbCsvLines String
headerLineWithSection sectionName = label "header line" $
  try $ do
    (IbCsvHordLine section Header rest) <- anySingle
    guard $ sectionName == section
    return rest

dataLine :: String -> Parsec Void IbCsvLines String
dataLine sectionName = label "data line" $
  try $ do
    (IbCsvHordLine _ Data rest) <- satisfy ((== sectionName) . ichlSection)
    return rest

csvParser :: String -> Parsec Void IbCsvLines Csv
csvParser sectionName = label "IB statement's single CSV" $ do
  headerRest <- headerLineWithSection sectionName
  dataRests <- many $ try $ dataLine sectionName
  return $ Csv $ headerRest ++ concat dataRests

sectionParser :: Parsec Void IbCsvLines (String, Section)
sectionParser = label "IB statement section" $ do
  (section, _) <- lookAhead headerLine
  csvs <- some1 $ csvParser section
  return (section, Section csvs)

lineParser :: Parsec Void IbCsvLines Statement
lineParser = (Statement . Map.fromList <$> many (try sectionParser)) <* eof

-- | Parses an IB CSV Statement
parse :: String -> Either String Statement
parse csv = do
  ibCsvLines <-
    prependErrorMessagePretty
      "Could not parse IB CSV statement into individual structured lines.\n"
      $ MP.parse rawStatementParser "" csv
  let ibCsvHordLines = mapMaybe toMaybeHordLine ibCsvLines
  prependErrorMessage
    "Could not parse the IB CSV statement.\n"
    $ MP.parse lineParser "" (IbCsvLines ibCsvHordLines)
  where
    prependErrorMessagePretty errMsg = first ((errMsg ++) . errorBundlePretty)
    prependErrorMessage errMsg = first ((errMsg ++) . parseErrorPretty . head . bundleErrors)
