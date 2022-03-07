-- | This module does a first-pass parsing of an IB statement CSV into
-- individual CSVs.
module Hledupt.Ib.Csv.RawParse (
  Statement (..),
  Section (..),
  Csv (..),
  parse,
) where

import Control.Lens (over)
import qualified Control.Lens as L
import Data.Either.Extra (mapLeft)
import Data.List.NonEmpty (some1)
import qualified Data.Map as Map
import Relude
import Text.Megaparsec (
  MonadParsec (eof, label, lookAhead, takeWhileP, try),
  ParseErrorBundle (bundleErrors),
  Parsec,
  Stream,
  VisualStream,
  anySingle,
  manyTill,
  parseErrorPretty,
  satisfy,
  single,
 )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Extra (bom)
import Text.Megaparsec.Extra (parsePretty)

newtype Statement = Statement
  { unStatement :: Map.Map String Section
  }
  deriving newtype (Eq, Show)

newtype Section = Section
  { unSection :: NonEmpty Csv
  }
  deriving newtype (Eq, Show)

newtype Csv = Csv
  { unCsv :: String
  }
  deriving newtype (IsString, Eq, Show)

data IbCsvLine = IbCsvLine
  { iclSection :: String
  , iclHeader :: String
  , iclRest :: String
  }
  deriving stock (Eq, Ord)

data Hord = Header | Data
  deriving stock (Eq, Ord, Show)

data IbCsvHordLine = IbCsvHordLine
  { ichlSection :: String
  , ichlHeader :: Hord
  , ichlRest :: String
  }
  deriving stock (Eq, Ord)

toMaybeHordLine :: IbCsvLine -> Maybe IbCsvHordLine
toMaybeHordLine
  IbCsvLine
    { iclSection = sec
    , iclHeader = header
    , iclRest = rest
    }
    | header == "Header" = Just $ IbCsvHordLine sec Header rest
    | header == "Data" = Just $ IbCsvHordLine sec Data rest
    | otherwise = Nothing

showIbCsvHordLine :: IbCsvHordLine -> [Char]
showIbCsvHordLine
  IbCsvHordLine
    { ichlSection = sec
    , ichlHeader = header
    , ichlRest = rest
    } =
    sec ++ "," ++ show header ++ "," ++ rest ++ "\n"

newtype IbCsvLines = IbCsvLines
  { unIbCsvLines :: [IbCsvHordLine]
  }

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
    | otherwise = Just (over L._2 IbCsvLines $ splitAt n s)
  takeWhile_ f = over L._2 IbCsvLines . MP.takeWhile_ f . unIbCsvLines

instance VisualStream IbCsvLines where
  showTokens Proxy (l :| ls) = concatMap showIbCsvHordLine (l : ls)
  tokensLength proxy = length . MP.showTokens proxy

ibCsvLineP :: Parsec Void String IbCsvLine
ibCsvLineP = do
  section <- takeWhileP (Just "not a comma") (/= ',')
  void $ single ','
  header <- takeWhileP (Just "not a comma") (/= ',')
  void $ single ','
  rest <- manyTill anySingle (void newline <|> eof)
  return $ IbCsvLine section header (rest ++ "\n")

rawStatementP :: Parsec Void String [IbCsvLine]
rawStatementP = do
  void $ optional bom
  icls <- many ibCsvLineP
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

lineP :: Parsec Void IbCsvLines Statement
lineP = (Statement . Map.fromList <$> many (try sectionParser)) <* eof

-- | Parses an IB CSV Statement
parse :: Text -> Either Text Statement
parse csv = do
  ibCsvLines <- parsePretty rawStatementP "IB CSV statement" (toString csv)
  let ibCsvHordLines = mapMaybe toMaybeHordLine ibCsvLines
  mapLeft toText
    . prependErrorMessage
      "Could not parse the IB CSV statement.\n"
    $ MP.parse lineP "" (IbCsvLines ibCsvHordLines)
 where
  prependErrorMessage errMsg = first ((errMsg ++) . parseErrorPretty . head . bundleErrors)
