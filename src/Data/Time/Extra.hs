module Data.Time.Extra (
  dayP,
) where

import Control.Applicative (some, (<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Relude (ToString (toString))
import Text.Megaparsec (MonadParsec (label, try), Parsec, match)
import Text.Megaparsec.Char (digitChar, string)
import Prelude

-- | Parses strings like "DD/MM/YYYY" or "YY-MM-DD" into a day.
dayP ::
  (Ord e) =>
  -- | Format string (see 'parseTimeM')
  String ->
  Parsec e Text Day
dayP fmt = label "date" $ do
  (dayStr, _) <-
    match . try $ do
      void $ some digitChar
      sepP
      void $ some digitChar
      sepP
      void $ some digitChar
  parseTimeM False defaultTimeLocale fmt (toString dayStr)
 where
  sepP :: (Ord e) => Parsec e Text ()
  sepP = void $ string "-" <|> string "/" <|> string "."
