module Data.Time.Calendar.Extra (
  monthP,
) where

import Data.Time.Calendar.MonthDay.Compat (MonthOfYear)
import Relude
import Relude.Unsafe (fromJust)
import Text.Megaparsec (
  Parsec,
  choice,
 )
import Text.Megaparsec.Char qualified as MP
import Prelude (lookup)

monthAssocList :: [(Text, MonthOfYear)]
monthAssocList =
  [ ("January", 1)
  , ("Januar", 1)
  , ("Jan.", 1)
  , ("Jan", 1)
  , ("February", 2)
  , ("Februar", 2)
  , ("Feb.", 2)
  , ("März", 3)
  , ("March", 3)
  , ("Mar", 3)
  , ("Mar.", 3)
  , ("April", 4)
  , ("Apr.", 4)
  , ("Apr", 4)
  , ("Mai", 5)
  , ("May", 5)
  , ("Juni", 6)
  , ("June", 6)
  , ("Jun.", 6)
  , ("Jun", 6)
  , ("Jul", 7)
  , ("Juli", 7)
  , ("July", 7)
  , ("Jul.", 7)
  , ("August", 8)
  , ("Aug.", 8)
  , ("Aug", 8)
  , ("September", 9)
  , ("Sept.", 9)
  , ("Sep", 9)
  , ("Oktober", 10)
  , ("October", 10)
  , ("Okt.", 10)
  , ("Okt", 10)
  , ("November", 11)
  , ("Nov.", 11)
  , ("Nov", 11)
  , ("December", 12)
  , ("Dezember", 12)
  , ("Dez.", 12)
  , ("Dez", 12)
  ]

-- Use a reverse-sorted list to make sure that the longest match is found first. In other words,
-- "June" is matched before "Jun".
-- More future-proof to do it this way than to keep that in mind in monthAssocList.
months :: [Text]
months = sortOn Down $ fst <$> monthAssocList

parseMonth :: Text -> Maybe MonthOfYear
parseMonth = flip lookup monthAssocList

monthP :: (Ord e) => Parsec e Text MonthOfYear
monthP = do
  month <- choice $ fmap MP.string months
  -- fromJust is unsafe, but the parse above guarantees the key's presence.
  -- Unfortunately, proving key presence on the type is too complex, so I think
  -- this solution is reasonable.
  return . fromJust . parseMonth $ month
