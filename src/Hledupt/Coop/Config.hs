-- | Parses a config file for parsing Coop receipts.
module Hledupt.Coop.Config (
  SharedExpenseRules,
  Config (..),
  emptyConfig,
  decodeConfig,
  getDebtors,
) where

import Control.Lens (over, _Left)
import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as Aeson
import Hledger (AccountName)
import Hledupt.Wallet (debtAssets, (<:>))
import Relude
import qualified Text.Regex.TDFA as Regex
import qualified Text.Regex.TDFA.Text as Regex

data SharedExpenseRule = SharedExpenseRule
  { -- | Matches product names covered by this rule.
    _ruleRegex :: !Regex.Regex
  , -- | The list of persons sharing this expense.
    _ruleDebtors :: ![Text]
  }

newtype ParseableRegex = ParseableRegex {unParseableRegex :: Regex.Regex}

instance FromJSON ParseableRegex where
  parseJSON = Aeson.withText "Regex" $
    \s ->
      either fail return $
        ParseableRegex <$> Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt s

instance FromJSON SharedExpenseRule where
  parseJSON = Aeson.withObject "SharedExpenseRule" $ \o ->
    SharedExpenseRule <$> (unParseableRegex <$> o .: "product") <*> o .: "debtors"

newtype SharedExpenseRules = SharedExpenseRules [SharedExpenseRule]

instance FromJSON SharedExpenseRules where
  parseJSON = Aeson.withArray "SharedExpenseRules" $ \v ->
    SharedExpenseRules . toList <$> sequence (parseJSON <$> v)

newtype Config = Config
  { shared :: SharedExpenseRules
  }

emptyConfig :: Config
emptyConfig = Config $ SharedExpenseRules []

instance FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \v -> Config <$> v .: "shared"

getDebtorsFromRule :: SharedExpenseRule -> Text -> Maybe [AccountName]
getDebtorsFromRule (SharedExpenseRule regex debtors) productName = do
  void $ join . rightToMaybe $ Regex.execute regex productName
  return ((<:>) debtAssets <$> debtors)

-- | Returns debtor account names sharing the expense.
getDebtors ::
  SharedExpenseRules ->
  -- | The product's name.
  Text ->
  [AccountName]
getDebtors (SharedExpenseRules rules) productName =
  fromMaybe [] $ asum $ flip getDebtorsFromRule productName <$> rules

-- | Decodes a JSON config.
decodeConfig :: LByteString -> Either Text Config
decodeConfig = over _Left (("Could not decode the JSON config.\n" <>) . toText) . Aeson.eitherDecode'
