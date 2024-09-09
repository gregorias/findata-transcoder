{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

-- | Configuration for parsing Coop receipts.
module Transcoder.Coop.Config (
  -- * Configuration
  Config (..),
  emptyConfig,
  decodeConfig,

  -- * Shared expense rules
  SharedExpenseRules,
  getDebtors,

  -- * Payment cards
  PaymentCard,
  assignAccount,
) where

import Control.Lens (over, _Left)
import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Hledger (AccountName)
import Relude
import Safe (headMay)
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text qualified as Regex
import Transcoder.Wallet (debtAssets, (<:>))

-- | Configuration for processing Coop receipts.
--
-- It consists of information on relevant payment cards and rules for sharing expenses.
data Config = Config
  { shared :: !SharedExpenseRules
  , paymentCards :: ![PaymentCard]
  }

emptyConfig :: Config
emptyConfig = Config (SharedExpenseRules []) []

instance FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \v ->
    Config
      <$> (fromMaybe (SharedExpenseRules []) <$> v .:? "shared")
      <*> (fromMaybe [] <$> v .:? "paymentCards")

-- | Decodes a JSON config.
decodeConfig :: LByteString -> Either Text Config
decodeConfig = over _Left (("Could not decode the JSON config.\n" <>) . toText) . Aeson.eitherDecode'

data SharedExpenseRule = SharedExpenseRule
  { _ruleRegex :: !Regex.Regex
  -- ^ Matches product names covered by this rule.
  , _ruleDebtors :: ![Text]
  -- ^ The list of persons sharing this expense.
  }

newtype ParseableRegex = ParseableRegex {unParseableRegex :: Regex.Regex}

instance FromJSON ParseableRegex where
  parseJSON = Aeson.withText "Regex"
    $ \s ->
      either fail (return . ParseableRegex) (Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt s)

instance FromJSON SharedExpenseRule where
  parseJSON = Aeson.withObject "SharedExpenseRule" $ \o ->
    (SharedExpenseRule . unParseableRegex <$> (o .: "product")) <*> o .: "debtors"

newtype SharedExpenseRules = SharedExpenseRules [SharedExpenseRule]

instance FromJSON SharedExpenseRules where
  parseJSON =
    Aeson.withArray "SharedExpenseRules"
      $ fmap (SharedExpenseRules . toList)
      . mapM parseJSON

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

data PaymentCard = PaymentCard
  { paymentCardLastFourDigits :: !Text
  -- ^ The last four digits of a payment card.
  , paymentCardAccount :: !AccountName
  -- ^ The account name of associated with the payment card.
  }

instance FromJSON PaymentCard where
  parseJSON = Aeson.withObject "PaymentCard" $ \o ->
    PaymentCard <$> o .: "lastFourDigits" <*> o .: "account"

assignAccount ::
  [PaymentCard] ->
  -- | An obscured account number, e.g., 'XXXX****1234'.
  Text ->
  Maybe AccountName
assignAccount cards obscuredAccountNumber = paymentCardAccount <$> foundCard
 where
  lastFourDigits = T.takeEnd 4 obscuredAccountNumber
  accountMatch = (lastFourDigits ==)
  foundCard = headMay $ filter (accountMatch . paymentCardLastFourDigits) cards
