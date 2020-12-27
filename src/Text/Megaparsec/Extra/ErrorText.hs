-- | A text wrapper that can be used for error messages.
module Text.Megaparsec.Extra.ErrorText (
  ErrorText (..),
) where

import Relude
import Text.Megaparsec.Error (ShowErrorComponent (..))

newtype ErrorText = ErrorText Text
  deriving newtype (Ord, Eq, IsString, ToString, Show)

instance ShowErrorComponent ErrorText where
  showErrorComponent = toString
