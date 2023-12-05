module Prettyprinter.Extra (
  PrettyException (..),
  unPrettyException,
) where

import Prettyprinter (Doc, unAnnotate)
import Relude

-- | An exception with an unannotated pretty-printable message.
--
-- The message is considered to be unannotated because it is inside an
-- existential type.
data PrettyException where
  PrettyException :: Doc ann -> PrettyException

-- | Unwrap a 'PrettyException' into an unnanotated 'Doc'.
unPrettyException :: PrettyException -> Doc ann
unPrettyException (PrettyException doc) = unAnnotate doc

deriving stock instance Show PrettyException

deriving stock instance Typeable PrettyException

instance Exception PrettyException
