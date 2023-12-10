{-# OPTIONS_GHC -Wno-orphans #-}

module Witch.Extra (
  nestException,
  eitherNestException,
) where

import Data.Either.Combinators (mapLeft)
import Prettyprinter (Pretty (..), vsep, (<+>))
import Prettyprinter qualified as P
import Prettyprinter.Extra (PrettyException (..), unPrettyException)
import Relude
import Text.Nicify (nicify)
import Witch (TryFrom (..), TryFromException (..), maybeTryFrom)

instance TryFrom (Maybe a) a where
  tryFrom = maybeTryFrom id

-- | Nests a TryFromException inside another a potentially wider one.
--
-- Transforms the nested exception into a PrettyException for pretty multi-line
-- printing. I don't need it for anything else. Can't pretty print it otherwise.
nestException ::
  (Show source) =>
  source' ->
  TryFromException source target ->
  TryFromException source' target'
nestException newSource e = TryFromException newSource (Just (SomeException $ PrettyException $ pretty e))

-- | Lift nestException to the Either monad used by TryFrom.
--
-- Name inspired by Aeson's eitherDecodeFoo family of functions.
eitherNestException ::
  (Show source) =>
  source' ->
  Either (TryFromException source target) b ->
  Either (TryFromException source' target') b
eitherNestException newSource = mapLeft (nestException newSource)

instance (Show a) => Pretty (TryFromException a b) where
  pretty (TryFromException source maybeSe) =
    vsep
      ( [ "TryFromException"
        , "source:" <+> P.hang 0 (show source & nicify & pretty)
        ]
          <> maybe [] (\se -> ["cause:" <+> P.hang 0 (prettyCause se)]) maybeSe
      )
   where
    prettyCause :: SomeException -> P.Doc ann
    prettyCause se = case fromException @PrettyException se of
      Just prettyException -> unPrettyException prettyException
      Nothing -> fromString $ show se
