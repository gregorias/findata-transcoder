{-# OPTIONS_GHC -Wno-orphans #-}

module Witch.Extra (
  nestException,
  eitherNestException,
) where

import Data.Either.Combinators (mapLeft)
import Prettyprinter (Pretty (..), vsep, (<+>))
import Prettyprinter qualified as P
import Prettyprinter.Extra (PrettyException, unPrettyException)
import Relude
import Text.Nicify (nicify)
import Witch (TryFrom (..), TryFromException (..), maybeTryFrom)

instance TryFrom (Maybe a) a where
  tryFrom = maybeTryFrom id

-- | Nests a TryFromException inside another a potentially wider one.
nestException ::
  (Show source, Typeable source, Typeable target) =>
  source' ->
  TryFromException source target ->
  TryFromException source' target'
nestException newSource e = TryFromException newSource (Just (SomeException e))

-- | Lift nestException to the Either monad used by TryFrom.
--
-- Name inspired by Aeson's eitherDecodeFoo family of functions.
eitherNestException ::
  (Show source, Typeable source, Typeable target) =>
  source' ->
  Either (TryFromException source target) b ->
  Either (TryFromException source' target') b
eitherNestException newSource = mapLeft (nestException newSource)

instance (Show a) => Pretty (TryFromException a b) where
  pretty (TryFromException source Nothing) =
    vsep
      [ "TryFromException"
      , "source:" <+> P.hang 0 (show source & nicify & pretty)
      ]
  pretty (TryFromException source (Just se)) =
    vsep
      [ "TryFromException"
      , "source:" <+> P.hang 0 (show source & nicify & pretty)
      , "cause:" <+> P.hang 0 cause
      ]
   where
    cause :: P.Doc ann
    cause = case fromException @PrettyException se of
      Just prettyException -> unPrettyException prettyException
      Nothing -> fromString $ show se
