{-# OPTIONS_GHC -Wno-orphans #-}

module Witch.Extra (
  nestException,
  eitherNestException,
) where

import Data.Either.Combinators (mapLeft)
import Relude
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
