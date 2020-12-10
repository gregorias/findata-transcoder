{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Match (matches, (=~)) where

import Data.Maybe (isJust)
import Replace.Megaparsec (breakCap)
import Text.Megaparsec (Stream, chunk)
import Text.Megaparsec.Stream (Stream (Tokens))

-- | Checks whether the given pattern is the stream.
matches ::
  (Stream s, Tokens s ~ s) =>
  -- | The input stream of text
  s ->
  -- | The pattern to match
  s ->
  Bool
matches input pat = isJust . breakCap (chunk pat) $ input

-- | The infix synonym of @matches@
(=~) ::
  (Stream s, Tokens s ~ s) =>
  -- | The input stream of text
  s ->
  -- | The pattern to match
  s ->
  Bool
(=~) = matches
