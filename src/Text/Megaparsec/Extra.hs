{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Megaparsec.Extra (
  parsePretty,
) where

import Data.Either.Extra (mapLeft)
import Language.Haskell.TH.Lift.Generics (genericLiftTypedCompat)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude
import Text.Megaparsec (
  Parsec,
  ShowErrorComponent,
  TraversableStream,
  VisualStream,
  errorBundlePretty,
  parse,
 )
import Text.Megaparsec qualified as MP

instance Lift MP.Pos where
  liftTyped = genericLiftTypedCompat

-- | Parses input and provides a pretty error message.
parsePretty ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  -- | Parser to run
  Parsec e s a ->
  -- | Name of source file
  Text ->
  -- | Input for parser
  s ->
  Either Text a
parsePretty parserP source = prettifyErrors . parse parserP (toString source)
 where
  prettifyErrors =
    mapLeft ((("Could not parse " <> source <> ":\n") <>) . toText . errorBundlePretty)
