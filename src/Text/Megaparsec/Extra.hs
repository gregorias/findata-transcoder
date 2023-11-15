{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Megaparsec.Extra (
  parsePretty,

  -- * Nested parsing
  nestP,
  ToNestedParser (..),
  ParsecWithSource (..),
) where

import Data.Either.Extra (mapLeft)
import Language.Haskell.TH.Lift.Generics (genericLiftTypedCompat)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude
import Text.Megaparsec (
  MonadParsec,
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

-- A utility class for 'nestP'.
class ToNestedParser s r p | p -> r, p -> s where
  toNestedParser :: p -> (s -> Either Text r)

instance ToNestedParser s r (s -> Either Text r) where
  toNestedParser = id

data ParsecWithSource e s r = ParsecWithSource
  { pwsSource :: !Text
  , pwsParsec :: !(Parsec e s r)
  }

instance (VisualStream s, TraversableStream s, ShowErrorComponent e) => ToNestedParser s r (ParsecWithSource e s r) where
  toNestedParser (ParsecWithSource{pwsSource, pwsParsec}) = parsePretty pwsParsec pwsSource

-- | Parses with a decoder and its follow up parser while maintaining correct cursor offsets.
nestP ::
  ( MonadParsec e s m
  , MonadFail m
  ) =>
  m a ->
  (a -> Either Text b) ->
  m b
nestP decodeP f = do
  originalPos <- MP.getOffset
  decoded <- decodeP
  either
    (MP.region (MP.setErrorOffset originalPos) . fail . toString)
    pure
    (f decoded)

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
