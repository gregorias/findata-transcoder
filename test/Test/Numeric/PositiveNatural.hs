{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Test.Numeric.PositiveNatural (tests) where

import Numeric.PositiveNatural (fromNatural, positiveNaturalP)
import Relude hiding (one)
import Relude.Unsafe (fromJust)
import Test.Hspec (describe, it)
import Test.Hspec qualified as HSpec
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Text.Megaparsec.Extra (parsePretty)
import qualified Data.Text as T

tests :: HSpec.SpecWith ()
tests = do
  describe "Numeric.PositiveNatural" $ do
    it "parses one" $ do
      let one = fromJust $ fromNatural 1
      parsePretty (positiveNaturalP @_ @_ @Void) "test" ("1" :: Text) `shouldBe` Right one
    it "does not parse zero" $ do
      parsePretty (positiveNaturalP @_ @_ @Void) "test" ("0" :: Text) `shouldSatisfy`
        (\case
            Left errorMsg -> "Expected a positive natural number but got '0'." `T.isInfixOf` errorMsg
            _ -> False
        )
    it "does not parse floats" $ do
      parsePretty (positiveNaturalP @_ @_ @Void) "test" ("0.2137" :: Text) `shouldSatisfy` isLeft
    it "does not parse negative numbers" $ do
      parsePretty (positiveNaturalP @_ @_ @Void) "test" ("-420" :: Text) `shouldSatisfy` isLeft
