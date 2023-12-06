module Test.Witch.Extra (tests) where

import NeatInterpolation (trimming)
import Prettyprinter (pretty)
import Prettyprinter.Extra (PrettyException (..))
import Prettyprinter.Util (putDocW)
import Relude
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Witch (TryFromException (..))
import Witch.Extra ()

tests :: Hspec.SpecWith ()
tests = do
  describe "Witch.Extra" $ do
    describe "Pretty TryFromException" $ do
      it "shows a pretty exception in multiple lines" $ do
        let base = PrettyException "foo"
        let firstLayer = TryFromException @Text "inner source" (Just (SomeException base))
        let firstLayerPretty = PrettyException $ pretty firstLayer
        let secondLayer = TryFromException @Text "outer source" (Just (SomeException firstLayerPretty))
        void $ putDocW 5 (pretty secondLayer)
        show (pretty secondLayer)
          `shouldBe` [trimming|
              TryFromException
              source: "outer source"
              cause: TryFromException
                     source: "inner source"
                     cause: foo|]
