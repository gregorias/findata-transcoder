module Test.Prettyprinter.Extra (tests) where

import Prettyprinter (vcat)
import Prettyprinter.Extra (PrettyException (..), unPrettyException)
import Relude
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Prettyprinter.Extra" $ do
    describe "PrettyException" $ do
      it "should be easily composable and printable" $ do
        let prettyException = PrettyException (vcat ["foo", "bar"])
        let content = unPrettyException prettyException
        show content `shouldBe` "foo\nbar"
