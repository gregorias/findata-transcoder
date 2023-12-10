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
import Witch.Extra qualified as Witch

data AdtSource = AdtSource {asA :: !Text, asB :: !Text}
  deriving stock (Show, Typeable)

tests :: Hspec.SpecWith ()
tests = do
  describe "Witch.Extra" $ do
    describe "Pretty TryFromException" $ do
      describe "nestException" $ do
        it "shows a pretty exception in multiple lines" $ do
          let base = PrettyException "foo"
          let firstLayer = TryFromException @Text "inner source" (Just (SomeException base))
          let secondLayer = Witch.nestException @Text "outer source" firstLayer
          void $ putDocW 5 (pretty secondLayer)
          show (pretty secondLayer)
            `shouldBe` [trimming|
                TryFromException
                source: "outer source"
                cause: TryFromException
                       source: "inner source"
                       cause: foo|]

      it "pretty prints the source" $ do
        let ex = TryFromException (AdtSource "A" "B") Nothing
        show (pretty ex)
          `shouldBe` [trimming|
              TryFromException
              source: AdtSource {
                          asA = "A",
                          asB = "B"
                      }|]
