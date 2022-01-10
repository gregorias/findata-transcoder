module Test.Hledupt.Coop.Config (
  tests,
) where

import Hledupt.Coop.Config (Config (..), decodeConfig, getDebtors)
import NeatInterpolation (trimming)
import Relude
import Test.HUnit.Lang (assertFailure)
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Coop.Config" $ do
    it "Parses the config" $ do
      let json =
            [trimming|
                {
                  "shared": [
                    {"product": "Butter",   "debtors": ["John Doe", "Mary Sue"]}
                  ]
                }
            |]
      let eitherConfig = decodeConfig $ encodeUtf8 json
      (Config rules) <-
        either
          (\e -> assertFailure $ "Could not decode the config. " <> toString e)
          return
          eitherConfig
      getDebtors rules "Bio Butter 200g"
        `shouldBe` [ "Assets:Debts:John Doe"
                   , "Assets:Debts:Mary Sue"
                   ]
