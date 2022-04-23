module Test.Transcoder.Coop.Config (
  tests,
) where

import NeatInterpolation (trimming)
import Relude
import Test.HUnit.Lang (assertFailure)
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Coop.Config (Config (..), decodeConfig, getDebtors)

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Coop.Config" $ do
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

    it "Parses the config and does not apply the rules" $ do
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
      getDebtors rules "Margarine 200g"
        `shouldBe` []
