{-# LANGUAGE OverloadedLists #-}

module Test.Data.Csv.Extra (
  tests,
) where

import Data.Csv qualified as Csv
import Data.Csv.Extra (FromNamedRecord (..), decodeByName, fieldP, lookup)
import Data.Vector (Vector)
import Relude
import Test.HUnit.Extra (assertRightOrFailPrint)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec.Extra (parsePretty)

data TestRecord
  = RecordA
  | RecordB
  deriving stock (Eq, Show)

instance FromNamedRecord TestRecord where
  parseNamedRecord = do
    t <- lookup "type"
    case t of
      ("A" :: String) -> return RecordA
      "B" -> return RecordB
      _ -> fail ""

tests :: SpecWith ()
tests = do
  describe "Data.Csv.Extra" $ do
    describe "decodeByName" $ do
      it "Decodes" $ do
        ( decodeByName "type\nA\nA\nB" ::
            Either
              String
              ( Csv.Header
              , Vector TestRecord
              )
          )
          `shouldBe` Right
            ( ["type"]
            ,
              [ RecordA
              , RecordA
              , RecordB
              ]
            )
    describe "fieldP" $ do
      it "decodes an unquoted field" $ do
        let fieldOr = parsePretty (fieldP @Void) "example" ("foo" :: Text)
        field <- assertRightOrFailPrint fieldOr
        field `shouldBe` "foo"
      it "decodes a quoted field" $ do
        let fieldOr = parsePretty (fieldP @Void) "example" ("\"fo,\no\"\"b\r\nar\"" :: Text)
        field <- assertRightOrFailPrint fieldOr
        field `shouldBe` "fo,\no\"b\r\nar"
