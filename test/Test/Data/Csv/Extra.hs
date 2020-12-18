{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Csv.Extra
  ( tests,
  )
where

import qualified Data.Csv as Csv
import Data.Csv.Extra
  ( FromNamedRecord (..),
    decodeByName,
    lookup,
  )
import Data.Vector (Vector)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

data TestRecord
  = RecordA
  | RecordB
  deriving (Eq, Show)

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
              ( Csv.Header,
                Vector TestRecord
              )
          )
          `shouldBe` Right
            ( ["type"],
              [ RecordA,
                RecordA,
                RecordB
              ]
            )
