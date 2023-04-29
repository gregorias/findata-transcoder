{-# LANGUAGE OverloadedLists #-}

module Test.Control.Applicative.Combinators.Extra (tests) where

import Control.Applicative.Combinators.Extra (some1Till, some1Till_)
import Relude
import Test.Hspec (describe, it)
import Test.Hspec qualified as HSpec
import Test.Hspec.Expectations.Pretty (shouldBe)

type MaybeState s a = MaybeT (State s) a

onS :: (Eq s) => s -> a -> MaybeState [s] a
onS s a = MaybeT $ do
  state
    ( \case
        (x : xs) -> if x == s then (Just a, xs) else (Nothing, x : xs)
        xs -> (Nothing, xs)
    )

parseString :: MaybeState [Char] a -> String -> Maybe a
parseString = evalState . runMaybeT

tests :: HSpec.SpecWith ()
tests = do
  describe "Control.Applicative.Combinators.Extra" $ do
    describe "some1Till" $ do
      let p = some1Till (onS 'a' "a") (onS 'e' "end")
      it "Parses until end" $ do
        parseString p "aaaaeaaa" `shouldBe` Just ["a", "a", "a", "a"]
        parseString p "aeaaa" `shouldBe` Just ["a"]
        parseString p "e" `shouldBe` Nothing
    describe "some1Till_" $ do
      let p = some1Till_ (onS 'a' "a") (onS 'e' "end")
      it "Parses until end and returns it" $ do
        parseString p "aaaaeaaa" `shouldBe` Just (["a", "a", "a", "a"], "end")
        parseString p "aeaaa" `shouldBe` Just (["a"], "end")
        parseString p "e" `shouldBe` Nothing
