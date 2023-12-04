-- | Extra functionalities for HUnit.
module Test.HUnit.Extra (
  assertLeft,
  assertRight,
  assertRightOrFailPrint,
  textShouldContain,
) where

import Relude
import Test.HUnit (Assertion, assertFailure)
import Test.Hspec.Expectations.Pretty (shouldContain)

-- | Asserts that the given Either is Left.
assertLeft :: (Show r) => Either l r -> IO l
assertLeft (Left l) = return l
assertLeft (Right r) = assertFailure $ "Expected Left, got " <> show r

-- | Asserts that the given Either is Right.
assertRight :: (Show l) => Either l r -> IO r
assertRight (Left l) = assertFailure $ "Expected Right, got " <> show l
assertRight (Right r) = return r

-- | Asserts that the given Either is Right.
--
-- Prints the error message if not.
-- Better than just assertRight, because it maintains newline formatting.
assertRightOrFailPrint :: Either Text r -> IO r
assertRightOrFailPrint (Left err) = assertFailure . toString $ "Expected Right, got an error:\n" <> err
assertRightOrFailPrint (Right r) = return r

-- | Asserts that the given text contains the given text.
textShouldContain :: Text -> Text -> Assertion
textShouldContain haystack needle = toString haystack `shouldContain` toString needle
