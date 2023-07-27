-- | Extra functionalities for HUnit.
module Test.HUnit.Extra (
  assertLeft,
  assertRight,
) where

import Relude
import Test.HUnit (assertFailure)

-- | Asserts that the given Either is Left.
assertLeft :: (Show r) => Either l r -> IO l
assertLeft (Left l) = return l
assertLeft (Right r) = assertFailure $ "Expected Left, got " <> show r

-- | Asserts that the given Either is Right.
assertRight :: (Show l) => Either l r -> IO r
assertRight (Left l) = assertFailure $ "Expected Right, got " <> show l
assertRight (Right r) = return r
