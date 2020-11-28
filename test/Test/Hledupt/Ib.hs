module Test.Hledupt.Ib
  ( ibTests,
  )
where

import Test.Hspec (describe)
import qualified Test.Hspec as Hspec

ibTests :: Hspec.SpecWith ()
ibTests = do
  describe "Interactive Brokers tests" $ do
    return ()
