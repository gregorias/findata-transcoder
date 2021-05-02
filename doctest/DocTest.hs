module DocTest (
  main,
) where

import Relude
import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-XNoImplicitPrelude"
    , "-XDerivingStrategies"
    , "-XGeneralizedNewtypeDeriving"
    , "-XOverloadedStrings"
    , "-XTypeApplications"
    , "-XScopedTypeVariables"
    , "-isrc"
    , "src/Data/Text/Trim.hs"
    , "src/Hledupt/Data/MyDecimal.hs"
    , "src/Hledupt/Data/Isin.hs"
    ]
