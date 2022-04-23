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
    , "src/Transcoder/Data/MyDecimal.hs"
    , "src/Transcoder/Data/Isin.hs"
    ]
