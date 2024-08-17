module DocTest (
  main,
) where

import Relude (IO, (=<<))
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "findata-transcoder" =<< getArgs
