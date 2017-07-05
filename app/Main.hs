module Main where

import System.Environment
import Lib
import List

main :: IO ()
main = getArgs
  >>= (\(from:to:_) ->
         concatWith "\n\n" <$>file_to_bib from >>=  writeFile to )
