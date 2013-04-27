module Main where

import Data.Text
import Prelude
import FFI

t :: Text
t = ffi "'OK'"

main :: Fay ()
main = do
  print t
  print $ pack "OK"
  putStrLn $ unpack $ pack "OK"
