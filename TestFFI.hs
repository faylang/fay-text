module TestFFI where

import FFI
import Fay.Text (Text)
import Prelude

d :: Automatic a -> Text
d = ffi "%1 + ''"

charToUpper :: Char -> Char
charToUpper = ffi "%1.toUpperCase()"
