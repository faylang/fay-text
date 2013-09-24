{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Test where

import           Fay.Text (Text, fromString)
import qualified Fay.Text as T
import           FFI
import           Prelude  hiding ((++))

(++) = T.append

eq :: Text -> Text -> Text -> Fay ()
eq msg exp res
  | exp == res = putStrLn . T.unpack $ msg ++ " [OK]"
  | otherwise = error $ T.unpack $ msg ++ ": Expected `" ++ exp ++ "' but got `" ++ res ++ "'"

b :: Bool -> Text
b True = "true"
b False = "false"

d :: Automatic a -> Text
d = ffi "%1 + ''"

charToUpper :: Char -> Char
charToUpper = ffi "%1.toUpperCase()"

main :: Fay ()
main = do
  eq "pack"  "abc" (T.pack (T.unpack "abc"))
  eq "empty" "" T.empty
  eq "cons" "abc" (T.cons 'a' "bc")
  eq "snoc" "abc" (T.snoc "ab" 'c')
  eq "append" "abcd" (T.append "ab" "cd")
  eq "head" "a" (T.cons (T.head "abc") "")
  eq "last" "c" (T.cons (T.last "abc") "")
  eq "tail" "bc" (T.tail "abc")
  eq "init" "ab" (T.init "abc")
  eq "null" "true" (b $ T.null "")
  eq "null" "false" (b $ T.null "abc")
  eq "length" "3" (d $ T.length "abc")
  eq "map" "ABC" (T.map charToUpper "abc")
  eq "intercalate" "a_b_c" (T.intercalate "_" ["a","b","c"])
  eq "intersperse" "a_b_c" (T.intersperse '_' "abc")
  eq "reverse" "cba" (T.reverse "abc")
  eq "toLower" "abc" (T.toLower "ABC")
  eq "toUpper" "ABC" (T.toUpper "abc")
  eq "concatMap" "AABBCC" (T.concatMap (\c -> charToUpper c `T.cons` (charToUpper c `T.cons` "")) "abc")
  eq "any" "true" (b $ T.any (== 'c') "abc")
  eq "all" "false" (b $ T.all (== 'a') "abc")
  eq "maximum" "x" (d $ T.maximum "axb")
  eq "minimum" "a" (d $ T.minimum "xay")
