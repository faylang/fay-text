## Changelog

#### 0.3.0.2 (2014-04-29)

* Allow `fay 0.20`

#### 0.3.0.1 (2014-01-14)

* Fix complation warnings

## 0.3 (2013-11-07)

* Add Ord instance for Text (okay because JS comparison operators work on the values)
* Make Text an EmptyDataDecl

## 0.2.0.0 (2013-09-24)

* Added a (large) subset of functions from `Data.Text`. These use the FFI so they should be pretty fast.
* Added `Eq` instance for `Text`.
* The `Text` type has been moved to `Fay.Text.Type` (re-exported by `Fay.Text`) in case you only want the type.

## 0.1.0.0 (2013-08-27)

* Initial release
