## Changelog

#### 0.3.2.2 (2015-02-11)

* Allow `fay 0.23.*`
* Allow `fay-base 0.20.*`

#### 0.3.2.1 (2015-01-05)

* Allow `fay 0.22.*`.

### 0.3.2 (2014-10-21)

* fay-text is now purely a compatibility layer between Fay and GHC. For Fay we reexport `Data.Text` from fay-base.

### 0.3.1 (2014-10-11)

* Allow `fay 0.21`

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
