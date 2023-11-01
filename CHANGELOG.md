# Revision history for bytesmith

## 0.3.11.0 -- 2023-??-??

* Add `Data.Bytes.Parser.Latin.hexWord32`.

## 0.3.10.0 -- 2023-07-25

* Add `mapErrorEffectfully`.
* Correct the implementation of `satisfy`.
* Add `takeUpTo`.

## 0.3.9.1 -- 2022-12-06

* Build with GHC 9.4.

## 0.3.9.0 -- 2022-07-14

* Build with GHC 9.2.3.

## 0.3.8.0 -- 2021-10-11

* Add `peek` and `peek'` to `Data.Bytes.Parser.Latin`.
* Add inline pragmas to most functions to prevent cost centers.
* Add support for WordRep-to-LiftedRep in Rebindable module.
* Allow building with newer Contiguous.
* Export `uneffectful`.

## 0.3.7.0 -- 2020-07-27

* Add `Data.Bytes.Parser.Base128` module for Base-128 encoding.
* Add `Data.Bytes.Parser.Leb128` module for LEB-128 encoding.
  Supports signed integers with zig-zag encoding.
* Add `skipWhile` to `Data.Bytes.Parser.Latin`.
* Reexport `endOfInput` and `isEndOfInput` from `Latin`.
* Add `charInsensitive` to ASCII module.
* Correct implementation of `peek` and `peek'`.

## 0.3.6.0 -- 2020-03-04

* Add `char12`
* Add `skipTrailedBy2`, `skipTrailedBy3`, and variants
  with an unboxed result.
* Add `cstring`
* Add `peekRemaining`
* Add `measure_` and `measure_#`, variants of `measure`
  that only give the byte count.
* Add `Data.Bytes.Parser.Rebindable`, the ultimate hack.
* Add `Data.Bytes.Latin.takeTrailedBy`

## 0.3.5.0 -- 2020-02-10

* Add big-endian and little-endian `word256` and `word256Array` parsers.
* Add `hexFixedWord64`.

## 0.3.4.0 -- 2020-02-03

* Add `hexFixedWord32`.

## 0.3.3.0 -- 2020-01-22

* Add `hexWord8`, `hexWord16`, and `hexFixedWord8`.

## 0.3.2.0 -- 2019-12-27

* Add `parseBytesEither` and `parseBytesMaybe`.
* Add common idioms from other parser libaries. This includes: `satisfy`,
  `satisfyWith`, `scan`, `peek`, and `peek'`.

## 0.3.1.0 -- 2019-11-12

* Add big-endian and little-endian parsers for `Word128`.
* Add a module for little-endian word parsers. This compliments the
  existing big-endian module.
* Add functions for parsing arrays of big/little endian words of
  various sizes.
* Add `skipUntil` to `Latin`.
* Add `char5`, `char6`, `char7`, `char8`, `char9`, `char10`, and
  `char11` to `Latin`.
* Correct the implementation of `takeTrailedBy`.

## 0.3.0.0 -- 2019-09-30

* Include the offset into the byte sequence in `Result`. Breaking change.
* Rename `hexWord16` to `hexFixedWord16`. Breaking change.
* Rename `parseBytesST` to `parseBytesEffectfully`. Breaking change.
* Add `hexNibbleLower` and `tryHexNibbleLower`.
* Add `hexNibble` and `tryHexNibble`.

## 0.2.0.1 -- 2019-09-24

* Correct an overflow-detection mistake in the implementation
  of machine-word parsers.

## 0.2.0.0 -- 2019-09-24

* Add big-endian word parsers.
* Redo module structure so that encoding-specific functions each
  live in their own module.
* Add a lot more functions and attempt to make naming somewhat
  consistent.
* Add `delimit`.
* Add `replicate`.
* Add `annotate` and its infix synonym `<?>`.

## 0.1.0.0 -- 2019-08-22

* First version.
