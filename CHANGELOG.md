# Revision history for bytesmith

## 0.3.6.0 -- 2020-??-??

* Add `char12`
* Add `skipTrailedByEither`

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
