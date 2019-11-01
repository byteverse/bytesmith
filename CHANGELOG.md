# Revision history for bytesmith

## 0.3.1.0 -- 2019-??-??

* Add big-endian and little-endian parsers for `Word128`.
* Add a module for little-endian word parsers. This compliments the
  existing big-endian module.
* Add functions for parsing arrays of big/little endian words of
  various sizes.
* Add `skipUntil` to `Latin`.

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
