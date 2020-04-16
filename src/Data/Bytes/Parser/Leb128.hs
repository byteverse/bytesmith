{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language TypeApplications #-}

-- | Parse numbers that have been encoded with <https://en.wikipedia.org/wiki/LEB128 LEB-128>.
-- LEB-128 allows arbitrarily large numbers to be encoded. Parsers in this
-- module will fail if the number they attempt to parse is outside the
-- range of what their target type can handle. The parsers for signed
-- numbers assume that the numbers have been
-- <https://developers.google.com/protocol-buffers/docs/encoding zigzig encoded>.
module Data.Bytes.Parser.Leb128
  ( -- * Unsigned
    word16
  , word32
  , word64
    -- * Signed (Zig-zag)
  , int16
  , int32
  , int64
  ) where

import Data.Bits (testBit,(.&.),unsafeShiftR,xor,complement)
import Data.Bits (unsafeShiftL,(.|.))
import Data.Bytes.Parser (Parser)
import Data.Int (Int16,Int32,Int64)
import Data.Word (Word8,Word16,Word32,Word64)

import qualified Data.Bytes.Parser as P

-- | Parse a LEB-128-encoded number. If the number is larger
-- than @0xFFFF@, fails with the provided error.
word16 :: e -> Parser e s Word16
word16 e = do
  w <- stepBoundedWord e 16 0 0
  pure (fromIntegral @Word64 @Word16 w)

-- | Parse a LEB-128-encoded number. If the number is larger
-- than @0xFFFFFFFF@, fails with the provided error.
word32 :: e -> Parser e s Word32
word32 e = do
  w <- stepBoundedWord e 32 0 0
  pure (fromIntegral @Word64 @Word32 w)

-- | Parse a LEB-128-encoded number. If the number is larger
-- than @0xFFFFFFFFFFFFFFFF@, fails with the provided error.
word64 :: e -> Parser e s Word64
word64 e = stepBoundedWord e 64 0 0

-- | Parse a LEB-128-zigzag-encoded signed number. If the encoded
-- number is outside the range @[-32768,32767]@, this fails with
-- the provided error.
int16 :: e -> Parser e s Int16
int16 = fmap zigzagDecode16 . word16

-- | Parse a LEB-128-zigzag-encoded signed number. If the encoded
-- number is outside the range @[-2147483648,2147483647]@, this
-- fails with the provided error.
int32 :: e -> Parser e s Int32
int32 = fmap zigzagDecode32 . word32

-- | Parse a LEB-128-zigzag-encoded signed number. If the encoded
-- number is outside the range @[-9223372036854775808,9223372036854775807]@,
-- this fails with the provided error.
int64 :: e -> Parser e s Int64
int64 = fmap zigzagDecode64 . word64

-- What these parameters are:
--
-- bitLimit: number of bits in the target word size
-- accShift: shift amount, increases by 7 at a time
stepBoundedWord :: e -> Int -> Word64 -> Int -> Parser e s Word64
stepBoundedWord e !bitLimit !acc0 !accShift = do
  raw <- P.any e
  let number = raw .&. 0x7F
      acc1 = acc0 .|.
        unsafeShiftL (fromIntegral @Word8 @Word64 number) accShift
      accShift' = accShift + 7
  if accShift' <= bitLimit
    then if testBit raw 7
      then stepBoundedWord e bitLimit acc1 accShift'
      else pure acc1
    else if fromIntegral @Word8 @Word raw < twoExp (bitLimit - accShift)
      then pure acc1 -- TODO: no need to mask upper bit in number
      else P.fail e

twoExp :: Int -> Word
twoExp x = unsafeShiftL 1 x

-- Zigzag decode strategy taken from https://stackoverflow.com/a/2211086/1405768
-- The accepted answer is a little bit, so an answer further down was used:
--
-- > zigzag_decode(value) = ( value >> 1 ) ^ ( ~( value & 1 ) + 1 )
zigzagDecode16 :: Word16 -> Int16
zigzagDecode16 n =
  fromIntegral ((unsafeShiftR n 1) `xor` (complement (n .&. 1) + 1))

zigzagDecode32 :: Word32 -> Int32
zigzagDecode32 n =
  fromIntegral ((unsafeShiftR n 1) `xor` (complement (n .&. 1) + 1))

zigzagDecode64 :: Word64 -> Int64
zigzagDecode64 n =
  fromIntegral ((unsafeShiftR n 1) `xor` (complement (n .&. 1) + 1))
