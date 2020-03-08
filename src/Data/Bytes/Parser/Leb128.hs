{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language TypeApplications #-}

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

import Data.Bits (testBit,(.&.),unsafeShiftR,xor)
import Data.Bytes.Parser (Parser)
import Data.Int (Int16,Int32,Int64)
import Data.Word (Word8,Word16,Word32,Word64)

import qualified Data.Bytes.Parser as P

-- | Parse a LEB-128-encoded number. If the number is larger
-- than @0xFFFF@, fails with the provided error.
word16 :: e -> Parser e s Word16
word16 e = do
  w <- stepBoundedWord e 0x200 0
  pure (fromIntegral @Word64 @Word16 w)

-- | Parse a LEB-128-encoded number. If the number is larger
-- than @0xFFFFFFFF@, fails with the provided error.
word32 :: e -> Parser e s Word32
word32 e = do
  w <- stepBoundedWord e 0x2000000 0
  pure (fromIntegral @Word64 @Word32 w)

-- | Parse a LEB-128-encoded number. If the number is larger
-- than @0xFFFFFFFFFFFFFFFF@, fails with the provided error.
word64 :: e -> Parser e s Word64
word64 e = stepBoundedWord e 0x200000000000000 0

int16 :: e -> Parser e s Int16
int16 = fmap zigzagDecode16 . word16

int32 :: e -> Parser e s Int32
int32 = fmap zigzagDecode32 . word32

int64 :: e -> Parser e s Int64
int64 = fmap zigzagDecode64 . word64

-- Precondition: accumulator is at most 128 times less than
-- the maximum word plus 1. For example, on 16-bit words:
--
-- succ_max: 0001_0000_0000_0000_0000
-- acc_max:  0000_0000_0010_0000_0000
stepBoundedWord :: e -> Word64 -> Word64 -> Parser e s Word64
stepBoundedWord e !bound !acc0 = do
  raw <- P.any e
  let number = raw .&. 0x7F
  let acc1 = fromIntegral @Word8 @Word64 number + (acc0 * 0x80)
  case testBit raw 7 of
    True -> if acc1 < bound
      then stepBoundedWord e bound acc1
      else P.fail e
    False -> pure acc1

-- Zigzag decode strategy taken from https://stackoverflow.com/a/2211086/1405768
zigzagDecode32 :: Word32 -> Int32
zigzagDecode32 u = (unsafeShiftR s 1) `xor` (negate (s .&. 1))
  where
  s = fromIntegral u :: Int32

zigzagDecode16 :: Word16 -> Int16
zigzagDecode16 u = (unsafeShiftR s 1) `xor` (negate (s .&. 1))
  where
  s = fromIntegral u :: Int16

zigzagDecode64 :: Word64-> Int64
zigzagDecode64 u = (unsafeShiftR s 1) `xor` (negate (s .&. 1))
  where
  s = fromIntegral u :: Int64
