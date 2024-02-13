{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}

-- | Big-endian fixed-width numbers.
module Data.Bytes.Parser.BigEndian
  ( -- * Unsigned
    word8
  , word16
  , word32
  , word64
  , word128
  , word256

    -- * Signed
  , int8
  , int16
  , int32
  , int64

    -- * Many

    -- ** Unsigned
  , word16Array
  , word32Array
  , word64Array
  , word128Array
  , word256Array
  ) where

import Prelude hiding (any, fail, length, takeWhile)

#if MIN_VERSION_base(4,18,0)
#else
import Control.Applicative (liftA2)
#endif
import Data.Bits (unsafeShiftL, (.|.))
import Data.Bytes.Parser.Internal (Parser, Result (..), swapArray128, swapArray16, swapArray256, swapArray32, swapArray64, uneffectful)
import Data.Bytes.Types (Bytes (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (ByteArray (..), PrimArray (..))
import Data.WideWord (Word128 (Word128), Word256 (Word256))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian), targetByteOrder)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM

-- | Unsigned 8-bit word.
word8 :: e -> Parser e s Word8
word8 = P.any

{- | Parse an array of big-endian unsigned 16-bit words. If the host is
big-endian, the implementation is optimized to simply @memcpy@ bytes
into the result array. The result array always has elements in
native-endian byte order.
-}
word16Array ::
  -- | Error message if not enough bytes are present
  e ->
  -- | Number of big-endian 16-bit words to expect
  Int ->
  -- | Native-endian elements
  Parser e s (PrimArray Word16)
word16Array e !n = case targetByteOrder of
  BigEndian -> fmap (asWord16s . Bytes.toByteArrayClone) (P.take e (n * 2))
  LittleEndian -> do
    bs <- P.take e (n * 2)
    let r = swapArray16 bs
    pure (asWord16s r)

-- | Parse an array of big-endian unsigned 32-bit words.
word32Array ::
  -- | Error message if not enough bytes are present
  e ->
  -- | Number of big-endian 32-bit words to expect
  Int ->
  -- | Native-endian elements
  Parser e s (PrimArray Word32)
word32Array e !n = case targetByteOrder of
  BigEndian -> fmap (asWord32s . Bytes.toByteArrayClone) (P.take e (n * 4))
  LittleEndian -> do
    bs <- P.take e (n * 4)
    let r = swapArray32 bs
    pure (asWord32s r)

-- | Parse an array of big-endian unsigned 64-bit words.
word64Array ::
  -- | Error message if not enough bytes are present
  e ->
  -- | Number of big-endian 64-bit words to consume
  Int ->
  -- | Native-endian elements
  Parser e s (PrimArray Word64)
word64Array e !n = case targetByteOrder of
  BigEndian -> fmap (asWord64s . Bytes.toByteArrayClone) (P.take e (n * 8))
  LittleEndian -> do
    bs <- P.take e (n * 8)
    let r = swapArray64 bs
    pure (asWord64s r)

-- | Parse an array of big-endian unsigned 256-bit words.
word256Array ::
  -- | Error message if not enough bytes are present
  e ->
  -- | Number of big-endian 256-bit words to consume
  Int ->
  -- | Native-endian elements
  Parser e s (PrimArray Word256)
word256Array e !n = case targetByteOrder of
  BigEndian -> fmap (asWord256s . Bytes.toByteArrayClone) (P.take e (n * 32))
  LittleEndian -> do
    bs <- P.take e (n * 32)
    let r = swapArray256 bs
    pure (asWord256s r)

-- | Parse an array of big-endian unsigned 128-bit words.
word128Array ::
  -- | Error message if not enough bytes are present
  e ->
  -- | Number of big-endian 128-bit words to consume
  Int ->
  -- | Native-endian elements
  Parser e s (PrimArray Word128)
word128Array e !n = case targetByteOrder of
  BigEndian -> fmap (asWord128s . Bytes.toByteArrayClone) (P.take e (n * 16))
  LittleEndian -> do
    bs <- P.take e (n * 16)
    let r = swapArray128 bs
    pure (asWord128s r)

asWord16s :: ByteArray -> PrimArray Word16
asWord16s (ByteArray x) = PrimArray x

asWord32s :: ByteArray -> PrimArray Word32
asWord32s (ByteArray x) = PrimArray x

asWord64s :: ByteArray -> PrimArray Word64
asWord64s (ByteArray x) = PrimArray x

asWord128s :: ByteArray -> PrimArray Word128
asWord128s (ByteArray x) = PrimArray x

asWord256s :: ByteArray -> PrimArray Word256
asWord256s (ByteArray x) = PrimArray x

-- | Unsigned 16-bit word.
word16 :: e -> Parser e s Word16
word16 e = uneffectful $ \chunk ->
  if length chunk >= 2
    then
      let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
          wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
       in Success
            (fromIntegral @Word @Word16 (unsafeShiftL (fromIntegral wa) 8 .|. fromIntegral wb))
            (offset chunk + 2)
            (length chunk - 2)
    else Failure e

-- | Unsigned 32-bit word.
word32 :: e -> Parser e s Word32
word32 e = uneffectful $ \chunk ->
  if length chunk >= 4
    then
      let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
          wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
          wc = PM.indexByteArray (array chunk) (offset chunk + 2) :: Word8
          wd = PM.indexByteArray (array chunk) (offset chunk + 3) :: Word8
       in Success
            ( fromIntegral @Word @Word32
                ( unsafeShiftL (fromIntegral wa) 24
                    .|. unsafeShiftL (fromIntegral wb) 16
                    .|. unsafeShiftL (fromIntegral wc) 8
                    .|. fromIntegral wd
                )
            )
            (offset chunk + 4)
            (length chunk - 4)
    else Failure e

-- | Unsigned 64-bit word.
word64 :: e -> Parser e s Word64
word64 e = uneffectful $ \chunk ->
  if length chunk >= 8
    then
      let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
          wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
          wc = PM.indexByteArray (array chunk) (offset chunk + 2) :: Word8
          wd = PM.indexByteArray (array chunk) (offset chunk + 3) :: Word8
          we = PM.indexByteArray (array chunk) (offset chunk + 4) :: Word8
          wf = PM.indexByteArray (array chunk) (offset chunk + 5) :: Word8
          wg = PM.indexByteArray (array chunk) (offset chunk + 6) :: Word8
          wh = PM.indexByteArray (array chunk) (offset chunk + 7) :: Word8
       in Success
            ( unsafeShiftL (fromIntegral wa) 56
                .|. unsafeShiftL (fromIntegral wb) 48
                .|. unsafeShiftL (fromIntegral wc) 40
                .|. unsafeShiftL (fromIntegral wd) 32
                .|. unsafeShiftL (fromIntegral we) 24
                .|. unsafeShiftL (fromIntegral wf) 16
                .|. unsafeShiftL (fromIntegral wg) 8
                .|. fromIntegral wh
            )
            (offset chunk + 8)
            (length chunk - 8)
    else Failure e

-- | Unsigned 128-bit word.
word128 :: e -> Parser e s Word128
word128 e = liftA2 Word128 (word64 e) (word64 e)

-- | Unsigned 256-bit word.
word256 :: e -> Parser e s Word256
word256 e = (\a b c d -> Word256 a b c d) <$> word64 e <*> word64 e <*> word64 e <*> word64 e

-- | Signed 8-bit integer.
int8 :: e -> Parser e s Int8
int8 = fmap fromIntegral . word8

-- | Signed 16-bit integer.
int16 :: e -> Parser e s Int16
int16 = fmap fromIntegral . word16

-- | Signed 32-bit integer.
int32 :: e -> Parser e s Int32
int32 = fmap fromIntegral . word32

-- | Signed 64-bit integer.
int64 :: e -> Parser e s Int64
int64 = fmap fromIntegral . word64
