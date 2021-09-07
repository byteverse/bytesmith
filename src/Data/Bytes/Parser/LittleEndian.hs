{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

-- | Little-endian fixed-width numbers.
module Data.Bytes.Parser.LittleEndian
  ( -- * One
    -- ** Unsigned
    word8
  , word16
  , word32
  , word64
  , word128
  , word256
    -- ** Signed
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
    -- ** Unsigned
  , int64Array
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Control.Applicative (liftA2)
import Data.Bits ((.|.),unsafeShiftL)
import Data.Primitive (ByteArray(..),PrimArray(..))
import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser,uneffectful)
import Data.Bytes.Parser.Internal (Result(..))
import Data.Bytes.Parser.Internal (swapArray16,swapArray32)
import Data.Bytes.Parser.Internal (swapArray64,swapArray128,swapArray256)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.WideWord (Word128(Word128),Word256(Word256))
import GHC.ByteOrder (ByteOrder(LittleEndian,BigEndian),targetByteOrder)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM

-- | Unsigned 8-bit word.
word8 :: e -> Parser e s Word8
word8 = P.any

-- | Array of little-endian unsigned 16-bit words. If the host is
-- little-endian, the implementation is optimized to simply @memcpy@
-- bytes into the result array. The result array always has elements
-- in native-endian byte order.
word16Array ::
     e -- ^ Error message if not enough bytes are present
  -> Int -- ^ Number of little-endian 16-bit words to expect
  -> Parser e s (PrimArray Word16) -- ^ Native-endian elements
word16Array e !n = case targetByteOrder of
  LittleEndian -> fmap (asWord16s . Bytes.toByteArrayClone) (P.take e (n * 2))
  BigEndian -> do
    bs <- P.take e (n * 2)
    let r = swapArray16 bs
    pure (asWord16s r)

-- | Parse an array of little-endian unsigned 32-bit words.
word32Array ::
     e -- ^ Error message if not enough bytes are present
  -> Int -- ^ Number of little-endian 32-bit words to consume
  -> Parser e s (PrimArray Word32) -- ^ Native-endian elements
word32Array e !n = case targetByteOrder of
  LittleEndian -> fmap (asWord32s . Bytes.toByteArrayClone) (P.take e (n * 4))
  BigEndian -> do
    bs <- P.take e (n * 4)
    let r = swapArray32 bs
    pure (asWord32s r)

-- | Parse an array of little-endian unsigned 64-bit words.
word64Array ::
     e -- ^ Error message if not enough bytes are present
  -> Int -- ^ Number of little-endian 64-bit words to consume
  -> Parser e s (PrimArray Word64) -- ^ Native-endian elements
word64Array e !n = case targetByteOrder of
  LittleEndian -> fmap (asWord64s . Bytes.toByteArrayClone) (P.take e (n * 8))
  BigEndian -> do
    bs <- P.take e (n * 8)
    let r = swapArray64 bs
    pure (asWord64s r)

-- | Parse an array of little-endian unsigned 128-bit words.
word128Array ::
     e -- ^ Error message if not enough bytes are present
  -> Int -- ^ Number of little-endian 128-bit words to consume
  -> Parser e s (PrimArray Word128) -- ^ Native-endian elements
word128Array e !n = case targetByteOrder of
  LittleEndian -> fmap (asWord128s . Bytes.toByteArrayClone) (P.take e (n * 16))
  BigEndian -> do
    bs <- P.take e (n * 16)
    let r = swapArray128 bs
    pure (asWord128s r)

-- | Parse an array of little-endian unsigned 256-bit words.
word256Array ::
     e -- ^ Error message if not enough bytes are present
  -> Int -- ^ Number of little-endian 256-bit words to consume
  -> Parser e s (PrimArray Word256) -- ^ Native-endian elements
word256Array e !n = case targetByteOrder of
  LittleEndian -> fmap (asWord256s . Bytes.toByteArrayClone) (P.take e (n * 32))
  BigEndian -> do
    bs <- P.take e (n * 32)
    let r = swapArray256 bs
    pure (asWord256s r)

-- | Parse an array of little-endian signed 64-bit words.
int64Array ::
     e -- ^ Error message if not enough bytes are present
  -> Int -- ^ Number of little-endian 64-bit words to expect
  -> Parser e s (PrimArray Int64) -- ^ Native-endian elements
int64Array e !n = do
  PrimArray x <- word64Array e n
  pure (PrimArray x)

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
word16 e = uneffectful $ \chunk -> if length chunk >= 2
  then
    let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
        wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
     in Success
          (fromIntegral @Word @Word16 (unsafeShiftL (fromIntegral wb) 8 .|. fromIntegral wa))
          (offset chunk + 2) (length chunk - 2)
  else Failure e

-- | Unsigned 32-bit word.
word32 :: e -> Parser e s Word32
word32 e = uneffectful $ \chunk -> if length chunk >= 4
  then
    let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
        wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
        wc = PM.indexByteArray (array chunk) (offset chunk + 2) :: Word8
        wd = PM.indexByteArray (array chunk) (offset chunk + 3) :: Word8
     in Success
          (fromIntegral @Word @Word32
            ( unsafeShiftL (fromIntegral wd) 24 .|.
              unsafeShiftL (fromIntegral wc) 16 .|.
              unsafeShiftL (fromIntegral wb) 8 .|.
              fromIntegral wa
            )
          )
          (offset chunk + 4) (length chunk - 4)
  else Failure e

-- | Unsigned 64-bit word.
word64 :: e -> Parser e s Word64
word64 e = uneffectful $ \chunk -> if length chunk >= 8
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
          ( unsafeShiftL (fromIntegral wh) 56 .|.
            unsafeShiftL (fromIntegral wg) 48 .|.
            unsafeShiftL (fromIntegral wf) 40 .|.
            unsafeShiftL (fromIntegral we) 32 .|.
            unsafeShiftL (fromIntegral wd) 24 .|.
            unsafeShiftL (fromIntegral wc) 16 .|.
            unsafeShiftL (fromIntegral wb) 8 .|.
            fromIntegral wa
          )
          (offset chunk + 8) (length chunk - 8)
  else Failure e

-- | Unsigned 256-bit word.
word256 :: e -> Parser e s Word256
word256 e = (\d c b a -> Word256 a b c d) <$> word64 e <*> word64 e <*> word64 e <*> word64 e

-- | Unsigned 128-bit word.
word128 :: e -> Parser e s Word128
word128 e = liftA2 (flip Word128) (word64 e) (word64 e)

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
