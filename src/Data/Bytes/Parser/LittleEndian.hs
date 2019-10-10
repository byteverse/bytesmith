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
  ( -- * Unsigned
    word8
  , word16
  , word32
  , word64
    -- * Signed
  , int8
  , int16
  , int32
  , int64
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bits ((.|.),unsafeShiftL)
import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser,uneffectful)
import Data.Bytes.Parser.Internal (InternalResult(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)

import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM

-- | Unsigned 8-bit word.
word8 :: e -> Parser e s Word8
word8 = P.any

-- | Unsigned 16-bit word.
word16 :: e -> Parser e s Word16
word16 e = uneffectful $ \chunk -> if length chunk >= 2
  then
    let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
        wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
     in InternalSuccess
          (fromIntegral @Word @Word16 (unsafeShiftL (fromIntegral wb) 8 .|. fromIntegral wa))
          (offset chunk + 2) (length chunk - 2)
  else InternalFailure e

-- | Unsigned 32-bit word.
word32 :: e -> Parser e s Word32
word32 e = uneffectful $ \chunk -> if length chunk >= 4
  then
    let wa = PM.indexByteArray (array chunk) (offset chunk) :: Word8
        wb = PM.indexByteArray (array chunk) (offset chunk + 1) :: Word8
        wc = PM.indexByteArray (array chunk) (offset chunk + 2) :: Word8
        wd = PM.indexByteArray (array chunk) (offset chunk + 3) :: Word8
     in InternalSuccess
          (fromIntegral @Word @Word32
            ( unsafeShiftL (fromIntegral wd) 24 .|.
              unsafeShiftL (fromIntegral wc) 16 .|.
              unsafeShiftL (fromIntegral wb) 8 .|.
              fromIntegral wa
            )
          )
          (offset chunk + 4) (length chunk - 4)
  else InternalFailure e

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
     in InternalSuccess
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
  else InternalFailure e

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

