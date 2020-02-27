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
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

module Data.Bytes.Parser.Internal
  ( Parser(..)
  , InternalResult(..)
  , InternalStep(..)
  , Bytes#
  , ST#
  , Result#
  , unfailing
  , uneffectful
  , uneffectful#
  , uneffectfulInt#
  , boxBytes
  , unboxBytes
  , unboxResult
  , fail
  , indexLatinCharArray
  , upcastUnitSuccess
    -- Swapping
  , swapArray16
  , swapArray32
  , swapArray64
  , swapArray128
  , swapArray256
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Control.Applicative (Alternative)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Primitive (ByteArray(ByteArray))
import Data.Bytes.Types (Bytes(..))
import Data.Kind (Type)
import Data.Word (Word8)
import GHC.Exts (TYPE,RuntimeRep,Int(I#),Int#,State#,ByteArray#,Char(C#))

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | A non-resumable parser.
newtype Parser :: forall (r :: RuntimeRep). Type -> Type -> TYPE r -> Type where
  Parser :: forall (r :: RuntimeRep) (e :: Type) (s :: Type) (a :: TYPE r).
    { runParser :: (# ByteArray#, Int#, Int# #) -> ST# s (Result# e a) } -> Parser e s a

-- The result of running a parser. Used internally.
data InternalResult e a
  = InternalFailure e
    -- An error message indicating what went wrong.
  | InternalSuccess !a !Int !Int
    -- The parsed value, the offset after the last consumed byte, and the
    -- number of bytes remaining in parsed slice.

data InternalStep a = InternalStep !a !Int !Int

uneffectful :: (Bytes -> InternalResult e a) -> Parser e s a
{-# inline uneffectful #-}
uneffectful f = Parser
  ( \b s0 -> (# s0, unboxResult (f (boxBytes b)) #) )

-- This is like uneffectful but for parsers that always succeed.
-- These combinators typically have names that begin with @try@.
unfailing :: (Bytes -> InternalStep a) -> Parser e s a
{-# inline unfailing #-}
unfailing f = Parser
  ( \b s0 -> (# s0, case f (boxBytes b) of { InternalStep a (I# off) (I# len) -> (# | (# a, off, len #) #) } #) )

boxBytes :: Bytes# -> Bytes
{-# inline boxBytes #-}
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unboxBytes :: Bytes -> Bytes#
{-# inline unboxBytes #-}
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

type Bytes# = (# ByteArray#, Int#, Int# #)
type ST# s (a :: TYPE r) = State# s -> (# State# s, a #)
type Result# e (a :: TYPE r) =
  (# e
  | (# a, Int#, Int# #) #) -- ints are offset and length

unboxResult :: InternalResult e a -> Result# e a
unboxResult (InternalSuccess a (I# b) (I# c)) = (# | (# a, b, c #) #)
unboxResult (InternalFailure e) = (# e | #)

-- | Combines the error messages using '<>' when both
-- parsers fail.
instance Monoid e => Alternative (Parser e s) where
  {-# inline empty #-}
  {-# inline (<|>) #-}
  empty = fail mempty
  Parser f <|> Parser g = Parser
    (\x s0 -> case f x s0 of
      (# s1, r0 #) -> case r0 of
        (# eRight | #) -> case g x s1 of
          (# s2, r1 #) -> case r1 of
            (# eLeft | #) -> (# s2, (# eRight <> eLeft | #) #)
            (# | r #) -> (# s2, (# | r #) #)
        (# | r #) -> (# s1, (# | r #) #)
    )

-- | Fail with the provided error message.
fail ::
     e -- ^ Error message
  -> Parser e s a
fail e = uneffectful $ \_ -> InternalFailure e

instance Applicative (Parser e s) where
  pure = pureParser
  (<*>) = Control.Monad.ap

instance Monad (Parser e s) where
  {-# inline return #-}
  {-# inline (>>=) #-}
  return = pureParser
  (>>=) = bindParser

instance Functor (Parser e s) where
  {-# inline fmap #-}
  fmap f (Parser g) = Parser
    (\x s0 -> case g x s0 of
      (# s1, r #) -> case r of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# a, b, c #) #) -> (# s1, (# | (# f a, b, c #) #) #)
    )

indexLatinCharArray :: ByteArray -> Int -> Char
{-# inline indexLatinCharArray #-}
indexLatinCharArray (ByteArray arr) (I# off) =
  C# (Exts.indexCharArray# arr off)

uneffectful# :: (Bytes -> Result# e a) -> Parser e s a
{-# inline uneffectful# #-}
uneffectful# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

uneffectfulInt# :: (Bytes -> Result# e Int# ) -> Parser e s Int#
{-# inline uneffectfulInt# #-}
uneffectfulInt# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

upcastUnitSuccess :: (# Int#, Int# #) -> Result# e ()
{-# inline upcastUnitSuccess #-}
upcastUnitSuccess (# b, c #) = (# | (# (), b, c #) #)

swapArray16 :: Bytes -> ByteArray
swapArray16 (Bytes{array,offset,length}) = runByteArrayST $ do
  dst <- PM.newByteArray length
  let go !ixSrc !ixDst !len = if len > 0
        then do
          let v0 = PM.indexByteArray array ixSrc :: Word8
              v1 = PM.indexByteArray array (ixSrc + 1) :: Word8
          PM.writeByteArray dst ixDst v1
          PM.writeByteArray dst (ixDst + 1) v0
          go (ixSrc + 2) (ixDst + 2) (len - 2)
        else pure ()
  go offset 0 length
  PM.unsafeFreezeByteArray dst

swapArray32 :: Bytes -> ByteArray
swapArray32 (Bytes{array,offset,length}) = runByteArrayST $ do
  dst <- PM.newByteArray length
  let go !ixSrc !ixDst !len = if len > 0
        then do
          let v0 = PM.indexByteArray array ixSrc :: Word8
              v1 = PM.indexByteArray array (ixSrc + 1) :: Word8
              v2 = PM.indexByteArray array (ixSrc + 2) :: Word8
              v3 = PM.indexByteArray array (ixSrc + 3) :: Word8
          PM.writeByteArray dst ixDst v3
          PM.writeByteArray dst (ixDst + 1) v2
          PM.writeByteArray dst (ixDst + 2) v1
          PM.writeByteArray dst (ixDst + 3) v0
          go (ixSrc + 4) (ixDst + 4) (len - 4)
        else pure ()
  go offset 0 length
  PM.unsafeFreezeByteArray dst

swapArray64 :: Bytes -> ByteArray
swapArray64 (Bytes{array,offset,length}) = runByteArrayST $ do
  dst <- PM.newByteArray length
  let go !ixSrc !ixDst !len = if len > 0
        then do
          let v0 = PM.indexByteArray array ixSrc :: Word8
              v1 = PM.indexByteArray array (ixSrc + 1) :: Word8
              v2 = PM.indexByteArray array (ixSrc + 2) :: Word8
              v3 = PM.indexByteArray array (ixSrc + 3) :: Word8
              v4 = PM.indexByteArray array (ixSrc + 4) :: Word8
              v5 = PM.indexByteArray array (ixSrc + 5) :: Word8
              v6 = PM.indexByteArray array (ixSrc + 6) :: Word8
              v7 = PM.indexByteArray array (ixSrc + 7) :: Word8
          PM.writeByteArray dst ixDst v7
          PM.writeByteArray dst (ixDst + 1) v6
          PM.writeByteArray dst (ixDst + 2) v5
          PM.writeByteArray dst (ixDst + 3) v4
          PM.writeByteArray dst (ixDst + 4) v3
          PM.writeByteArray dst (ixDst + 5) v2
          PM.writeByteArray dst (ixDst + 6) v1
          PM.writeByteArray dst (ixDst + 7) v0
          go (ixSrc + 8) (ixDst + 8) (len - 8)
        else pure ()
  go offset 0 length
  PM.unsafeFreezeByteArray dst

swapArray128 :: Bytes -> ByteArray
swapArray128 (Bytes{array,offset,length}) = runByteArrayST $ do
  dst <- PM.newByteArray length
  let go !ixSrc !ixDst !len = if len > 0
        then do
          let v0 = PM.indexByteArray array ixSrc :: Word8
              v1 = PM.indexByteArray array (ixSrc + 1) :: Word8
              v2 = PM.indexByteArray array (ixSrc + 2) :: Word8
              v3 = PM.indexByteArray array (ixSrc + 3) :: Word8
              v4 = PM.indexByteArray array (ixSrc + 4) :: Word8
              v5 = PM.indexByteArray array (ixSrc + 5) :: Word8
              v6 = PM.indexByteArray array (ixSrc + 6) :: Word8
              v7 = PM.indexByteArray array (ixSrc + 7) :: Word8
              v8 = PM.indexByteArray array (ixSrc + 8) :: Word8
              v9 = PM.indexByteArray array (ixSrc + 9) :: Word8
              v10 = PM.indexByteArray array (ixSrc + 10) :: Word8
              v11 = PM.indexByteArray array (ixSrc + 11) :: Word8
              v12 = PM.indexByteArray array (ixSrc + 12) :: Word8
              v13 = PM.indexByteArray array (ixSrc + 13) :: Word8
              v14 = PM.indexByteArray array (ixSrc + 14) :: Word8
              v15 = PM.indexByteArray array (ixSrc + 15) :: Word8
          PM.writeByteArray dst ixDst v15
          PM.writeByteArray dst (ixDst + 1) v14
          PM.writeByteArray dst (ixDst + 2) v13
          PM.writeByteArray dst (ixDst + 3) v12
          PM.writeByteArray dst (ixDst + 4) v11
          PM.writeByteArray dst (ixDst + 5) v10
          PM.writeByteArray dst (ixDst + 6) v9
          PM.writeByteArray dst (ixDst + 7) v8
          PM.writeByteArray dst (ixDst + 8) v7
          PM.writeByteArray dst (ixDst + 9) v6
          PM.writeByteArray dst (ixDst + 10) v5
          PM.writeByteArray dst (ixDst + 11) v4
          PM.writeByteArray dst (ixDst + 12) v3
          PM.writeByteArray dst (ixDst + 13) v2
          PM.writeByteArray dst (ixDst + 14) v1
          PM.writeByteArray dst (ixDst + 15) v0
          go (ixSrc + 16) (ixDst + 16) (len - 16)
        else pure ()
  go offset 0 length
  PM.unsafeFreezeByteArray dst

swapArray256 :: Bytes -> ByteArray
swapArray256 (Bytes{array,offset,length}) = runByteArrayST $ do
  dst <- PM.newByteArray length
  let go !ixSrc !ixDst !len = if len > 0
        then do
          let loop !i
                | i < 32 = do
                    let v = PM.indexByteArray array (ixSrc + i) :: Word8
                    PM.writeByteArray dst (ixDst + (31 - i)) v
                    loop (i + 1)
                | otherwise = pure ()
          loop 0
          go (ixSrc + 32) (ixDst + 32) (len - 32)
        else pure ()
  go offset 0 length
  PM.unsafeFreezeByteArray dst

pureParser :: a -> Parser e s a
{-# inline pureParser #-}
pureParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindParser :: Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# inline bindParser #-}
bindParser (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )
