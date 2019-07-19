{-# language BangPatterns #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}

module Data.ByteStream.Parser
  ( Parser
  , decWord
  ) where

import Prelude hiding (length)

import Data.Kind (Type)
import GHC.Exts (Word(W#),Word#,TYPE,State#,Int#,ByteArray#)
import GHC.Exts (Int(I#))
import Data.Bytes.Types (Bytes(..))
import Data.Word (Word8)
import Data.Primitive (ByteArray(..))

import qualified Data.Primitive as PM

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Either# (a :: Type) (b :: TYPE r) = (# a | b #)
type ST# s (a :: TYPE r) = State# s -> (# State# s, a #)
type Result# es ep s (a :: TYPE r) =
  (# es
  | ep
  | (# a, Bytes#, ByteStream es s #) #)
newtype ByteStream e s = ByteStream
  { runByteStream ::
      ST# s (# e | (# #) | (# Bytes# , ByteStream e s #) #)
  }

data Result es ep s a
  = StreamException es
  | ParseException ep
  | Success !a {-# UNPACK #-} !Bytes (ByteStream es s)
newtype Parser es ep s a = Parser
  (Bytes# -> ByteStream es s -> ST# s (Result# es ep s a))

upcastWordResult :: Result# es ep s Word# -> Result# es ep s Word
upcastWordResult (# es | | #) = (# es | | #)
upcastWordResult (# | ep | #) = (# | ep | #)
upcastWordResult (# | | (# a, b, c #) #) = (# | | (# W# a, b, c #) #)

decWord :: ep -> Parser es ep s Word
decWord e = Parser
  (\chunk0 stream0 s0 -> case decWordStart e (boxBytes chunk0) stream0 s0 of
    (# s1, r #) -> (# s1, upcastWordResult r #)
  )

decWordStart ::
     ep -- Error message
  -> Bytes -- Chunk
  -> ByteStream es s -- stream for pulling more chunks
  -> ST# s (Result# es ep s Word# )
decWordStart e !chunk0 stream0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then decWordMore e w (advance 1 chunk0) stream0 s0
          else (# s0, (# | e | #) #)
  else case runByteStream stream0 s0 of
    (# s1, r #) -> case r of
      (# err | | #) -> (# s1, (# err | | #) #)
      (# | (# #) | #) -> (# s1, (# | e | #) #)
      (# | | (# chunk1, stream1 #) #) ->
        decWordStart e (boxBytes chunk1) stream1 s1

-- This will not inline since it is recursive, but worker
-- wrapper will still happen.
decWordMore ::
     e1 -- Error message
  -> Word -- Accumulator
  -> Bytes -- Chunk
  -> ByteStream es s -- stream for pulling more chunks
  -> ST# s (Result# es ep s Word# )
decWordMore e !acc !chunk0 stream0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then decWordMore e (acc * 10 + w)
                 (advance 1 chunk0) stream0 s0
          else (# s0, (# | | (# unW acc, unboxBytes chunk0, stream0 #) #) #)
  else case runByteStream stream0 s0 of
    (# s1, r #) -> case r of
      (# err | | #) -> (# s1, (# err | | #) #)
      (# | (# #) | #) ->
        (# s1, (# | | (# unW acc, unboxBytes chunk0, stream0 #) #) #)
      (# | | (# chunk1, stream1 #) #) ->
        decWordMore e acc (boxBytes chunk1) stream1 s1

advance :: Int -> Bytes -> Bytes
advance n (Bytes arr off len) = Bytes arr (off + n) (len - n)

unW :: Word -> Word#
unW (W# w) = w

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)


