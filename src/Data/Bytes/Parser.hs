{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

module Data.Bytes.Parser
  ( -- * Types
    Parser(..)
  , Parser#(..)
  , Result(..)
    -- * Run Parsers
  , parseByteArray
  , parseBytes
    -- * Build Parsers
  , ascii
  , decWord
  , decWord8
  , decWord16
  , endOfInput
  , skipUntilAsciiConsume
  , skipAscii
  , skipAscii1
  , skipAlphaAscii
  , skipAlphaAscii1
  , skipDigitsAscii
  , skipDigitsAscii1
    -- * Expose Internals
  , cursor
  , expose
    -- * Cut down on boxing
  , unboxWord32
  , boxWord32
  ) where

import Prelude hiding (length)

import Data.Char (ord)
import Data.Kind (Type)
import GHC.ST (ST(..),runST)
import GHC.Exts (Word(W#),Word#,TYPE,State#,Int#,ByteArray#)
import GHC.Exts (Int(I#),RuntimeRep)
import GHC.Word (Word16(W16#),Word8(W8#),Word32(W32#))
import Data.Bytes.Types (Bytes(..))
import Data.Primitive (ByteArray(..))

import qualified Data.Primitive as PM
import qualified Control.Monad

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Either# (a :: Type) (b :: TYPE r) = (# a | b #)
type ST# s (a :: TYPE r) = State# s -> (# State# s, a #)
type Result# e (a :: TYPE r) =
  (# e
  | (# a, Int#, Int# #) #) -- ints are offset and length

data Result e a
  = Failure e
  | Success !a !Int !Int -- offset and length

newtype Parser# :: forall (r :: RuntimeRep). Type -> Type -> TYPE r -> Type where
  Parser# :: forall (r :: RuntimeRep) (e :: Type) (s :: Type) (a :: TYPE r).
    { runParser# :: Bytes# -> ST# s (Result# e a) } -> Parser# e s a

newtype Parser :: Type -> Type -> Type -> Type where
  Parser :: { runParser :: Bytes# -> ST# s (Result# e a) } -> Parser e s a

parseByteArray :: (forall s. Parser e s a) -> ByteArray -> Result e a
parseByteArray p b =
  parseBytes p (Bytes b 0 (PM.sizeofByteArray b))

parseBytes :: forall e a. (forall s. Parser e s a) -> Bytes -> Result e a
parseBytes p !b = runST action
  where
  action :: forall s. ST s (Result e a)
  action = case p @s of
    Parser f -> ST
      (\s0 -> case f (unboxBytes b) s0 of
        (# s1, r #) -> (# s1, boxResult r #)
      )

instance Functor (Parser e s) where
  {-# inline fmap #-}
  fmap f (Parser g) = Parser
    (\x s0 -> case g x s0 of
      (# s1, r #) -> case r of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# a, b, c #) #) -> (# s1, (# | (# f a, b, c #) #) #)
    )

instance Applicative (Parser e s) where
  pure = pureParser
  (<*>) = Control.Monad.ap

instance Monad (Parser e s) where
  {-# inline return #-}
  {-# inline (>>=) #-}
  return = pureParser
  Parser f >>= g = Parser
    (\x@(# arr, _, _ #) s0 -> case f x s0 of
      (# s1, r0 #) -> case r0 of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# y, b, c #) #) ->
          runParser (g y) (# arr, b, c #) s1
    )

pureParser :: a -> Parser e s a
pureParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

upcastUnitSuccess :: (# Int#, Int# #) -> Result# e ()
upcastUnitSuccess (# b, c #) = (# | (# (), b, c #) #)

upcastWordResult :: Result# e Word# -> Result# e Word
upcastWordResult (# e | #) = (# e | #)
upcastWordResult (# | (# a, b, c #) #) = (# | (# W# a, b, c #) #)

-- Precondition: the word is small enough
upcastWord16Result :: Result# e Word# -> Result# e Word16
upcastWord16Result (# e | #) = (# e | #)
upcastWord16Result (# | (# a, b, c #) #) = (# | (# W16# a, b, c #) #)

-- Precondition: the word is small enough
upcastWord8Result :: Result# e Word# -> Result# e Word8
upcastWord8Result (# e | #) = (# e | #)
upcastWord8Result (# | (# a, b, c #) #) = (# | (# W8# a, b, c #) #)

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- | Get the current offset into the chunk. Using this makes
-- it possible to observe the internal difference between 'Bytes'
-- that refer to equivalent slices. Be careful.
cursor :: Parser e s Int
cursor = uneffectful $ \chunk ->
  Success (offset chunk) (offset chunk) (length chunk)

-- | Return the byte array being parsed. This includes bytes
-- that preceed the current offset and may include bytes that
-- go beyond the length. This is somewhat dangerous, so only
-- use this is you know what you're doing.
expose :: Parser e s ByteArray
expose = uneffectful $ \chunk ->
  Success (array chunk) (offset chunk) (length chunk)

uneffectful :: (Bytes -> Result e a) -> Parser e s a
uneffectful f = Parser
  ( \b s0 -> (# s0, unboxResult (f (boxBytes b)) #) )

uneffectful# :: (Bytes -> Result# e a) -> Parser e s a
uneffectful# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

-- | Only valid for characters with a Unicode code point lower
-- than 128. This consumes a single byte, decoding it as an ASCII
-- character.
ascii :: e -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
ascii e !c = uneffectful $ \chunk -> if length chunk > 0
  then if PM.indexByteArray (array chunk) (offset chunk) == c2w c
    then Success () (offset chunk + 1) (length chunk - 1)
    else Failure e
  else Failure e

-- | Skip ASCII-encoded digits until a non-digit is encountered.
skipDigitsAscii :: Parser e s ()
skipDigitsAscii = uneffectful# $ \c ->
  upcastUnitSuccess (skipDigitsAsciiLoop c)

-- | Skip uppercase and lowercase letters until a non-alpha
-- character is encountered.
skipDigitsAscii1 :: e -> Parser e s ()
skipDigitsAscii1 e = uneffectful# $ \c ->
  skipDigitsAscii1LoopStart e c

-- | Skip uppercase and lowercase letters until a non-alpha
-- character is encountered.
skipAlphaAscii :: Parser e s ()
skipAlphaAscii = uneffectful# $ \c ->
  upcastUnitSuccess (skipAlphaAsciiLoop c)

-- | Skip uppercase and lowercase letters until a non-alpha
-- character is encountered.
skipAlphaAscii1 :: e -> Parser e s ()
skipAlphaAscii1 e = uneffectful# $ \c ->
  skipAlphaAsciiLoop1Start e c

-- | Skip the character any number of times. This succeeds
-- even if the character was not present.
skipAscii :: Char -> Parser e s ()
skipAscii !w = uneffectful# $ \c ->
  upcastUnitSuccess (skipLoop (c2w w) c)

-- | Skip the character any number of times. It must occur
-- at least once or else this will fail.
skipAscii1 :: e -> Char -> Parser e s ()
skipAscii1 e !w = uneffectful# $ \c ->
  skipLoop1Start e (c2w w) c

skipDigitsAsciiLoop ::
     Bytes -- Chunk
  -> (# Int#, Int# #)
skipDigitsAsciiLoop !c = if length c > 0
  then
    let w = PM.indexByteArray (array c) (offset c) :: Word8
     in if w >= c2w '0' && w <= c2w '9'
          then skipDigitsAsciiLoop (advance 1 c)
          else (# unI (offset c), unI (length c) #)
  else (# unI (offset c), unI (length c) #)

skipAlphaAsciiLoop ::
     Bytes -- Chunk
  -> (# Int#, Int# #)
skipAlphaAsciiLoop !c = if length c > 0
  then
    let w = PM.indexByteArray (array c) (offset c) :: Word8
     in if (w >= c2w 'a' && w <= c2w 'z') || (w >= c2w 'A' && w <= c2w 'Z')
          then skipAlphaAsciiLoop (advance 1 c)
          else (# unI (offset c), unI (length c) #)
  else (# unI (offset c), unI (length c) #)

skipAlphaAsciiLoop1Start ::
     e
  -> Bytes -- chunk
  -> Result# e ()
skipAlphaAsciiLoop1Start e !c = if length c > 0
  then 
    let w = PM.indexByteArray (array c) (offset c) :: Word8
     in if (w >= c2w 'a' && w <= c2w 'z') || (w >= c2w 'A' && w <= c2w 'Z')
          then upcastUnitSuccess (skipAlphaAsciiLoop (advance 1 c))
          else (# e | #)
  else (# e | #)

skipDigitsAscii1LoopStart ::
     e
  -> Bytes -- chunk
  -> Result# e ()
skipDigitsAscii1LoopStart e !c = if length c > 0
  then 
    let w = PM.indexByteArray (array c) (offset c) :: Word8
     in if w >= c2w '0' && w <= c2w '9'
          then upcastUnitSuccess (skipDigitsAsciiLoop (advance 1 c))
          else (# e | #)
  else (# e | #)

skipLoop ::
     Word8 -- byte to match
  -> Bytes -- Chunk
  -> (# Int#, Int# #)
skipLoop !w !c = if length c > 0
  then if PM.indexByteArray (array c) (offset c) == w
    then skipLoop w (advance 1 c)
    else (# unI (offset c), unI (length c) #)
  else (# unI (offset c), unI (length c) #)

skipLoop1Start ::
     e
  -> Word8 -- byte to match
  -> Bytes -- chunk
  -> Result# e ()
skipLoop1Start e !w !chunk0 = if length chunk0 > 0
  then if PM.indexByteArray (array chunk0) (offset chunk0) == w
    then upcastUnitSuccess (skipLoop w (advance 1 chunk0))
    else (# e | #)
  else (# e | #)

skipUntilAsciiConsume :: e -> Char -> Parser e s ()
skipUntilAsciiConsume e !w = uneffectful# $ \c ->
  skipUntilConsumeLoop e (c2w w) c

skipUntilConsumeLoop ::
     e -- Error message
  -> Word8 -- byte to match
  -> Bytes -- Chunk
  -> Result# e ()
skipUntilConsumeLoop e !w !c = if length c > 0
  then if PM.indexByteArray (array c) (offset c) /= w
    then skipUntilConsumeLoop e w (advance 1 c)
    else (# | (# (), unI (offset c + 1), unI (length c - 1) #) #)
  else (# e | #)

-- | Fails if there is still more input remaining.
endOfInput :: e -> Parser e s ()
-- GHC should decide to inline this after optimization.
endOfInput e = uneffectful $ \chunk -> if length chunk == 0
  then Success () (offset chunk) 0
  else Failure e

decWord8 :: e -> Parser e s Word8
decWord8 e = Parser
  (\chunk0 s0 -> case decSmallWordStart e 256 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord8Result r #)
  )

decWord16 :: e -> Parser e s Word16
decWord16 e = Parser
  (\chunk0 s0 -> case decSmallWordStart e 65536 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord16Result r #)
  )

decWord :: e -> Parser e s Word
decWord e = Parser
  (\chunk0 s0 -> case decWordStart e (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWordResult r #)
  )

decWordStart ::
     e -- Error message
  -> Bytes -- Chunk
  -> ST# s (Result# e Word# )
decWordStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then (# s0, decWordMore e w (advance 1 chunk0) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

decSmallWordStart ::
     e -- Error message
  -> Word -- Upper Bound
  -> Bytes -- Chunk
  -> ST# s (Result# e Word# )
decSmallWordStart e !limit !chunk0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then (# s0, decSmallWordMore e w limit (advance 1 chunk0) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

-- This will not inline since it is recursive, but worker
-- wrapper will still happen.
decWordMore ::
     e -- Error message
  -> Word -- Accumulator
  -> Bytes -- Chunk
  -> Result# e Word#
decWordMore e !acc !chunk0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then decWordMore e (acc * 10 + w)
                 (advance 1 chunk0)
          else (# | (# unW acc, unI (offset chunk0), unI (length chunk0)  #) #)
  else (# | (# unW acc, unI (offset chunk0), 0# #) #)

decSmallWordMore ::
     e -- Error message
  -> Word -- Accumulator
  -> Word -- Upper Bound
  -> Bytes -- Chunk
  -> Result# e Word#
decSmallWordMore e !acc !limit !chunk0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then
            let w' = acc * 10 + w
             in if w' < limit
                  then decSmallWordMore e w' limit (advance 1 chunk0)
                  else (# e | #)
          else (# | (# unW acc, unI (offset chunk0), unI (length chunk0)  #) #)
  else (# | (# unW acc, unI (offset chunk0), 0# #) #)

advance :: Int -> Bytes -> Bytes
advance n (Bytes arr off len) = Bytes arr (off + n) (len - n)

unW :: Word -> Word#
unW (W# w) = w

unI :: Int -> Int#
unI (I# w) = w

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

unboxResult :: Result e a -> Result# e a
unboxResult (Success a (I# b) (I# c)) = (# | (# a, b, c #) #)
unboxResult (Failure e) = (# e | #)

boxResult :: Result# e a -> Result e a
boxResult (# | (# a, b, c #) #) = Success a (I# b) (I# c)
boxResult (# e | #) = Failure e

unboxWord32 :: Parser s e Word32 -> Parser# s e Word#
unboxWord32 (Parser f) = Parser#
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# W32# a, b, c #) #) -> (# s1, (# | (# a, b, c #) #) #)
  )

boxWord32 :: Parser# s e Word# -> Parser s e Word32
boxWord32 (Parser# f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W32# a, b, c #) #) #)
  )
