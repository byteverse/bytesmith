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

-- | Parse input as though it were text encoded by
-- ISO 8859-1 (Latin-1). All byte sequences are valid
-- text under ISO 8859-1.
module Data.Bytes.Parser.Latin
  ( -- * Matching
    char
  , char2
  , char3
  , char4
    -- * Get Character
  , any
  , opt
  , opt#
    -- * Skip
  , skipDigits
  , skipDigits1
  , skipChar
  , skipChar1
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser(..),uneffectful,Result#,uneffectful#)
import Data.Bytes.Parser.Internal (InternalResult(..),indexLatinCharArray,upcastUnitSuccess)
import Data.Word (Word8)
import Data.Char (ord)
import GHC.Exts (Int(I#),Int#,Char#,(+#),(-#),indexCharArray#)

import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM

-- | Consume the next character, failing if it does not
-- match the expected value or if there is no more input.
char :: e -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
char e !c = uneffectful $ \chunk -> if length chunk > 0
  then if indexLatinCharArray (array chunk) (offset chunk) == c
    then InternalSuccess () (offset chunk + 1) (length chunk - 1)
    else InternalFailure e
  else InternalFailure e

-- | Consume the next two characters, failing if they do
-- not match they expected values.
--
-- > char2 e a b === char e a *> char e b
char2 :: e -> Char -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
char2 e !c0 !c1 = uneffectful $ \chunk ->
  if | length chunk > 1
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
         -> InternalSuccess () (offset chunk + 2) (length chunk - 2)
     | otherwise -> InternalFailure e

-- | Consume the three characters, failing if they do
-- not match they expected values.
--
-- > char3 e a b c === char e a *> char e b *> char e c
char3 :: e -> Char -> Char -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
char3 e !c0 !c1 !c2 = uneffectful $ \chunk ->
  if | length chunk > 2
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
         -> InternalSuccess () (offset chunk + 3) (length chunk - 3)
     | otherwise -> InternalFailure e

-- | Consume the four characters, failing if they do
-- not match they expected values.
--
-- > char4 e a b c d === char e a *> char e b *> char e c *> char e d
char4 :: e -> Char -> Char -> Char -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
char4 e !c0 !c1 !c2 !c3 = uneffectful $ \chunk ->
  if | length chunk > 3
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
         -> InternalSuccess () (offset chunk + 4) (length chunk - 4)
     | otherwise -> InternalFailure e

-- | Consumes and returns the next character in the input.
any :: e -> Parser e s Char
any e = uneffectful $ \chunk -> if length chunk > 0
  then
    let c = indexLatinCharArray (array chunk) (offset chunk)
     in InternalSuccess c (offset chunk + 1) (length chunk - 1)
  else InternalFailure e

-- | Consume a character from the input or return Nothing if
-- end of the stream has been reached. Since ISO 8859-1 maps every
-- bytes to a character, this parser never fails.
opt :: Parser e s (Maybe Char)
opt = uneffectful $ \chunk -> case length chunk of
  0 -> InternalSuccess Nothing (offset chunk) (length chunk)
  _ -> InternalSuccess
    (Just (indexLatinCharArray (array chunk) (offset chunk)))
    (offset chunk + 1) (length chunk - 1)

-- | Variant of @opt@ with unboxed result.
opt# :: Parser e s (# (# #) | Char# #)
{-# inline opt# #-}
opt# = Parser
  (\(# arr, off, len #) s0 -> case len of
    0# -> (# s0, (# | (# (# (# #) | #), off, len #) #) #)
    _ -> (# s0, (# | (# (# | indexCharArray# arr off #), off +# 1#, len -# 1# #) #) #)
  )

skipDigitsAsciiLoop ::
     Bytes -- Chunk
  -> (# Int#, Int# #)
skipDigitsAsciiLoop !c = if length c > 0
  then
    let w = indexLatinCharArray (array c) (offset c)
     in if w >= '0' && w <= '9'
          then skipDigitsAsciiLoop (Bytes.unsafeDrop 1 c)
          else (# unI (offset c), unI (length c) #)
  else (# unI (offset c), unI (length c) #)

skipDigitsAscii1LoopStart ::
     e
  -> Bytes -- chunk
  -> Result# e ()
skipDigitsAscii1LoopStart e !c = if length c > 0
  then 
    let w = indexLatinCharArray (array c) (offset c)
     in if w >= '0' && w <= '9'
          then upcastUnitSuccess (skipDigitsAsciiLoop (Bytes.unsafeDrop 1 c))
          else (# e | #)
  else (# e | #)

-- | Variant of 'skipDigits' that requires at least one digit
-- to be present.
skipDigits1 :: e -> Parser e s ()
skipDigits1 e = uneffectful# $ \c ->
  skipDigitsAscii1LoopStart e c

-- | Skip the characters @0-9@ until a non-digit is encountered.
-- This parser does not fail.
skipDigits :: Parser e s ()
skipDigits = uneffectful# $ \c ->
  upcastUnitSuccess (skipDigitsAsciiLoop c)

unI :: Int -> Int#
unI (I# w) = w

-- | Skip the character any number of times. This succeeds
-- even if the character was not present.
skipChar :: Char -> Parser e s ()
skipChar !w = uneffectful# $ \c ->
  upcastUnitSuccess (skipLoop w c)

-- | Skip the character any number of times. It must occur
-- at least once or else this will fail.
skipChar1 :: e -> Char -> Parser e s ()
skipChar1 e !w = uneffectful# $ \c ->
  skipLoop1Start e w c

skipLoop ::
     Char -- byte to match
  -> Bytes -- Chunk
  -> (# Int#, Int# #)
skipLoop !w !c = if length c > 0
  then if indexLatinCharArray (array c) (offset c) == w
    then skipLoop w (Bytes.unsafeDrop 1 c)
    else (# unI (offset c), unI (length c) #)
  else (# unI (offset c), unI (length c) #)

skipLoop1Start ::
     e
  -> Char -- byte to match
  -> Bytes -- chunk
  -> Result# e ()
skipLoop1Start e !w !chunk0 = if length chunk0 > 0
  then if indexLatinCharArray (array chunk0) (offset chunk0) == w
    then upcastUnitSuccess (skipLoop w (Bytes.unsafeDrop 1 chunk0))
    else (# e | #)
  else (# e | #)
