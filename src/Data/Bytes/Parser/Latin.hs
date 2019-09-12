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
    -- ** Required
    char
  , char2
  , char3
  , char4
    -- ** Try
  , trySatisfy
  , trySatisfyThen
    -- * Get Character
  , any
  , opt
  , opt#
    -- * Skip
  , skipDigits
  , skipDigits1
  , skipChar
  , skipChar1
  , skipThroughChar
    -- * Numbers
    -- ** Decimal
    -- *** Unsigned
  , decWord
  , decWord8
  , decWord16
  , decWord32
    -- *** Signed
  , decUnsignedInt
  , decUnsignedInt#
  , decSignedInt
  , decStandardInt
  , decTrailingInt
  , decSignedInteger
  , decUnsignedInteger
    -- ** Hexadecimal
  , hexWord16
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bits ((.|.))
import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser(..),ST#,uneffectful,Result#,uneffectful#)
import Data.Bytes.Parser.Internal (InternalResult(..),indexLatinCharArray,upcastUnitSuccess)
import Data.Bytes.Parser.Internal (boxBytes)
import Data.Bytes.Parser (bindFromLiftedToInt)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Kind (Type)
import GHC.Exts (Int(I#),Char(C#),Word#,Int#,Char#,(+#),(-#),indexCharArray#)
import GHC.Exts (TYPE,RuntimeRep)
import GHC.Word (Word(W#),Word8(W8#),Word16(W16#),Word32(W32#))

import qualified GHC.Exts as Exts
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM

-- | Runs the predicate on the next character in the input. If the
-- predicate is matched, this consumes the character. Otherwise,
-- the character is not consumed. This returns @False@ if the end
-- of the input has been reached. This never fails.
trySatisfy :: (Char -> Bool) -> Parser e s Bool
trySatisfy f = uneffectful $ \chunk -> case length chunk of
  0 -> InternalSuccess False (offset chunk) (length chunk)
  _ -> case f (indexLatinCharArray (array chunk) (offset chunk)) of
    True -> InternalSuccess True (offset chunk + 1) (length chunk - 1)
    False -> InternalSuccess False (offset chunk) (length chunk)

-- | Runs the function on the next character in the input. If the
-- function returns @Just@, this consumes the character and then
-- runs the parser on the remaining input. If the function returns
-- @Nothing@, this does not consume the tested character, and it
-- runs the default parser on the input (which includes the tested
-- character). If there is no input remaining, this also runs the
-- default parser. This combinator never fails.
trySatisfyThen :: forall (r :: RuntimeRep) (e :: Type) (s :: Type) (a :: TYPE r).
     Parser e s a -- ^ Default parser. Runs on @Nothing@ or end of input.
  -> (Char -> Maybe (Parser e s a)) -- ^ Parser-selecting predicate
  -> Parser e s a
{-# inline trySatisfyThen #-}
trySatisfyThen (Parser g) f = Parser
  (\input@(# arr,off0,len0 #) s0 -> case len0 of
    0# -> g input s0
    _ -> case f (C# (indexCharArray# arr off0)) of
      Nothing -> g input s0
      Just (Parser p) -> p (# arr, off0 +# 1#, len0 -# 1# #) s0
  )

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

-- | Parse a decimal-encoded 8-bit word. If the number is larger
-- than 255, this parser fails.
decWord8 :: e -> Parser e s Word8
decWord8 e = Parser
  (\chunk0 s0 -> case decSmallWordStart e 256 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord8Result r #)
  )

-- | Parse a decimal-encoded 16-bit word. If the number is larger
-- than 65535, this parser fails.
decWord16 :: e -> Parser e s Word16
decWord16 e = Parser
  (\chunk0 s0 -> case decSmallWordStart e 65536 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord16Result r #)
  )

-- | Parse a decimal-encoded 32-bit word. If the number is larger
-- than 4294967295, this parser fails.
decWord32 :: e -> Parser e s Word32
-- This will not work on 32-bit platforms.
decWord32 e = Parser
  (\chunk0 s0 -> case decSmallWordStart e 4294967296 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord32Result r #)
  )

-- | Parse a decimal-encoded number. If the number is too large to be
-- represented by a machine word, this fails with the provided
-- error message. This accepts any number of leading zeroes.
decWord :: e -> Parser e s Word
decWord e = Parser
  (\chunk0 s0 -> case decWordStart e (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWordResult r #)
  )

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
          then (# s0, decSmallWordMore e w limit (Bytes.unsafeDrop 1 chunk0) #)
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
        !acc' = acc * 10 + w
     in if w < 10 && acc' >= acc
          then decWordMore e acc' (Bytes.unsafeDrop 1 chunk0)
          else (# | (# unW acc, unI (offset chunk0), unI (length chunk0)  #) #)
  else (# | (# unW acc, unI (offset chunk0), 0# #) #)


upcastWordResult :: Result# e Word# -> Result# e Word
upcastWordResult (# e | #) = (# e | #)
upcastWordResult (# | (# a, b, c #) #) = (# | (# W# a, b, c #) #)

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
                  then decSmallWordMore e w' limit (Bytes.unsafeDrop 1 chunk0)
                  else (# e | #)
          else (# | (# unW acc, unI (offset chunk0), unI (length chunk0)  #) #)
  else (# | (# unW acc, unI (offset chunk0), 0# #) #)

unW :: Word -> Word#
unW (W# w) = w

decWordStart ::
     e -- Error message
  -> Bytes -- Chunk
  -> ST# s (Result# e Word# )
decWordStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then (# s0, decWordMore e w (Bytes.unsafeDrop 1 chunk0) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

-- Precondition: the word is small enough
upcastWord16Result :: Result# e Word# -> Result# e Word16
upcastWord16Result (# e | #) = (# e | #)
upcastWord16Result (# | (# a, b, c #) #) = (# | (# W16# a, b, c #) #)

-- Precondition: the word is small enough
upcastWord32Result :: Result# e Word# -> Result# e Word32
upcastWord32Result (# e | #) = (# e | #)
upcastWord32Result (# | (# a, b, c #) #) = (# | (# W32# a, b, c #) #)

-- Precondition: the word is small enough
upcastWord8Result :: Result# e Word# -> Result# e Word8
upcastWord8Result (# e | #) = (# e | #)
upcastWord8Result (# | (# a, b, c #) #) = (# | (# W8# a, b, c #) #)

-- | Parse a decimal-encoded number. If the number is too large to be
-- represented by a machine integer, this fails with the provided
-- error message. This rejects input with that is preceeded by plus
-- or minus. Consequently, it does not parse negative numbers. Use
-- 'decStandardInt' or 'decSignedInt' for that purpose. On a 64-bit
-- platform 'decWord' will successfully parse 9223372036854775808
-- (i.e. @2 ^ 63@), but 'decUnsignedInt' will fail. This parser allows
-- leading zeroes.
decUnsignedInt :: e -> Parser e s Int
decUnsignedInt e = Parser
  (\chunk0 s0 -> case decPosIntStart e (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastIntResult r #)
  )

-- | Variant of 'decUnsignedInt' with an unboxed result.
decUnsignedInt# :: e -> Parser e s Int#
decUnsignedInt# e = Parser
  (\chunk0 s0 -> decPosIntStart e (boxBytes chunk0) s0)

-- | Parse a decimal-encoded number. If the number is too large to be
-- represented by a machine integer, this fails with the provided
-- error message. This allows the number to optionally be prefixed
-- by plus or minus. If the sign prefix is not present, the number
-- is interpreted as positive. This allows leading zeroes.
decSignedInt :: e -> Parser e s Int
decSignedInt e = Parser
  (\chunk0 s0 -> case runParser (decSignedInt# e) chunk0 s0 of
    (# s1, r #) -> (# s1, upcastIntResult r #)
  )

-- | Variant of 'decUnsignedInt' that lets the caller supply a leading
-- digit. This is useful when parsing a language where integers with
-- leading zeroes are considered invalid. The caller must consume the
-- plus or minus sign (if either of those are allowed) and the first
-- digit before calling this parser.
decTrailingInt ::
     e -- ^ Error message
  -> Int -- ^ Leading digit, should be between @-9@ and @9@.
  -> Parser e s Int
decTrailingInt e !w = Parser
  (\chunk0 s0 -> case runParser (decTrailingInt# e w) chunk0 s0 of
    (# s1, r #) -> (# s1, upcastIntResult r #)
  )

decTrailingInt# ::
     e -- Error message
  -> Int -- Leading digit, should be between @-9@ and @9@.
  -> Parser e s Int#
decTrailingInt# e !w = if w >= 0
  then Parser (\chunk0 s0 -> (# s0, decPosIntMore e w (boxBytes chunk0) #))
  else Parser (\chunk0 s0 -> (# s0, decNegIntMore e w (boxBytes chunk0) #))

-- | Parse a decimal-encoded number. If the number is too large to be
-- represented by a machine integer, this fails with the provided
-- error message. This allows the number to optionally be prefixed
-- by minus. If the minus prefix is not present, the number
-- is interpreted as positive. The disallows a leading plus sign.
-- For example, 'decStandardInt' rejects @+42@, but 'decSignedInt'
-- allows it.
decStandardInt :: e -> Parser e s Int
decStandardInt e = Parser
  (\chunk0 s0 -> case runParser (decStandardInt# e) chunk0 s0 of
    (# s1, r #) -> (# s1, upcastIntResult r #)
  )

decSignedInt# :: e -> Parser e s Int#
{-# noinline decSignedInt# #-}
decSignedInt# e = any e `bindFromLiftedToInt` \c -> case c of
  '+' -> Parser -- plus sign
    (\chunk0 s0 -> decPosIntStart e (boxBytes chunk0) s0)
  '-' -> Parser -- minus sign
    (\chunk0 s0 -> decNegIntStart e (boxBytes chunk0) s0)
  _ -> Parser -- no sign, there should be a digit here 
    (\chunk0 s0 ->
      let !w = char2Word c - 48
        in if w < 10
             then (# s0, decPosIntMore e (fromIntegral @Word @Int w) (boxBytes chunk0) #)
             else (# s0, (# e | #) #)
    )

-- This is the same as decSignedInt except that we disallow
-- a leading plus sign.
decStandardInt# :: e -> Parser e s Int#
{-# noinline decStandardInt# #-}
decStandardInt# e = any e `bindFromLiftedToInt` \c -> case c of
  '-' -> Parser -- minus sign
    (\chunk0 s0 -> decNegIntStart e (boxBytes chunk0) s0)
  _ -> Parser -- no sign, there should be a digit here 
    (\chunk0 s0 ->
      let !w = char2Word c - 48
        in if w < 10
             then (# s0, decPosIntMore e (fromIntegral @Word @Int w) (boxBytes chunk0) #)
             else (# s0, (# e | #) #)
    )

-- | Parse a decimal-encoded positive integer of arbitrary
-- size. Note: this is not implemented efficiently. This
-- pulls in one digit at a time, multiplying the accumulator
-- by ten each time and adding the new digit. Since
-- arithmetic involving arbitrary-precision integers is
-- somewhat expensive, it would be better to pull in several
-- digits at a time, convert those to a machine-sized integer,
-- then upcast and perform the multiplication and addition.
decUnsignedInteger :: e -> Parser e s Integer
decUnsignedInteger e = Parser
  (\chunk0 s0 -> decUnsignedIntegerStart e (boxBytes chunk0) s0)

decSignedInteger :: e -> Parser e s Integer
{-# noinline decSignedInteger #-}
decSignedInteger e = any e >>= \c -> case c of
  '+' -> do
    decUnsignedInteger e
  '-' -> do
    x <- decUnsignedInteger e
    pure $! negate x
  _ -> Parser -- no sign, there should be a digit here 
    (\chunk0 s0 ->
      let !w = char2Word c - 48 in
      if w < 10
        then
          let !r = decIntegerChunks
                (fromIntegral @Word @Int w)
                10
                0
                (boxBytes chunk0)
           in (# s0, (# | r #) #)
        else (# s0, (# e | #) #)
    )

decPosIntStart ::
     e -- Error message
  -> Bytes -- Chunk
  -> ST# s (Result# e Int# )
decPosIntStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then (# s0, decPosIntMore e (fromIntegral @Word @Int w) (Bytes.unsafeDrop 1 chunk0) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

decNegIntStart ::
     e -- Error message
  -> Bytes -- Chunk
  -> ST# s (Result# e Int# )
decNegIntStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then (# s0, decNegIntMore e (negate (fromIntegral @Word @Int w)) (Bytes.unsafeDrop 1 chunk0) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

decUnsignedIntegerStart ::
     e
  -> Bytes
  -> ST# s (Result# e Integer)
decUnsignedIntegerStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < (10 :: Word8)
          then
            let !r = decIntegerChunks
                  (fromIntegral @Word8 @Int w)
                  10
                  0
                  (Bytes.unsafeDrop 1 chunk0)
             in (# s0, (# | r #) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

-- This will not inline since it is recursive, but worker
-- wrapper will still happen.
decNegIntMore ::
     e -- Error message
  -> Int -- Accumulator
  -> Bytes -- Chunk
  -> Result# e Int#
decNegIntMore e !acc !chunk0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
        !acc' = acc * 10 - (fromIntegral @Word @Int w)
     in if w < 10
          then if acc' <= acc
            then decNegIntMore e acc' (Bytes.unsafeDrop 1 chunk0)
            else (# e | #)
          else (# | (# unI acc, unI (offset chunk0), unI (length chunk0)  #) #)
  else (# | (# unI acc, unI (offset chunk0), 0# #) #)

-- This will not inline since it is recursive, but worker
-- wrapper will still happen. Fails if the accumulator
-- exceeds the size of a machine integer.
decPosIntMore ::
     e -- Error message
  -> Int -- Accumulator
  -> Bytes -- Chunk
  -> Result# e Int#
decPosIntMore e !acc !chunk0 = if len > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
        !acc' = acc * 10 + (fromIntegral @Word @Int w)
     in if w < 10
          then if acc' >= acc
            then decPosIntMore e acc' (Bytes.unsafeDrop 1 chunk0)
            else (# e | #)
          else (# | (# unI acc, unI (offset chunk0), len# #) #)
  else (# | (# unI acc, unI (offset chunk0), 0# #) #)
  where
  !len@(I# len# ) = length chunk0

-- This will not inline since it is recursive, but worker
-- wrapper will still happen. When the accumulator
-- exceeds the size of a machine integer, this pushes the
-- accumulated machine int and the shift amount onto the
-- stack.
-- We are intentionally lazy in the accumulator. There is
-- no need to force this on every iteration. We do however,
-- force it preemptively every time it changes.
-- Because of how we track overflow, we are able to use the
-- same function for both positive and negative numbers.
decIntegerChunks ::
     Int -- Chunk accumulator (e.g. 236)
  -> Int -- Chunk base-ten bound (e.g. 1000)
  -> Integer -- Accumulator
  -> Bytes -- Chunk
  -> (# Integer, Int#, Int# #)
decIntegerChunks !nAcc !eAcc acc !chunk0 = if len > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then let !eAcc' = eAcc * 10 in
            if eAcc' >= eAcc
              then decIntegerChunks
                (nAcc * 10 + fromIntegral @Word @Int w)
                eAcc'
                acc
                (Bytes.unsafeDrop 1 chunk0)
              else
                -- In this case, notice that we deliberately
                -- unconsume the digit that would have caused
                -- an overflow.
                let !r = (acc * fromIntegral @Int @Integer eAcc)
                       + (fromIntegral @Int @Integer nAcc)
                 in decIntegerChunks 0 1 r chunk0
          else
            let !r = (acc * fromIntegral @Int @Integer eAcc)
                   + (fromIntegral @Int @Integer nAcc)
             in (# r, unI (offset chunk0), len# #)
  else
    let !r = (acc * fromIntegral @Int @Integer eAcc)
           + (fromIntegral @Int @Integer nAcc)
     in (# r, unI (offset chunk0), 0# #)
  where
  !len@(I# len# ) = length chunk0

upcastIntResult :: Result# e Int# -> Result# e Int
upcastIntResult (# e | #) = (# e | #)
upcastIntResult (# | (# a, b, c #) #) = (# | (# I# a, b, c #) #)

char2Word :: Char -> Word
char2Word = fromIntegral . ord

-- | Skip all characters until the character from the is encountered
-- and then consume the matching character as well. Visually,
-- @skipThroughChar 'C'@ advances the cursor like this:
-- 
-- >  A Z B Y C X D W
-- > |->->->->-|
skipThroughChar :: e -> Char -> Parser e s ()
skipThroughChar e !w = uneffectful# $ \c ->
  skipUntilConsumeLoop e w c

skipUntilConsumeLoop ::
     e -- Error message
  -> Char -- byte to match
  -> Bytes -- Chunk
  -> Result# e ()
skipUntilConsumeLoop e !w !c = if length c > 0
  then if indexLatinCharArray (array c) (offset c) /= w
    then skipUntilConsumeLoop e w (Bytes.unsafeDrop 1 c)
    else (# | (# (), unI (offset c + 1), unI (length c - 1) #) #)
  else (# e | #)

-- | Parse exactly four ASCII-encoded characters, interpretting
-- them as the hexadecimal encoding of a 32-bit number. Note that
-- this rejects a sequence such as @5A9@, requiring @05A9@ instead.
-- This is insensitive to case.
hexWord16 :: e -> Parser e s Word16
{-# inline hexWord16 #-}
hexWord16 e = Parser
  (\x s0 -> case runParser (hexWord16# e) x s0 of
    (# s1, r #) -> case r of
      (# err | #) -> (# s1, (# err | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W16# a, b, c #) #) #)
  )

hexWord16# :: e -> Parser e s Word#
{-# noinline hexWord16# #-}
hexWord16# e = uneffectfulWord# $ \chunk -> if length chunk >= 4
  then
    let !w0@(W# n0) = oneHex $ PM.indexByteArray (array chunk) (offset chunk)
        !w1@(W# n1) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 1)
        !w2@(W# n2) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 2)
        !w3@(W# n3) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 3)
     in if | w0 .|. w1 .|. w2 .|. w3 /= maxBound ->
             (# |
                (# (n0 `Exts.timesWord#` 4096##) `Exts.plusWord#`
                   (n1 `Exts.timesWord#` 256##) `Exts.plusWord#`
                   (n2 `Exts.timesWord#` 16##) `Exts.plusWord#`
                   n3
                ,  unI (offset chunk) +# 4#
                ,  unI (length chunk) -# 4# #) #)
           | otherwise -> (# e | #)
  else (# e | #)


-- Returns the maximum machine word if the argument is not
-- the ASCII encoding of a hexadecimal digit.
oneHex :: Word8 -> Word
oneHex w
  | w >= 48 && w < 58 = (fromIntegral w - 48)
  | w >= 65 && w < 71 = (fromIntegral w - 55)
  | w >= 97 && w < 103 = (fromIntegral w - 87)
  | otherwise = maxBound

uneffectfulWord# :: (Bytes -> Result# e Word#) -> Parser e s Word#
uneffectfulWord# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

