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
  , char5
  , char6
  , char7
  , char8
  , char9
  , char10
  , char11
  , char12
    -- ** Try
  , trySatisfy
  , trySatisfyThen
    -- * One Character
  , any
  , opt
  , opt#
    -- * Many Characters
  , takeTrailedBy
    -- * Lookahead
  , peek
  , peek'
    -- * Skip
  , skipDigits
  , skipDigits1
  , skipChar
  , skipChar1
  , skipTrailedBy
  , skipUntil
  , skipWhile
    -- * End of Input
  , endOfInput
  , isEndOfInput
    -- * Numbers
    -- ** Decimal
    -- *** Unsigned
  , decWord
  , decWord8
  , decWord16
  , decWord32
  , decWord64
    -- *** Signed
  , decUnsignedInt
  , decUnsignedInt#
  , decSignedInt
  , decStandardInt
  , decTrailingInt
  , decTrailingInt#
  , decSignedInteger
  , decUnsignedInteger
  , decTrailingInteger
    -- ** Hexadecimal
    -- *** Variable Length
  , hexWord8
  , hexWord16
    -- *** Fixed Length
  , hexFixedWord8
  , hexFixedWord16
  , hexFixedWord32
  , hexFixedWord64
    -- *** Digit
  , hexNibbleLower
  , tryHexNibbleLower
  , hexNibble
  , tryHexNibble
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bits ((.|.))
import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (InternalStep(..),unfailing)
import Data.Bytes.Parser.Internal (Parser(..),ST#,uneffectful,Result#,uneffectful#)
import Data.Bytes.Parser.Internal (Result(..),indexLatinCharArray,upcastUnitSuccess)
import Data.Bytes.Parser.Internal (boxBytes)
import Data.Bytes.Parser (bindFromLiftedToInt,isEndOfInput,endOfInput)
import Data.Bytes.Parser.Unsafe (expose,cursor,unconsume)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Kind (Type)
import GHC.Exts (Int(I#),Char(C#),Word#,Int#,Char#,(+#),(-#),indexCharArray#)
import GHC.Exts (TYPE,RuntimeRep,int2Word#,or#)
import GHC.Exts (ltWord#,gtWord#,notI#)
import GHC.Word (Word(W#),Word8(W8#),Word16(W16#),Word32(W32#),Word64(W64#))

import qualified GHC.Exts as Exts
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM

-- | Runs the predicate on the next character in the input. If the
-- predicate is matched, this consumes the character. Otherwise,
-- the character is not consumed. This returns @False@ if the end
-- of the input has been reached. This never fails.
trySatisfy :: (Char -> Bool) -> Parser e s Bool
trySatisfy f = uneffectful $ \chunk -> case length chunk of
  0 -> Success False (offset chunk) (length chunk)
  _ -> case f (indexLatinCharArray (array chunk) (offset chunk)) of
    True -> Success True (offset chunk + 1) (length chunk - 1)
    False -> Success False (offset chunk) (length chunk)

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
{-# inline char #-}
char e !c = uneffectful $ \chunk -> if length chunk > 0
  then if indexLatinCharArray (array chunk) (offset chunk) == c
    then Success () (offset chunk + 1) (length chunk - 1)
    else Failure e
  else Failure e

-- | Consume the next two characters, failing if they do
-- not match the expected values.
--
-- > char2 e a b === char e a *> char e b
char2 :: e -> Char -> Char -> Parser e s ()
{-# inline char2 #-}
char2 e !c0 !c1 = uneffectful $ \chunk ->
  if | length chunk > 1
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
         -> Success () (offset chunk + 2) (length chunk - 2)
     | otherwise -> Failure e

-- | Consume three characters, failing if they do
-- not match the expected values.
--
-- > char3 e a b c === char e a *> char e b *> char e c
char3 :: e -> Char -> Char -> Char -> Parser e s ()
{-# inline char3 #-}
char3 e !c0 !c1 !c2 = uneffectful $ \chunk ->
  if | length chunk > 2
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
         -> Success () (offset chunk + 3) (length chunk - 3)
     | otherwise -> Failure e

-- | Consume four characters, failing if they do
-- not match the expected values.
--
-- > char4 e a b c d === char e a *> char e b *> char e c *> char e d
char4 :: e -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char4 #-}
char4 e !c0 !c1 !c2 !c3 = uneffectful $ \chunk ->
  if | length chunk > 3
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
         -> Success () (offset chunk + 4) (length chunk - 4)
     | otherwise -> Failure e

-- | Consume five characters, failing if they do
-- not match the expected values.
char5 :: e -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char5 #-}
char5 e !c0 !c1 !c2 !c3 !c4 = uneffectful $ \chunk ->
  if | length chunk > 4
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
         -> Success () (offset chunk + 5) (length chunk - 5)
     | otherwise -> Failure e

-- | Consume six characters, failing if they do
-- not match the expected values.
char6 :: e -> Char -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char6 #-}
char6 e !c0 !c1 !c2 !c3 !c4 !c5 = uneffectful $ \chunk ->
  if | length chunk > 5
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
         -> Success () (offset chunk + 6) (length chunk - 6)
     | otherwise -> Failure e

-- | Consume seven characters, failing if they do
-- not match the expected values.
char7 :: e -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char7 #-}
char7 e !c0 !c1 !c2 !c3 !c4 !c5 !c6 = uneffectful $ \chunk ->
  if | length chunk > 6
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
     , indexLatinCharArray (array chunk) (offset chunk + 6) == c6
         -> Success () (offset chunk + 7) (length chunk - 7)
     | otherwise -> Failure e

-- | Consume eight characters, failing if they do
-- not match the expected values.
char8 :: e -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char8 #-}
char8 e !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 = uneffectful $ \chunk ->
  if | length chunk > 7
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
     , indexLatinCharArray (array chunk) (offset chunk + 6) == c6
     , indexLatinCharArray (array chunk) (offset chunk + 7) == c7
         -> Success () (offset chunk + 8) (length chunk - 8)
     | otherwise -> Failure e

-- | Consume nine characters, failing if they do
-- not match the expected values.
char9 :: e -> Char -> Char -> Char -> Char
  -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char9 #-}
char9 e !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 = uneffectful $ \chunk ->
  if | length chunk > 8
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
     , indexLatinCharArray (array chunk) (offset chunk + 6) == c6
     , indexLatinCharArray (array chunk) (offset chunk + 7) == c7
     , indexLatinCharArray (array chunk) (offset chunk + 8) == c8
         -> Success () (offset chunk + 9) (length chunk - 9)
     | otherwise -> Failure e

-- | Consume ten characters, failing if they do
-- not match the expected values.
char10 :: e -> Char -> Char -> Char -> Char -> Char
  -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char10 #-}
char10 e !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 = uneffectful $ \chunk ->
  if | length chunk > 9
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
     , indexLatinCharArray (array chunk) (offset chunk + 6) == c6
     , indexLatinCharArray (array chunk) (offset chunk + 7) == c7
     , indexLatinCharArray (array chunk) (offset chunk + 8) == c8
     , indexLatinCharArray (array chunk) (offset chunk + 9) == c9
         -> Success () (offset chunk + 10) (length chunk - 10)
     | otherwise -> Failure e

-- | Consume eleven characters, failing if they do
-- not match the expected values.
char11 :: e -> Char -> Char -> Char -> Char -> Char -> Char
  -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char11 #-}
char11 e !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 = uneffectful $ \chunk ->
  if | length chunk > 10
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
     , indexLatinCharArray (array chunk) (offset chunk + 6) == c6
     , indexLatinCharArray (array chunk) (offset chunk + 7) == c7
     , indexLatinCharArray (array chunk) (offset chunk + 8) == c8
     , indexLatinCharArray (array chunk) (offset chunk + 9) == c9
     , indexLatinCharArray (array chunk) (offset chunk + 10) == c10
         -> Success () (offset chunk + 11) (length chunk - 11)
     | otherwise -> Failure e

-- | Consume twelve characters, failing if they do
-- not match the expected values.
char12 :: e -> Char -> Char -> Char -> Char -> Char -> Char
  -> Char -> Char -> Char -> Char -> Char -> Char -> Parser e s ()
{-# inline char12 #-}
char12 e !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 !c11 = uneffectful $ \chunk ->
  if | length chunk > 11
     , indexLatinCharArray (array chunk) (offset chunk) == c0
     , indexLatinCharArray (array chunk) (offset chunk + 1) == c1
     , indexLatinCharArray (array chunk) (offset chunk + 2) == c2
     , indexLatinCharArray (array chunk) (offset chunk + 3) == c3
     , indexLatinCharArray (array chunk) (offset chunk + 4) == c4
     , indexLatinCharArray (array chunk) (offset chunk + 5) == c5
     , indexLatinCharArray (array chunk) (offset chunk + 6) == c6
     , indexLatinCharArray (array chunk) (offset chunk + 7) == c7
     , indexLatinCharArray (array chunk) (offset chunk + 8) == c8
     , indexLatinCharArray (array chunk) (offset chunk + 9) == c9
     , indexLatinCharArray (array chunk) (offset chunk + 10) == c10
     , indexLatinCharArray (array chunk) (offset chunk + 11) == c11
         -> Success () (offset chunk + 12) (length chunk - 12)
     | otherwise -> Failure e

-- | Consumes and returns the next character in the input.
any :: e -> Parser e s Char
{-# inline any #-}
any e = uneffectful $ \chunk -> if length chunk > 0
  then
    let c = indexLatinCharArray (array chunk) (offset chunk)
     in Success c (offset chunk + 1) (length chunk - 1)
  else Failure e

-- | Consume a character from the input or return @Nothing@ if
-- end of the stream has been reached. Since ISO 8859-1 maps every
-- bytes to a character, this parser never fails.
opt :: Parser e s (Maybe Char)
{-# inline opt #-}
opt = uneffectful $ \chunk -> case length chunk of
  0 -> Success Nothing (offset chunk) (length chunk)
  _ -> Success
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
{-# inline skipDigits1 #-}
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
{-# inline skipChar #-}
skipChar !w = uneffectful# $ \c ->
  upcastUnitSuccess (skipLoop w c)

-- | Skip the character any number of times. It must occur
-- at least once or else this will fail.
skipChar1 :: e -> Char -> Parser e s ()
{-# inline skipChar1 #-}
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

-- | Parse a hexadecimal-encoded 8-bit word. If the number is larger
-- than 255, this parser fails. This allows leading zeroes and is
-- insensitive to case. For example, @00A@, @0a@ and @A@ would all
-- be accepted as the same number.
hexWord8 :: e -> Parser e s Word8
hexWord8 e = Parser
  (\chunk0 s0 -> case hexSmallWordStart e 256 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord8Result r #)
  )

-- | Parse a hexadecimal-encoded 16-bit word. If the number is larger
-- than 65535, this parser fails. This allows leading zeroes and is
-- insensitive to case. For example, @0100a@ and @100A@ would both
-- be accepted as the same number.
hexWord16 :: e -> Parser e s Word16
hexWord16 e = Parser
  (\chunk0 s0 -> case hexSmallWordStart e 65536 (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord16Result r #)
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

-- | Parse a decimal-encoded unsigned number. If the number is
-- too large to be represented by a 64-bit word, this fails with
-- the provided error message. This accepts any number of leading
-- zeroes.
decWord64 :: e -> Parser e s Word64
decWord64 e = Parser
  (\chunk0 s0 -> case decWordStart e (boxBytes chunk0) s0 of
    (# s1, r #) -> (# s1, upcastWord64Result r #)
  )

hexSmallWordStart ::
     e -- Error message
  -> Word -- Upper Bound
  -> Bytes -- Chunk
  -> ST# s (Result# e Word# )
hexSmallWordStart e !limit !chunk0 s0 = if length chunk0 > 0
  then case oneHexMaybe (PM.indexByteArray (array chunk0) (offset chunk0)) of
    Nothing -> (# s0, (# e | #) #)
    Just w -> (# s0, hexSmallWordMore e w limit (Bytes.unsafeDrop 1 chunk0) #)
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
decWordMore e !acc !chunk0 = case len of
  0 -> (# | (# unW (fromIntegral acc), unI (offset chunk0), 0# #) #)
  _ ->
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then
            let (overflow,acc') = unsignedPushBase10 acc w
             in if overflow
               then (# e | #)
               else decWordMore e acc' (Bytes.unsafeDrop 1 chunk0)
          else (# | (# unW (fromIntegral acc), unI (offset chunk0), len# #) #)
  where
  !len@(I# len# ) = length chunk0

upcastWordResult :: Result# e Word# -> Result# e Word
{-# inline upcastWordResult #-}
upcastWordResult (# e | #) = (# e | #)
upcastWordResult (# | (# a, b, c #) #) = (# | (# W# a, b, c #) #)

-- This only works on 64-bit platforms.
upcastWord64Result :: Result# e Word# -> Result# e Word64
{-# inline upcastWord64Result #-}
upcastWord64Result (# e | #) = (# e | #)
upcastWord64Result (# | (# a, b, c #) #) = (# | (# W64# a, b, c #) #)

hexSmallWordMore ::
     e -- Error message
  -> Word -- Accumulator
  -> Word -- Upper Bound
  -> Bytes -- Chunk
  -> Result# e Word#
hexSmallWordMore e !acc !limit !chunk0 = if length chunk0 > 0
  then case oneHexMaybe (PM.indexByteArray (array chunk0) (offset chunk0)) of
    Nothing -> (# | (# unW acc, unI (offset chunk0), unI (length chunk0)  #) #)
    Just w -> let w' = acc * 16 + w in
      if w' < limit
        then hexSmallWordMore e w' limit (Bytes.unsafeDrop 1 chunk0)
        else (# e | #)
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
{-# inline upcastWord16Result #-}
upcastWord16Result (# e | #) = (# e | #)
upcastWord16Result (# | (# a, b, c #) #) = (# | (# W16# (Exts.wordToWord16# a), b, c #) #)

-- Precondition: the word is small enough
upcastWord32Result :: Result# e Word# -> Result# e Word32
{-# inline upcastWord32Result #-}
upcastWord32Result (# e | #) = (# e | #)
upcastWord32Result (# | (# a, b, c #) #) = (# | (# W32# (Exts.wordToWord32# a), b, c #) #)

-- Precondition: the word is small enough
upcastWord8Result :: Result# e Word# -> Result# e Word8
{-# inline upcastWord8Result #-}
upcastWord8Result (# e | #) = (# e | #)
upcastWord8Result (# | (# a, b, c #) #) = (# | (# W8# (Exts.wordToWord8# a), b, c #) #)

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
-- digit. This is useful when parsing formats like JSON where integers with
-- leading zeroes are considered invalid. The calling context must
-- consume the first digit before calling this parser. Results are
-- always positive numbers.
decTrailingInt ::
     e -- ^ Error message
  -> Int -- ^ Leading digit, should be between @0@ and @9@.
  -> Parser e s Int
decTrailingInt e (I# w) = Parser
  (\chunk0 s0 -> case runParser (decTrailingInt# e w) chunk0 s0 of
    (# s1, r #) -> (# s1, upcastIntResult r #)
  )

decTrailingInt# ::
     e -- Error message
  -> Int# -- Leading digit, should be between @0@ and @9@.
  -> Parser e s Int#
decTrailingInt# e !w =
  Parser (\chunk0 s0 -> (# s0, decPosIntMore e (W# (int2Word# w)) maxIntAsWord (boxBytes chunk0) #))

maxIntAsWord :: Word
maxIntAsWord = fromIntegral (maxBound :: Int)

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
             then (# s0, decPosIntMore e w maxIntAsWord (boxBytes chunk0) #)
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
             then (# s0, decPosIntMore e w maxIntAsWord (boxBytes chunk0) #)
             else (# s0, (# e | #) #)
    )

-- | Variant of 'decUnsignedInteger' that lets the caller supply a leading
-- digit. This is useful when parsing formats like JSON where integers with
-- leading zeroes are considered invalid. The calling context must
-- consume the first digit before calling this parser. Results are
-- always positive numbers.
decTrailingInteger ::
     Int -- ^ Leading digit, should be between @0@ and @9@.
  -> Parser e s Integer
decTrailingInteger (I# w) =
  Parser (\chunk0 s0 -> (# s0, (# | decIntegerChunks (I# w) 10 0 (boxBytes chunk0) #) #))

-- | Parse a decimal-encoded positive integer of arbitrary
-- size. This rejects input that begins with a plus or minus
-- sign.
decUnsignedInteger :: e -> Parser e s Integer
decUnsignedInteger e = Parser
  (\chunk0 s0 -> decUnsignedIntegerStart e (boxBytes chunk0) s0)

-- | Parse a decimal-encoded integer of arbitrary size.
-- This accepts input that begins with a plus or minus sign.
-- Input without a sign prefix is interpreted as positive.
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
          then (# s0, decPosIntMore e w maxIntAsWord (Bytes.unsafeDrop 1 chunk0) #)
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
          then 
            case decPosIntMore e w (maxIntAsWord + 1) (Bytes.unsafeDrop 1 chunk0) of
             (# | (# x, y, z #) #) -> 
               (# s0, (# | (# (notI# x +# 1# ), y, z #) #) #)
             (# err | #) -> 
               (# s0, (# err | #) #)
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
-- wrapper will still happen. Fails if the accumulator
-- exceeds the upper bound.
decPosIntMore ::
     e -- Error message
  -> Word -- Accumulator, precondition: less than or equal to bound 
  -> Word -- Inclusive Upper Bound, either (2^63 - 1) or 2^63
  -> Bytes -- Chunk
  -> Result# e Int#
decPosIntMore e !acc !upper !chunk0 = if len > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then
            let (overflow,acc') = positivePushBase10 acc w upper
             in if overflow
               then (# e | #)
               else decPosIntMore e acc' upper (Bytes.unsafeDrop 1 chunk0)
          else (# | (# unI (fromIntegral acc), unI (offset chunk0), len# #) #)
  else (# | (# unI (fromIntegral acc), unI (offset chunk0), 0# #) #)
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

-- | Take characters until the specified character is encountered.
-- Consumes the matched character as well. Fails if the character
-- is not present.  Visually, the cursor advancement and resulting
-- @Bytes@ for @takeTrailedBy \'D\'@ look like this:
--
-- >  A B C D E F | input
-- > |->->->-|    | cursor
-- > {-*-*-}      | result bytes
takeTrailedBy :: e -> Char -> Parser e s Bytes
takeTrailedBy e !w = do
  !start <- cursor
  skipTrailedBy e w
  !end <- cursor
  !arr <- expose
  pure (Bytes arr start (end - (start + 1)))

-- | Skip all characters until the terminator is encountered
-- and then consume the matching character as well. Visually,
-- @skipTrailedBy \'C\'@ advances the cursor like this:
-- 
-- >  A Z B Y C X C W
-- > |->->->->-|
--
-- This fails if it reaches the end of input without encountering
-- the character.
skipTrailedBy :: e -> Char -> Parser e s ()
skipTrailedBy e !w = uneffectful# $ \c ->
  skipUntilConsumeLoop e w c

-- | Skip all characters until the terminator is encountered.
-- This does not consume the terminator. Visually, @skipUntil \'C\'@
-- advances the cursor like this:
-- 
-- >  A Z B Y C X C W
-- > |->->->-|
--
-- This succeeds if it reaches the end of the input without
-- encountering the terminator. It never fails.
skipUntil :: Char -> Parser e s ()
skipUntil !w = uneffectful# $ \c -> skipUntilLoop w c

skipUntilLoop ::
     Char -- byte to match
  -> Bytes -- Chunk
  -> Result# e ()
skipUntilLoop !w !c = case length c of
  0 -> (# | (# (), unI (offset c), 0# #) #)
  _ -> if indexLatinCharArray (array c) (offset c) /= w
    then skipUntilLoop w (Bytes.unsafeDrop 1 c)
    else (# | (# (), unI (offset c), unI (length c) #) #)

skipUntilConsumeLoop ::
     e -- Error message
  -> Char -- byte to match
  -> Bytes -- Chunk
  -> Result# e ()
skipUntilConsumeLoop e !w !c = case length c of
  0 -> (# e | #)
  _ -> if indexLatinCharArray (array c) (offset c) /= w
    then skipUntilConsumeLoop e w (Bytes.unsafeDrop 1 c)
    else (# | (# (), unI (offset c + 1), unI (length c - 1) #) #)


-- | Parse exactly eight ASCII-encoded characters, interpreting them as the
-- hexadecimal encoding of a 32-bit number. Note that this rejects a sequence
-- such as @BC5A9@, requiring @000BC5A9@ instead. This is insensitive to case.
hexFixedWord32 :: e -> Parser e s Word32
{-# inline hexFixedWord32 #-}
hexFixedWord32 e = Parser
  (\x s0 -> case runParser (hexFixedWord32# e) x s0 of
    (# s1, r #) -> case r of
      (# err | #) -> (# s1, (# err | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W32# (Exts.wordToWord32# a), b, c #) #) #)
  )

hexFixedWord32# :: e -> Parser e s Word#
{-# noinline hexFixedWord32# #-}
hexFixedWord32# e = uneffectfulWord# $ \chunk -> if length chunk >= 8
  then
    let !w0@(W# n0) = oneHex $ PM.indexByteArray (array chunk) (offset chunk)
        !w1@(W# n1) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 1)
        !w2@(W# n2) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 2)
        !w3@(W# n3) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 3)
        !w4@(W# n4) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 4)
        !w5@(W# n5) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 5)
        !w6@(W# n6) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 6)
        !w7@(W# n7) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 7)
     in if | w0 .|. w1 .|. w2 .|. w3 .|. w4 .|. w5 .|. w6 .|. w7 /= maxBound ->
             (# |
                (# (n0 `Exts.timesWord#` 268435456##) `Exts.plusWord#`
                   (n1 `Exts.timesWord#` 16777216##) `Exts.plusWord#`
                   (n2 `Exts.timesWord#` 1048576##) `Exts.plusWord#`
                   (n3 `Exts.timesWord#` 65536##) `Exts.plusWord#`
                   (n4 `Exts.timesWord#` 4096##) `Exts.plusWord#`
                   (n5 `Exts.timesWord#` 256##) `Exts.plusWord#`
                   (n6 `Exts.timesWord#` 16##) `Exts.plusWord#`
                   n7
                ,  unI (offset chunk) +# 8#
                ,  unI (length chunk) -# 8# #) #)
           | otherwise -> (# e | #)
  else (# e | #)

-- | Parse exactly 16 ASCII-encoded characters, interpreting them as the
-- hexadecimal encoding of a 64-bit number. Note that this rejects a sequence
-- such as @BC5A9@, requiring @00000000000BC5A9@ instead. This is insensitive
-- to case.
hexFixedWord64 :: e -> Parser e s Word64
{-# inline hexFixedWord64 #-}
hexFixedWord64 e = Parser
  (\x s0 -> case runParser (hexFixedWord64# e) x s0 of
    (# s1, r #) -> case r of
      (# err | #) -> (# s1, (# err | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W64# a, b, c #) #) #)
  )

hexFixedWord64# :: e -> Parser e s Word#
{-# noinline hexFixedWord64# #-}
hexFixedWord64# e = uneffectfulWord# $ \chunk -> if length chunk >= 16
  then
    let go !off !len !acc = case len of
          0 -> case acc of
            W# r -> 
              (# | (# r
              ,  unI off
              ,  unI (length chunk) -# 16# #) #)
          _ -> case oneHexMaybe (PM.indexByteArray (array chunk) off) of
            Nothing -> (# e | #)
            Just w -> go (off + 1) (len - 1) ((acc * 16) + w)
     in go (offset chunk) (16 :: Int) (0 :: Word)
  else (# e | #)

-- | Parse exactly four ASCII-encoded characters, interpreting
-- them as the hexadecimal encoding of a 16-bit number. Note that
-- this rejects a sequence such as @5A9@, requiring @05A9@ instead.
-- This is insensitive to case. This is particularly useful when
-- parsing escape sequences in C or JSON, which allow encoding
-- characters in the Basic Multilingual Plane as @\\uhhhh@.
hexFixedWord16 :: e -> Parser e s Word16
{-# inline hexFixedWord16 #-}
hexFixedWord16 e = Parser
  (\x s0 -> case runParser (hexFixedWord16# e) x s0 of
    (# s1, r #) -> case r of
      (# err | #) -> (# s1, (# err | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W16# (Exts.wordToWord16# a), b, c #) #) #)
  )

hexFixedWord16# :: e -> Parser e s Word#
{-# noinline hexFixedWord16# #-}
hexFixedWord16# e = uneffectfulWord# $ \chunk -> if length chunk >= 4
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

-- | Parse exactly two ASCII-encoded characters, interpretting
-- them as the hexadecimal encoding of a 8-bit number. Note that
-- this rejects a sequence such as @A@, requiring @0A@ instead.
-- This is insensitive to case.
hexFixedWord8 :: e -> Parser e s Word8
{-# inline hexFixedWord8 #-}
hexFixedWord8 e = Parser
  (\x s0 -> case runParser (hexFixedWord8# e) x s0 of
    (# s1, r #) -> case r of
      (# err | #) -> (# s1, (# err | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W8# (Exts.wordToWord8# a), b, c #) #) #)
  )

hexFixedWord8# :: e -> Parser e s Word#
{-# noinline hexFixedWord8# #-}
hexFixedWord8# e = uneffectfulWord# $ \chunk -> if length chunk >= 2
  then
    let !w0@(W# n0) = oneHex $ PM.indexByteArray (array chunk) (offset chunk)
        !w1@(W# n1) = oneHex $ PM.indexByteArray (array chunk) (offset chunk + 1)
     in if | w0 .|. w1 /= maxBound ->
             (# |
                (# (n0 `Exts.timesWord#` 16##) `Exts.plusWord#`
                   n1
                ,  unI (offset chunk) +# 2#
                ,  unI (length chunk) -# 2# #) #)
           | otherwise -> (# e | #)
  else (# e | #)

-- | Consume a single character that is the lowercase hexadecimal
-- encoding of a 4-bit word. Fails if the character is not in the class
-- @[a-f0-9]@.
hexNibbleLower :: e -> Parser e s Word
hexNibbleLower e = uneffectful $ \chunk -> case length chunk of
  0 -> Failure e
  _ ->
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8 in
    if | w >= 48 && w < 58 -> Success (fromIntegral w - 48) (offset chunk + 1) (length chunk - 1)
       | w >= 97 && w < 103 -> Success (fromIntegral w - 87) (offset chunk + 1) (length chunk - 1)
       | otherwise -> Failure e

-- | Consume a single character that is the case-insensitive hexadecimal
-- encoding of a 4-bit word. Fails if the character is not in the class
-- @[a-fA-F0-9]@.
hexNibble :: e -> Parser e s Word
hexNibble e = uneffectful $ \chunk -> case length chunk of
  0 -> Failure e
  _ ->
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8 in
    if | w >= 48 && w < 58 -> Success (fromIntegral w - 48) (offset chunk + 1) (length chunk - 1)
       | w >= 65 && w < 71 -> Success (fromIntegral w - 55) (offset chunk + 1) (length chunk - 1)
       | w >= 97 && w < 103 -> Success (fromIntegral w - 87) (offset chunk + 1) (length chunk - 1)
       | otherwise -> Failure e

-- | Consume a single character that is the lowercase hexadecimal
-- encoding of a 4-bit word. Returns @Nothing@ without consuming
-- the character if it is not in the class @[a-f0-9]@. The parser
-- never fails.
tryHexNibbleLower :: Parser e s (Maybe Word)
tryHexNibbleLower = unfailing $ \chunk -> case length chunk of
  0 -> InternalStep Nothing (offset chunk) (length chunk)
  _ ->
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8 in
    if | w >= 48 && w < 58 -> InternalStep (Just (fromIntegral w - 48)) (offset chunk + 1) (length chunk - 1)
       | w >= 97 && w < 103 -> InternalStep (Just (fromIntegral w - 87)) (offset chunk + 1) (length chunk - 1)
       | otherwise -> InternalStep Nothing (offset chunk) (length chunk)

-- | Consume a single character that is the case-insensitive hexadecimal
-- encoding of a 4-bit word. Returns @Nothing@ without consuming
-- the character if it is not in the class @[a-fA-F0-9]@. This parser
-- never fails.
tryHexNibble :: Parser e s (Maybe Word)
tryHexNibble = unfailing $ \chunk -> case length chunk of
  0 -> InternalStep Nothing (offset chunk) (length chunk)
  _ ->
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8 in
    if | w >= 48 && w < 58 -> InternalStep (Just (fromIntegral w - 48)) (offset chunk + 1) (length chunk - 1)
       | w >= 65 && w < 71 -> InternalStep (Just (fromIntegral w - 55)) (offset chunk + 1) (length chunk - 1)
       | w >= 97 && w < 103 -> InternalStep (Just (fromIntegral w - 87)) (offset chunk + 1) (length chunk - 1)
       | otherwise -> InternalStep Nothing (offset chunk) (length chunk)

-- Returns the maximum machine word if the argument is not
-- the ASCII encoding of a hexadecimal digit.
oneHex :: Word8 -> Word
{-# inline oneHex #-}
oneHex w
  | w >= 48 && w < 58 = (fromIntegral w - 48)
  | w >= 65 && w < 71 = (fromIntegral w - 55)
  | w >= 97 && w < 103 = (fromIntegral w - 87)
  | otherwise = maxBound

oneHexMaybe :: Word8 -> Maybe Word
{-# inline oneHexMaybe #-}
oneHexMaybe w
  | w >= 48 && w < 58 = Just (fromIntegral w - 48)
  | w >= 65 && w < 71 = Just (fromIntegral w - 55)
  | w >= 97 && w < 103 = Just (fromIntegral w - 87)
  | otherwise = Nothing

uneffectfulWord# :: (Bytes -> Result# e Word#) -> Parser e s Word#
{-# inline uneffectfulWord# #-}
uneffectfulWord# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

-- Precondition: the arguments are non-negative. Boolean is
-- true when overflow happens. Performs: a * 10 + b
-- Postcondition: when overflow is false, the resulting
-- word is less than or equal to the upper bound
positivePushBase10 :: Word -> Word -> Word -> (Bool,Word)
{-# inline positivePushBase10 #-}
positivePushBase10 (W# a) (W# b) (W# upper) = 
  let !(# ca, r0 #) = Exts.timesWord2# a 10##
      !r1 = Exts.plusWord# r0 b
      !cb = int2Word# (gtWord# r1 upper)
      !cc = int2Word# (ltWord# r1 0##)
      !c = ca `or#` cb `or#` cc
   in (case c of { 0## -> False; _ -> True }, W# r1)

unsignedPushBase10 :: Word -> Word -> (Bool,Word)
{-# inline unsignedPushBase10 #-}
unsignedPushBase10 (W# a) (W# b) = 
  let !(# ca, r0 #) = Exts.timesWord2# a 10##
      !r1 = Exts.plusWord# r0 b
      !cb = int2Word# (ltWord# r1 r0)
      !c = ca `or#` cb
   in (case c of { 0## -> False; _ -> True }, W# r1)

-- | Skip while the predicate is matched. This is always inlined.
skipWhile :: (Char -> Bool) -> Parser e s ()
{-# inline skipWhile #-}
skipWhile f = go where
  go = isEndOfInput >>= \case
    True -> pure ()
    False -> do
      w <- anyUnsafe
      if f w
        then go
        else unconsume 1

-- Interpret the next byte as an Latin1-encoded character.
-- Does not check to see if any characters are left. This
-- is not exported.
anyUnsafe :: Parser e s Char
{-# inline anyUnsafe #-}
anyUnsafe = uneffectful $ \chunk ->
  let w = indexCharArray (array chunk) (offset chunk) :: Char
   in Success w (offset chunk + 1) (length chunk - 1)

-- Reads one byte and interprets it as Latin1-encoded character.
indexCharArray :: PM.ByteArray -> Int -> Char
{-# inline indexCharArray #-}
indexCharArray (PM.ByteArray x) (I# i) = C# (indexCharArray# x i)

-- | Match any character, to perform lookahead. Returns 'Nothing' if
--   end of input has been reached. Does not consume any input.
--
--   /Note/: Because this parser does not fail, do not use it
--   with combinators such as 'many', because such as 'many',
--   because such parsers loop until a failure occurs. Careless
--   use will thus result in an infinite loop.
peek :: Parser e s (Maybe Char)
{-# inline peek #-}
peek = uneffectful $ \(Bytes arr off len) ->
  let v = if len > 0
        then Just (indexCharArray arr off)
        else Nothing
  in Success v off len

-- | Match any byte, to perform lookahead. Does not consume any
--   input, but will fail if end of input has been reached.
peek' :: e -> Parser e s Char
{-# inline peek' #-}
peek' e = uneffectful $ \(Bytes arr off len) -> if len > 0
  then Success (indexCharArray arr off) off len
  else Failure e
