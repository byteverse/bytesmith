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

module Data.Bytes.Parser
  ( -- * Types
    Parser(..)
  , Result(..)
    -- * Run Parsers
  , parseByteArray
  , parseBytes
  , parseBytesST
    -- * Build Parsers
  , fail
  , peekAnyAscii
  , ascii
  , ascii3
  , ascii4
  , any
  , anyAscii
  , anyAscii#
  , anyUtf8#
  , anyAsciiOpt
  , decWord
  , decWord8
  , decWord16
  , decWord32
  , decUnsignedInt
  , decUnsignedInt#
  , decSignedInt
  , decStandardInt
  , decTrailingInt
  , hexWord16
  , decPositiveInteger
  , endOfInput
  , isEndOfInput
  , skipUntilAsciiConsume
  , takeWhile
  , skipWhile
  , skipAscii
  , skipAscii1
  , skipAlphaAscii
  , skipAlphaAscii1
  , skipDigitsAscii
  , skipDigitsAscii1
    -- * Lift Effects
  , effect
    -- * Expose Internals
    -- | Everything in this section is unsafe and can lead to
    -- nondeterministic output or segfaults if used incorrectly.
  , cursor
  , expose
  , unconsume
  , jump
    -- * Cut down on boxing
  , unboxWord32
  , boxWord32
  , unboxIntPair
  , boxIntPair
    -- * Specialized Bind
    -- $bind
  , bindChar
  , bindToIntPair
  , bindIntToIntPair
  , bindCharToIntPair
    -- * Specialized Pure
  , pureIntPair
    -- * Specialized Fail
  , failIntPair
    -- * Alternative
  , orElse
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Control.Applicative (Alternative)
import Data.Char (ord)
import Data.Bits ((.&.),(.|.),unsafeShiftL,xor)
import Data.Kind (Type)
import GHC.ST (ST(..),runST)
import GHC.Exts (Word(W#),Word#,TYPE,State#,Int#,ByteArray#)
import GHC.Exts (Int(I#),Char(C#),chr#,RuntimeRep)
import GHC.Exts (Char#,(+#),(-#),(<#),(>#),word2Int#)
import GHC.Exts (indexCharArray#,indexWord8Array#,ord#)
import GHC.Exts (timesWord#,plusWord#)
import GHC.Word (Word16(W16#),Word8(W8#),Word32(W32#))
import Data.Bytes.Types (Bytes(..))
import qualified Data.Bytes as B
import Data.Primitive (ByteArray(..))

import qualified Control.Applicative
import qualified Data.Primitive as PM
import qualified Control.Monad

type Bytes# = (# ByteArray#, Int#, Int# #)
type ST# s (a :: TYPE r) = State# s -> (# State# s, a #)
type Result# e (a :: TYPE r) =
  (# e
  | (# a, Int#, Int# #) #) -- ints are offset and length

-- | A non-resumable parser.
newtype Parser :: forall (r :: RuntimeRep). Type -> Type -> TYPE r -> Type where
  Parser :: forall (r :: RuntimeRep) (e :: Type) (s :: Type) (a :: TYPE r).
    { runParser :: (# ByteArray#, Int#, Int# #) -> ST# s (Result# e a) } -> Parser e s a

-- | The result of running a parser.
data Result e a
  = Failure e
    -- ^ An error message indicating what went wrong.
  | Success !a !Int
    -- ^ The parsed value and the number of bytes
    -- remaining in parsed slice.
  deriving (Eq,Show)

-- The result of running a parser. Used internally.
data InternalResult e a
  = InternalFailure e
    -- An error message indicating what went wrong.
  | InternalSuccess !a !Int !Int
    -- The parsed value, the offset after the last consumed byte, and the
    -- number of bytes remaining in parsed slice.

-- | Parse a slice of a byte array. This can succeed even if the
-- entire slice was not consumed by the parser.
parseBytes :: forall e a. (forall s. Parser e s a) -> Bytes -> Result e a
parseBytes p !b = runST action
  where
  action :: forall s. ST s (Result e a)
  action = case p @s of
    Parser f -> ST
      (\s0 -> case f (unboxBytes b) s0 of
        (# s1, r #) -> (# s1, boxPublicResult r #)
      )

-- | Variant of 'parseBytes' that accepts an unsliced 'ByteArray'.
parseByteArray :: (forall s. Parser e s a) -> ByteArray -> Result e a
parseByteArray p b =
  parseBytes p (Bytes b 0 (PM.sizeofByteArray b))

-- | Variant of 'parseBytes' that allows the parser to be run
-- as part of an existing effectful context.
parseBytesST :: Parser e s a -> Bytes -> ST s (Result e a)
parseBytesST (Parser f) !b = ST
  (\s0 -> case f (unboxBytes b) s0 of
    (# s1, r #) -> (# s1, boxPublicResult r #)
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

pureParser :: a -> Parser e s a
pureParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

upcastUnitSuccess :: (# Int#, Int# #) -> Result# e ()
upcastUnitSuccess (# b, c #) = (# | (# (), b, c #) #)

upcastWordResult :: Result# e Word# -> Result# e Word
upcastWordResult (# e | #) = (# e | #)
upcastWordResult (# | (# a, b, c #) #) = (# | (# W# a, b, c #) #)

upcastIntResult :: Result# e Int# -> Result# e Int
upcastIntResult (# e | #) = (# e | #)
upcastIntResult (# | (# a, b, c #) #) = (# | (# I# a, b, c #) #)

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

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- | Get the current offset into the chunk. Using this makes
-- it possible to observe the internal difference between 'Bytes'
-- that refer to equivalent slices. Be careful.
cursor :: Parser e s Int
cursor = uneffectful $ \chunk ->
  InternalSuccess (offset chunk) (offset chunk) (length chunk)

-- | Return the byte array being parsed. This includes bytes
-- that preceed the current offset and may include bytes that
-- go beyond the length. This is somewhat dangerous, so only
-- use this is you know what you're doing.
expose :: Parser e s ByteArray
expose = uneffectful $ \chunk ->
  InternalSuccess (array chunk) (offset chunk) (length chunk)

-- | Move the cursor back by @n@ bytes. Precondition: you
-- must have previously consumed at least @n@ bytes.
unconsume :: Int -> Parser e s ()
unconsume n = uneffectful $ \chunk ->
  InternalSuccess () (offset chunk - n) (length chunk + n)

-- | Set the position to the given index. Precondition: the index
-- must be valid. It should be the result of an earlier call to
-- 'cursor'.
jump :: Int -> Parser e s ()
jump ix = uneffectful $ \chunk ->
  InternalSuccess () ix (length chunk + (offset chunk - ix))

uneffectful :: (Bytes -> InternalResult e a) -> Parser e s a
{-# inline uneffectful #-}
uneffectful f = Parser
  ( \b s0 -> (# s0, unboxResult (f (boxBytes b)) #) )

uneffectful# :: (Bytes -> Result# e a) -> Parser e s a
uneffectful# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

uneffectfulWord# :: (Bytes -> Result# e Word#) -> Parser e s Word#
uneffectfulWord# f = Parser
  ( \b s0 -> (# s0, (f (boxBytes b)) #) )

-- | Lift an effectful computation into a parser.
effect :: ST s a -> Parser e s a
effect (ST f) = Parser
  ( \(# _, off, len #) s0 -> case f s0 of
    (# s1, a #) -> (# s1, (# | (# a, off, len #) #) #)
  )

-- | Only valid for characters with a Unicode code point lower
-- than 128. This consumes a single byte, decoding it as an ASCII
-- character.
ascii :: e -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
ascii e !c = uneffectful $ \chunk -> if length chunk > 0
  then if PM.indexByteArray (array chunk) (offset chunk) == c2w c
    then InternalSuccess () (offset chunk + 1) (length chunk - 1)
    else InternalFailure e
  else InternalFailure e

-- | Parse three bytes in succession.
ascii3 :: e -> Char -> Char -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
ascii3 e !c0 !c1 !c2 = uneffectful $ \chunk ->
  if | length chunk > 2
     , PM.indexByteArray (array chunk) (offset chunk) == c2w c0
     , PM.indexByteArray (array chunk) (offset chunk + 1) == c2w c1
     , PM.indexByteArray (array chunk) (offset chunk + 2) == c2w c2
         -> InternalSuccess () (offset chunk + 3) (length chunk - 3)
     | otherwise -> InternalFailure e

-- | Parse four bytes in succession.
ascii4 :: e -> Char -> Char -> Char -> Char -> Parser e s ()
-- GHC should decide to inline this after optimization.
ascii4 e !c0 !c1 !c2 !c3 = uneffectful $ \chunk ->
  if | length chunk > 3
     , PM.indexByteArray (array chunk) (offset chunk) == c2w c0
     , PM.indexByteArray (array chunk) (offset chunk + 1) == c2w c1
     , PM.indexByteArray (array chunk) (offset chunk + 2) == c2w c2
     , PM.indexByteArray (array chunk) (offset chunk + 3) == c2w c3
         -> InternalSuccess () (offset chunk + 4) (length chunk - 4)
     | otherwise -> InternalFailure e

-- | Fail with the provided error message.
fail ::
     e -- ^ Error message
  -> Parser e s a
fail e = uneffectful $ \_ -> InternalFailure e

-- | Interpret the next byte as an ASCII-encoded character.
-- Fails if the byte corresponds to a number above 127.
peekAnyAscii :: e -> Parser e s Char
peekAnyAscii e = uneffectful $ \chunk -> if length chunk > 0
  then
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8
     in if w < 128
          then InternalSuccess
                 (C# (chr# (unI (fromIntegral w))))
                 (offset chunk)
                 (length chunk)
          else InternalFailure e
  else InternalFailure e

-- | Consumes and returns the next byte in the input.
-- Fails if no characters are left.
any :: e -> Parser e s Word8
{-# inline any #-}
any e = uneffectful $ \chunk -> if length chunk > 0
  then
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8
     in InternalSuccess w (offset chunk + 1) (length chunk - 1)
  else InternalFailure e

-- Interpret the next byte as an ASCII-encoded character.
-- Does not check to see if any characters are left. This
-- is not exported.
anyUnsafe :: Parser e s Word8
{-# inline anyUnsafe #-}
anyUnsafe = uneffectful $ \chunk ->
  let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8
   in InternalSuccess w (offset chunk + 1) (length chunk - 1)

-- | Interpret the next byte as an ASCII-encoded character.
-- Fails if the byte corresponds to a number above 127.
anyAscii :: e -> Parser e s Char
{-# inline anyAscii #-}
anyAscii e = uneffectful $ \chunk -> if length chunk > 0
  then
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8
     in if w < 128
          then InternalSuccess
                 (C# (chr# (unI (fromIntegral w))))
                 (offset chunk + 1)
                 (length chunk - 1)
          else InternalFailure e
  else InternalFailure e

-- | Interpret the next byte as an ASCII-encoded character.
-- Fails if the byte corresponds to a number above 127.
anyAscii# :: e -> Parser e s Char#
{-# inline anyAscii# #-}
anyAscii# e = Parser
  (\(# arr, off, len #) s0 -> case len of
    0# -> (# s0, (# e | #) #)
    _ ->
      let !w = indexCharArray# arr off
       in case ord# w <# 128# of
            1# -> (# s0, (# | (# w, off +# 1#, len -# 1# #) #) #)
            _ -> (# s0, (# e | #) #)
  )

-- | Interpret the next one to four bytes as a UTF-8-encoded character.
-- Fails if the decoded codepoint is in the range U+D800 through U+DFFF.
anyUtf8# :: e -> Parser e s Char#
{-# noinline anyUtf8# #-}
anyUtf8# e = Parser
  (\(# arr, off, len #) s0 -> case len ># 0# of
    1# ->
      let !w0 = indexWord8Array# arr off
       in if | oneByteChar (W8# w0) -> 
                 (# s0, (# | (# chr# (word2Int# w0), off +# 1#, len -# 1# #) #) #)
             | twoByteChar (W8# w0) ->
                 if | I# len > 1
                    , w1 <- indexWord8Array# arr (off +# 1#)
                    , followingByte (W8# w1)
                    , C# c <- codepointFromTwoBytes (W8# w0) (W8# w1)
                      -> (# s0, (# | (# c, off +# 2#, len -# 2# #) #) #)
                    | otherwise -> (# s0, (# e | #) #)
             | threeByteChar (W8# w0) ->
                 if | I# len > 2
                    , w1 <- indexWord8Array# arr (off +# 1# )
                    , w2 <- indexWord8Array# arr (off +# 2# )
                    , followingByte (W8# w1)
                    , !c@(C# c#) <- codepointFromThreeBytes (W8# w0) (W8# w1) (W8# w2)
                    , c < '\xD800' || c > '\xDFFF'
                      -> (# s0, (# | (# c#, off +# 3#, len -# 3# #) #) #)
                    | otherwise -> (# s0, (# e | #) #)
             | fourByteChar (W8# w0) ->
                 if | I# len > 3
                    , w1 <- indexWord8Array# arr (off +# 1# )
                    , w2 <- indexWord8Array# arr (off +# 2# )
                    , w3 <- indexWord8Array# arr (off +# 3# )
                    , followingByte (W8# w1)
                    , !(C# c#) <- codepointFromFourBytes (W8# w0) (W8# w1) (W8# w2) (W8# w3)
                      -> (# s0, (# | (# c#, off +# 4#, len -# 4# #) #) #)
                    | otherwise -> (# s0, (# e | #) #)
             | otherwise -> (# s0, (# e | #) #)
    _ -> (# s0, (# e | #) #)
  )

-- | Interpret the next byte as an ASCII-encoded character.
-- Fails if the byte corresponds to a number above 127. Returns
-- nothing if the end of the input has been reached.
anyAsciiOpt :: e -> Parser e s (Maybe Char)
{-# inline anyAsciiOpt #-}
anyAsciiOpt e = uneffectful $ \chunk -> if length chunk > 0
  then
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8
     in if w < 128
          then InternalSuccess
                 (Just (C# (chr# (unI (fromIntegral w)))))
                 (offset chunk + 1)
                 (length chunk - 1)
          else InternalFailure e
  else InternalSuccess Nothing (offset chunk) (length chunk)

-- | Take while the predicate is matched. This is always inlined.
takeWhile :: (Word8 -> Bool) -> Parser e s Bytes
{-# inline takeWhile #-}
takeWhile f = uneffectful $ \chunk -> case B.takeWhile f chunk of
  bytes -> InternalSuccess bytes (offset chunk + length bytes) (length chunk - length bytes)

-- | Skip while the predicate is matched. This is always inlined.
skipWhile :: (Word8 -> Bool) -> Parser e s ()
{-# inline skipWhile #-}
skipWhile f = go where
  go = isEndOfInput >>= \case
    True -> pure ()
    False -> do
      w <- anyUnsafe
      if f w
        then go
        else unconsume 1

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
                (# (n0 `timesWord#` 4096##) `plusWord#`
                   (n1 `timesWord#` 256##) `plusWord#`
                   (n2 `timesWord#` 16##) `plusWord#`
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

-- | Skip bytes until the character from the ASCII plane is encountered.
-- This does not ensure that the skipped bytes were ASCII-encoded
-- characters.
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
  then InternalSuccess () (offset chunk) 0
  else InternalFailure e

-- | Returns true if there are no more bytes in the input. Returns
-- false otherwise. Always succeeds.
isEndOfInput :: Parser e s Bool
-- GHC should decide to inline this after optimization.
isEndOfInput = uneffectful $ \chunk ->
  InternalSuccess (length chunk == 0) (offset chunk) (length chunk)

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

-- | Parse a decimal-encoded number. If the number is too large to be
-- represented by a machine integer, this fails with the provided
-- error message. This rejects input with that is preceeded by plus
-- or minus. Consequently, it does not parse negative numbers. Use
-- 'decStandardInt' or 'decSignedInt' for that purpose. On a 64-bit
-- platform 'decWord' will successfully parse 9223372036854775808
-- (i.e. @2 ^ 63@), but 'decUnsignedInt' will fail. This allows
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
decSignedInt# e = any e `bindToIntHash` \c -> case c of
  43 -> Parser -- plus sign
    (\chunk0 s0 -> decPosIntStart e (boxBytes chunk0) s0)
  45 -> Parser -- minus sign
    (\chunk0 s0 -> decNegIntStart e (boxBytes chunk0) s0)
  _ -> Parser -- no sign, there should be a digit here 
    (\chunk0 s0 ->
      let !w = fromIntegral @Word8 @Word c - 48
        in if w < 10
             then (# s0, decPosIntMore e (fromIntegral @Word @Int w) (boxBytes chunk0) #)
             else (# s0, (# e | #) #)
    )

-- This is the same as decSignedInt except that we disallow
-- a leading plus sign.
decStandardInt# :: e -> Parser e s Int#
{-# noinline decStandardInt# #-}
decStandardInt# e = any e `bindToIntHash` \c -> case c of
  45 -> Parser -- minus sign
    (\chunk0 s0 -> decNegIntStart e (boxBytes chunk0) s0)
  _ -> Parser -- no sign, there should be a digit here 
    (\chunk0 s0 ->
      let !w = fromIntegral @Word8 @Word c - 48
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
decPositiveInteger :: e -> Parser e s Integer
decPositiveInteger e = Parser
  (\chunk0 s0 -> decPositiveIntegerStart e (boxBytes chunk0) s0)

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

decPosIntStart ::
     e -- Error message
  -> Bytes -- Chunk
  -> ST# s (Result# e Int# )
decPosIntStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then (# s0, decPosIntMore e (fromIntegral @Word @Int w) (advance 1 chunk0) #)
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
          then (# s0, decNegIntMore e (negate (fromIntegral @Word @Int w)) (advance 1 chunk0) #)
          else (# s0, (# e | #) #)
  else (# s0, (# e | #) #)

-- No limit on length for integers.
decPositiveIntegerStart ::
     e
  -> Bytes
  -> ST# s (Result# e Integer)
decPositiveIntegerStart e !chunk0 s0 = if length chunk0 > 0
  then
    let !w = (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < (10 :: Word8)
          then
            let !r = decPosIntegerChunks
                  (fromIntegral @Word8 @Int w)
                  10
                  0
                  (advance 1 chunk0)
             in (# s0, (# | r #) #)
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
        !acc' = acc * 10 + w
     in if w < 10 && acc' >= acc
          then decWordMore e acc' (advance 1 chunk0)
          else (# | (# unW acc, unI (offset chunk0), unI (length chunk0)  #) #)
  else (# | (# unW acc, unI (offset chunk0), 0# #) #)

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
     in if w < 10 && acc' <= acc
          then decNegIntMore e acc' (advance 1 chunk0)
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
            then decPosIntMore e acc' (advance 1 chunk0)
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
decPosIntegerChunks ::
     Int -- Chunk accumulator (e.g. 236)
  -> Int -- Chunk base-ten bound (e.g. 1000)
  -> Integer -- Accumulator
  -> Bytes -- Chunk
  -> (# Integer, Int#, Int# #)
decPosIntegerChunks !nAcc !eAcc acc !chunk0 = if len > 0
  then
    let !w = fromIntegral @Word8 @Word
          (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then let !eAcc' = eAcc * 10 in
            if eAcc' >= eAcc
              then decPosIntegerChunks
                (nAcc * 10 + fromIntegral @Word @Int w)
                eAcc'
                acc
                (advance 1 chunk0)
              else
                -- In this case, notice that we deliberately
                -- unconsume the digit that would have caused
                -- an overflow.
                let !r = (acc * fromIntegral @Int @Integer eAcc)
                       + (fromIntegral @Int @Integer nAcc)
                 in decPosIntegerChunks 0 1 r chunk0
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

decIntegerMore ::
     e -- Error message
  -> Integer -- Accumulator
  -> Bytes -- Chunk
  -> Result# e Integer
decIntegerMore e !acc !chunk0 = if length chunk0 > 0
  then
    let w :: Word8
        !w = (PM.indexByteArray (array chunk0) (offset chunk0)) - 48
     in if w < 10
          then
            let w' = acc * 10 + fromIntegral w
             in decIntegerMore e w' (advance 1 chunk0)
          else (# | (# acc, unI (offset chunk0), unI (length chunk0) #) #)
  else (# | (# acc, unI (offset chunk0), 0# #) #)

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

unboxResult :: InternalResult e a -> Result# e a
unboxResult (InternalSuccess a (I# b) (I# c)) = (# | (# a, b, c #) #)
unboxResult (InternalFailure e) = (# e | #)

boxPublicResult :: Result# e a -> Result e a
boxPublicResult (# | (# a, _, c #) #) = Success a (I# c)
boxPublicResult (# e | #) = Failure e

-- | Convert a 'Word32' parser to a 'Word#' parser.
unboxWord32 :: Parser e s Word32 -> Parser e s Word#
unboxWord32 (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# W32# a, b, c #) #) -> (# s1, (# | (# a, b, c #) #) #)
  )

-- | Convert a @(Int,Int)@ parser to a @(# Int#, Int# #)@ parser.
unboxIntPair :: Parser e s (Int,Int) -> Parser e s (# Int#, Int# #)
unboxIntPair (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# (I# y, I# z), b, c #) #) -> (# s1, (# | (# (# y, z #), b, c #) #) #)
  )

-- | Convert a 'Word#' parser to a 'Word32' parser. Precondition:
-- the argument parser only returns words less than 4294967296.
boxWord32 :: Parser e s Word# -> Parser e s Word32
boxWord32 (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# W32# a, b, c #) #) #)
  )

-- | Convert a @(# Int#, Int# #)@ parser to a @(Int,Int)@ parser.
boxIntPair :: Parser e s (# Int#, Int# #) -> Parser e s (Int,Int)
boxIntPair (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# (# y, z #), b, c #) #) -> (# s1, (# | (# (I# y, I# z), b, c #) #) #)
  )


-- | There is a law-abiding instance of 'Alternative' for 'Parser'.
-- However, it is not terribly useful since error messages seldom
-- have a 'Monoid' instance. This function is a variant of @\<|\>@
-- that is right-biased in its treatment of error messages.
-- Consequently, @orElse@ lacks an identity.
-- See <https://github.com/bos/attoparsec/issues/122 attoparsec issue #122>
-- for more discussion of this topic.
infixl 3 `orElse`
orElse :: Parser x s a -> Parser e s a -> Parser e s a
{-# inline orElse #-}
orElse (Parser f) (Parser g) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# _ | #) -> g x s1
      (# | r #) -> (# s1, (# | r #) #)
  )

codepointFromFourBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Char
codepointFromFourBytes w1 w2 w3 w4 = C#
  ( chr#
    ( unI $ fromIntegral
      ( unsafeShiftL (word8ToWord w1 .&. 0b00001111) 18 .|. 
        unsafeShiftL (word8ToWord w2 .&. 0b00111111) 12 .|. 
        unsafeShiftL (word8ToWord w3 .&. 0b00111111) 6 .|. 
        (word8ToWord w4 .&. 0b00111111)
      )
    )
  )

codepointFromThreeBytes :: Word8 -> Word8 -> Word8 -> Char
codepointFromThreeBytes w1 w2 w3 = C#
  ( chr#
    ( unI $ fromIntegral
      ( unsafeShiftL (word8ToWord w1 .&. 0b00001111) 12 .|. 
        unsafeShiftL (word8ToWord w2 .&. 0b00111111) 6 .|. 
        (word8ToWord w3 .&. 0b00111111)
      )
    )
  )

codepointFromTwoBytes :: Word8 -> Word8 -> Char
codepointFromTwoBytes w1 w2 = C#
  ( chr#
    ( unI $ fromIntegral @Word @Int
      ( unsafeShiftL (word8ToWord w1 .&. 0b00011111) 6 .|. 
        (word8ToWord w2 .&. 0b00111111)
      )
    )
  )

oneByteChar :: Word8 -> Bool
oneByteChar !w = w .&. 0b10000000 == 0

twoByteChar :: Word8 -> Bool
twoByteChar !w = w .&. 0b11100000 == 0b11000000

threeByteChar :: Word8 -> Bool
threeByteChar !w = w .&. 0b11110000 == 0b11100000

fourByteChar :: Word8 -> Bool
fourByteChar !w = w .&. 0b11111000 == 0b11110000

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

followingByte :: Word8 -> Bool
followingByte !w = xor w 0b01000000 .&. 0b11000000 == 0b11000000

{- $bind
Sometimes, GHC ends up building join points in a way that
boxes arguments unnecessarily. In this situation, special variants
of monadic @>>=@ can be helpful. If @C#@, @I#@, etc. never
get used in your original source code, GHC will not introduce them.
-}

-- | Specialization of monadic bind for parsers that return 'Char#'.
bindChar :: Parser s e Char# -> (Char# -> Parser s e a) -> Parser s e a
{-# inline bindChar #-}
bindChar (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindCharToIntPair :: Parser s e Char# -> (Char# -> Parser s e (# Int#, Int# #)) -> Parser s e (# Int#, Int# #)
{-# inline bindCharToIntPair #-}
bindCharToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindToIntHash :: Parser s e a -> (a -> Parser s e Int#) -> Parser s e Int#
{-# inline bindToIntHash #-}
bindToIntHash (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindToIntPair :: Parser s e a -> (a -> Parser s e (# Int#, Int# #)) -> Parser s e (# Int#, Int# #)
{-# inline bindToIntPair #-}
bindToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindIntToIntPair :: Parser s e Int# -> (Int# -> Parser s e (# Int#, Int# #)) -> Parser s e (# Int#, Int# #)
{-# inline bindIntToIntPair #-}
bindIntToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

pureIntPair ::
     (# Int#, Int# #)
  -> Parser s e (# Int#, Int# #)
{-# inline pureIntPair #-}
pureIntPair a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

failIntPair :: e -> Parser e s (# Int#, Int# #)
{-# inline failIntPair #-}
failIntPair e = Parser
  (\(# _, _, _ #) s -> (# s, (# e | #) #))
