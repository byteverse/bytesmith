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
    -- * One Byte
  , any
    -- * Many Bytes
  , takeWhile
    -- * Skip
  , skipWhile
    -- * Match
  , byteArray
  , bytes
    -- * End of Input
  , endOfInput
  , isEndOfInput
  , remaining
    -- * Control Flow
  , fail
  , orElse
    -- * Subparsing
  , measure
    -- * Lift Effects
  , effect
    -- * Box Result
  , boxWord32
  , boxIntPair
    -- * Unbox Result
  , unboxWord32
  , unboxIntPair
    -- * Specialized Bind
    -- | Sometimes, GHC ends up building join points in a way that
    -- boxes arguments unnecessarily. In this situation, special variants
    -- of monadic @>>=@ can be helpful. If @C#@, @I#@, etc. never
    -- get used in your original source code, GHC will not introduce them.
  , bindFromCharToLifted
  , bindFromLiftedToIntPair
  , bindFromLiftedToInt
  , bindFromIntToIntPair
  , bindFromCharToIntPair
  , bindFromMaybeCharToIntPair
  , bindFromMaybeCharToLifted
    -- * Specialized Pure
  , pureIntPair
    -- * Specialized Fail
  , failIntPair
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
import Data.Bytes.Types (Bytes(..))
import Data.Primitive (ByteArray(..))
import Data.Bytes.Parser.Internal (InternalResult(..),Parser(..),unboxBytes,boxBytes,Result#,Bytes#,ST#,uneffectful,uneffectful#,fail,upcastUnitSuccess)
import Data.Bytes.Parser.Unsafe (unconsume)
import GHC.Word (Word32(W32#),Word16(W16#),Word8(W8#))

import qualified Data.Bytes as B
import qualified Data.Primitive as PM

-- | The result of running a parser.
data Result e a
  = Failure e
    -- ^ An error message indicating what went wrong.
  | Success !a !Int
    -- ^ The parsed value and the number of bytes
    -- remaining in parsed slice.
  deriving (Eq,Show)

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

-- | Lift an effectful computation into a parser.
effect :: ST s a -> Parser e s a
effect (ST f) = Parser
  ( \(# _, off, len #) s0 -> case f s0 of
    (# s1, a #) -> (# s1, (# | (# a, off, len #) #) #)
  )

byteArray :: e -> ByteArray -> Parser e s ()
byteArray e !expected = bytes e (B.fromByteArray expected)

bytes :: e -> Bytes -> Parser e s ()
bytes e !expected = Parser
  ( \actual@(# _, off, len #) s ->
    let r = if B.isPrefixOf expected (boxBytes actual)
          then let !(I# movement) = length expected in
            (# | (# (), off +# movement, len -# movement #) #)
          else (# e | #)
     in (# s, r #)
  )

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

-- | Take while the predicate is matched. This is always inlined.
takeWhile :: (Word8 -> Bool) -> Parser e s Bytes
{-# inline takeWhile #-}
takeWhile f = uneffectful $ \chunk -> case B.takeWhile f chunk of
  bs -> InternalSuccess bs (offset chunk + length bs) (length chunk - length bs)

remaining :: Parser e s Bytes
{-# inline remaining #-}
remaining = uneffectful $ \chunk ->
  InternalSuccess chunk (offset chunk + length chunk) 0

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

-- | Specialization of monadic bind for parsers that return 'Char#'.
bindFromCharToLifted :: Parser s e Char# -> (Char# -> Parser s e a) -> Parser s e a
{-# inline bindFromCharToLifted #-}
bindFromCharToLifted (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindFromCharToIntPair :: Parser s e Char# -> (Char# -> Parser s e (# Int#, Int# #)) -> Parser s e (# Int#, Int# #)
{-# inline bindFromCharToIntPair #-}
bindFromCharToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindFromLiftedToInt :: Parser s e a -> (a -> Parser s e Int#) -> Parser s e Int#
{-# inline bindFromLiftedToInt #-}
bindFromLiftedToInt (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindFromLiftedToIntPair :: Parser s e a -> (a -> Parser s e (# Int#, Int# #)) -> Parser s e (# Int#, Int# #)
{-# inline bindFromLiftedToIntPair #-}
bindFromLiftedToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindFromIntToIntPair :: Parser s e Int# -> (Int# -> Parser s e (# Int#, Int# #)) -> Parser s e (# Int#, Int# #)
{-# inline bindFromIntToIntPair #-}
bindFromIntToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindFromMaybeCharToIntPair ::
     Parser s e (# (# #) | Char# #)
  -> ((# (# #) | Char# #) -> Parser s e (# Int#, Int# #))
  -> Parser s e (# Int#, Int# #)
{-# inline bindFromMaybeCharToIntPair #-}
bindFromMaybeCharToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

bindFromMaybeCharToLifted ::
     Parser s e (# (# #) | Char# #)
  -> ((# (# #) | Char# #) -> Parser s e a)
  -> Parser s e a
{-# inline bindFromMaybeCharToLifted #-}
bindFromMaybeCharToLifted (Parser f) g = Parser
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

measure :: Parser e s a -> Parser e s (Int,a)
{-# inline measure #-}
measure (Parser f) = Parser
  (\x@(# _, pre, _ #) s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, post, c #) #) -> (# s1, (# | (# (I# (post -# pre), y),post,c #) #) #)
  )
