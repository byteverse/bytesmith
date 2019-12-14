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

-- | Parse non-resumable sequence of bytes. To parse a byte sequence
-- as text, use the @Ascii@, @Latin@, and @Utf8@ modules instead.
-- Functions for parsing decimal-encoded numbers are found in those
-- modules.
module Data.Bytes.Parser
  ( -- * Types
    Parser
  , Result(..)
  , Slice(..)
    -- * Run Parsers
    -- ** Result
  , parseByteArray
  , parseBytes
  , parseBytesEffectfully
    -- ** Result
  , parseBytesEither
    -- * One Byte
  , any
  , anyBut
    -- * Many Bytes
  , take
  , takeWhile
  , takeTrailedBy
    -- * Skip
  , skipWhile
  , skipTrailedBy
    -- * Match
  , byteArray
  , bytes
  , satisfy
  , satisfyWith
    -- * End of Input
  , endOfInput
  , isEndOfInput
  , remaining
    -- * Scanning
  , scan
    -- * Lookahead
  , peek
  , peek'
    -- * Control Flow
  , fail
  , orElse
  , annotate
  , (<?>)
    -- * Repetition
  , replicate
    -- * Subparsing
  , delimit
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

import Prelude hiding (length,any,fail,takeWhile,take,replicate)

import Data.Bytes.Parser.Internal (InternalResult(..),Parser(..),ST#,unboxBytes)
import Data.Bytes.Parser.Internal (boxBytes,Result#,uneffectful,fail)
import Data.Bytes.Parser.Internal (uneffectful#)
import Data.Bytes.Parser.Types (Result(Failure,Success),Slice(Slice))
import Data.Bytes.Parser.Unsafe (unconsume,expose,cursor)
import Data.Bytes.Types (Bytes(..))
import Data.Primitive (ByteArray(..))
import GHC.Exts (Int(I#),Word#,Int#,Char#,runRW#,(+#),(-#),(>=#))
import GHC.ST (ST(..))
import GHC.Word (Word32(W32#),Word8)
import Data.Primitive.Contiguous (Contiguous,Element)

import qualified Data.Bytes as B
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C

-- | Parse a byte sequence. This can succeed even if the
-- entire slice was not consumed by the parser.
parseBytes :: forall e a. (forall s. Parser e s a) -> Bytes -> Result e a
parseBytes p !b = runResultST action
  where
  action :: forall s. ST# s (Result# e a)
  action s0 = case p @s of
    Parser f -> f (unboxBytes b) s0

-- | Variant of 'parseBytes' that discards the new offset and the
-- remaining length. This does not, however, require the remaining
-- length to be zero. Use 'endOfInput' to accomplish that.
parseBytesEither :: forall e a. (forall s. Parser e s a) -> Bytes -> Either e a
parseBytesEither p !b = runEitherST action
  where
  action :: forall s. ST# s (Result# e a)
  action s0 = case p @s of
    Parser f -> f (unboxBytes b) s0

-- Similar to runResultST
runEitherST :: (forall s. ST# s (Result# e x)) -> Either e x
runEitherST f = case (runRW# (\s0 -> case f s0 of { (# _, r #) -> r })) of
  (# e | #) -> Left e
  (# | (# x, _, _ #) #) -> Right x

-- This is used internally to help reduce boxing when a parser
-- gets run. Due to the late inlining of runRW#, this variant
-- of runST still cause the result value to be boxed. However,
-- it avoids the additional boxing that the Success data
-- constructor would normally cause.
runResultST :: (forall s. ST# s (Result# e x)) -> Result e x
runResultST f = case (runRW# (\s0 -> case f s0 of { (# _, r #) -> r })) of
  (# e | #) -> Failure e
  (# | (# x, off, len #) #) -> Success (Slice (I# off) (I# len) x)

-- | Variant of 'parseBytes' that accepts an unsliced 'ByteArray'.
parseByteArray :: (forall s. Parser e s a) -> ByteArray -> Result e a
parseByteArray p b =
  parseBytes p (Bytes b 0 (PM.sizeofByteArray b))

-- | Variant of 'parseBytes' that allows the parser to be run
-- as part of an existing effectful context.
parseBytesEffectfully :: Parser e s a -> Bytes -> ST s (Result e a)
parseBytesEffectfully (Parser f) !b = ST
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

infix 0 <?>

-- | Infix version of 'annotate'.
(<?>) :: Parser x s a -> e -> Parser e s a
(<?>) = annotate

-- | Annotate a parser. If the parser fails, the error will
--   be returned.
annotate :: Parser x s a -> e -> Parser e s a
annotate p e = p `orElse` fail e

-- | Consumes and returns the next byte in the input.
-- Fails if no characters are left.
any :: e -> Parser e s Word8
{-# inline any #-}
any e = uneffectful $ \chunk -> if length chunk > 0
  then
    let w = PM.indexByteArray (array chunk) (offset chunk) :: Word8
     in InternalSuccess w (offset chunk + 1) (length chunk - 1)
  else InternalFailure e

-- | Match any byte except the given one.
anyBut :: e -> Word8 -> Parser e s Word8
anyBut e g = uneffectful $ \chunk -> if length chunk > 0
  then case B.unsafeIndex chunk 1 of
    w -> if w == g
      then InternalFailure e
      else InternalSuccess w (offset chunk + 1) (length chunk - 1)
  else InternalFailure e

-- | Match any byte, to perform lookahead. Returns 'Nothing' if
--   end of input has been reached. Does not consume any input.
--
--   /Note/: Because this parser does not fail, do not use it
--   with combinators such as 'many', because such as 'many',
--   because such parsers loop until a failure occurs. Careless
--   use will thus result in an infinite loop.
peek :: Parser e s (Maybe Word8)
peek = uneffectful $ \chunk ->
  let v = if length chunk > 0
        then Just (B.unsafeIndex chunk 1)
        else Nothing
  in InternalSuccess v (offset chunk) (length chunk)

-- | Match any byte, to perform lookahead. Does not consume any
--   input, but will fail if end of input has been reached.
peek' :: e -> Parser e s Word8
peek' e = uneffectful $ \chunk -> if length chunk > 0
  then InternalSuccess (B.unsafeIndex chunk 1) (offset chunk) (length chunk)
  else InternalFailure e

-- | A stateful scanner. The predicate consumes and transforms a
--   state argument, and each transformed state is passed to
--   successive invocations of the predicate on each byte of the input
--   until one returns 'Nothing' or the input ends.
--
--   This parser does not fail. It will return the initial state
--   if the predicate returns 'Nothing' on the first byte of input.
--
--   /Note/: Because this parser does not fail, do not use it with
--   combinators such a 'many', because such parsers loop until a
--   failure occurs. Careless use will thus result in an infinite loop.
scan :: state -> (state -> Word8 -> Maybe state) -> Parser e s state
scan s0 t = do
  let go s = do
        mw <- peek
        case mw of
          Nothing -> pure s
          Just w -> case t s w of
            Just s' -> go s'
            Nothing -> pure s
  go s0

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

-- | Take bytes until the specified byte is encountered. Consumes
-- the matched byte as well. Fails if the byte is not present.
-- Visually, the cursor advancement and resulting @Bytes@ for
-- @takeTrailedBy 0x19@ look like this:
--
-- >  0x10 0x13 0x08 0x15 0x19 0x23 0x17 | input
-- > |---->---->---->---->----|          | cursor
-- > {----*----*----*----}               | result bytes
takeTrailedBy :: e -> Word8 -> Parser e s Bytes
takeTrailedBy e !w = do
  !start <- cursor
  skipTrailedBy e w
  !end <- cursor
  !arr <- expose
  pure (Bytes arr start (end - (start + 1)))

-- | Skip all characters until the character from the is encountered
-- and then consume the matching byte as well.
skipTrailedBy :: e -> Word8 -> Parser e s ()
skipTrailedBy e !w = uneffectful# (\c -> skipUntilConsumeByteLoop e w c)

skipUntilConsumeByteLoop ::
     e -- Error message
  -> Word8 -- byte to match
  -> Bytes -- Chunk
  -> Result# e ()
skipUntilConsumeByteLoop e !w !c = if length c > 0
  then if PM.indexByteArray (array c) (offset c) /= (w :: Word8)
    then skipUntilConsumeByteLoop e w (B.unsafeDrop 1 c)
    else (# | (# (), unI (offset c + 1), unI (length c - 1) #) #)
  else (# e | #)

-- | Take the given number of bytes. Fails if there is not enough
--   remaining input.
take :: e -> Int -> Parser e s Bytes
{-# inline take #-}
take e n = uneffectful $ \chunk -> if n <= B.length chunk
  then case B.unsafeTake n chunk of
    bs -> InternalSuccess bs (offset chunk + n) (length chunk - n)
  else InternalFailure e

-- | Consume all remaining bytes in the input.
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

-- | The parser @satisfy p@ succeeds for any byte for which the
--   predicate @p@ returns 'True'. Returns the byte that is
--   actually parsed.
satisfy :: e -> (Word8 -> Bool) -> Parser e s Word8
satisfy e p = satisfyWith e id p
{-# inline satisfy #-}

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds
--   if the predicate @p@ returns 'True' on the transformed value.
--   The parser returns the transformed byte that was parsed.
satisfyWith :: e -> (Word8 -> a) -> (a -> Bool) -> Parser e s Word8
satisfyWith e f p = uneffectful $ \chunk -> if length chunk > 1
  then case B.unsafeIndex chunk 1 of
    w -> if p (f w)
      then InternalSuccess w (offset chunk + 1) (length chunk - 1)
      else InternalFailure e
  else InternalFailure e

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
boxPublicResult (# | (# a, b, c #) #) = Success (Slice (I# b) (I# c) a)
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

-- | Augment a parser with the number of bytes that were consume while
-- it executed.
measure :: Parser e s a -> Parser e s (Int,a)
{-# inline measure #-}
measure (Parser f) = Parser
  (\x@(# _, pre, _ #) s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, post, c #) #) -> (# s1, (# | (# (I# (post -# pre), y),post,c #) #) #)
  )

-- | Run a parser in a delimited context, failing if the requested number
-- of bytes are not available or if the delimited parser does not
-- consume all input. This combinator can be understood as a composition
-- of 'take', 'effect', 'parseBytesEffectfully', and 'endOfInput'. It is
-- provided as a single combinator because for convenience and because it is
-- easy to make mistakes when manually assembling the aforementioned parsers.
-- The pattern of prefixing an encoding with its length is common.
-- This is discussed more in
-- <https://github.com/bos/attoparsec/issues/129 attoparsec issue #129>.
--
-- > delimit e1 e2 n remaining === take e1 n
delimit ::
     e -- ^ Error message when not enough bytes are present
  -> e -- ^ Error message when delimited parser does not consume all input
  -> Int -- ^ Exact number of bytes delimited parser is expected to consume
  -> Parser e s a -- ^ Parser to execute in delimited context
  -> Parser e s a
delimit esz eleftovers (I# n) (Parser f) = Parser
  ( \(# arr, off, len #) s0 -> case len >=# n of
    1# -> case f (# arr, off, n #) s0 of
      (# s1, r #) -> case r of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# a, newOff, leftovers #) #) -> case leftovers of
          0# -> (# s1, (# | (# a, newOff, len -# n #) #) #)
          _ -> (# s1, (# eleftovers | #) #)
    _ -> (# s0, (# esz | #) #)
  )

-- | Replicate a parser @n@ times, writing the results into
-- an array of length @n@. For @Array@ and @SmallArray@, this
-- is lazy in the elements, so be sure the they result of the
-- parser is evaluated appropriately to avoid unwanted thunks.
replicate :: forall arr e s a. (Contiguous arr, Element arr a)
  => Int -- ^ Number of times to run the parser
  -> Parser e s a -- ^ Parser
  -> Parser e s (arr a)
{-# inline replicate #-}
replicate !len p = do
  marr <- effect (C.new len)
  let go :: Int -> Parser e s (arr a)
      go !ix = if ix < len
        then do
          a <- p
          effect (C.write marr ix a)
          go (ix + 1)
        else effect (C.unsafeFreeze marr)
  go 0

unI :: Int -> Int#
unI (I# w) = w
