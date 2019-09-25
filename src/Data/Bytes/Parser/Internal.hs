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
  , boxBytes
  , unboxBytes
  , unboxResult
  , fail
  , indexLatinCharArray
  , upcastUnitSuccess
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Control.Applicative (Alternative)
import Data.Primitive (ByteArray(ByteArray))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Kind (Type)
import GHC.Exts (TYPE,RuntimeRep,Int(I#),Int#,State#,ByteArray#,Char(C#))

import qualified Control.Applicative
import qualified Control.Monad
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

pureParser :: a -> Parser e s a
pureParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))


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

upcastUnitSuccess :: (# Int#, Int# #) -> Result# e ()
{-# inline upcastUnitSuccess #-}
upcastUnitSuccess (# b, c #) = (# | (# (), b, c #) #)

