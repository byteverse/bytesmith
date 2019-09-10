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
  ( char
  , char2
  , char3
  , char4
  , any
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser(..),uneffectful)
import Data.Bytes.Parser.Internal (InternalResult(..),indexLatinCharArray)
import Data.Word (Word8)
import Data.Char (ord)

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

