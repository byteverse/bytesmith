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

-- | Parse input as ASCII-encoded text. Parsers in
-- this module will fail if they encounter a byte above
-- @0x7F@.
module Data.Bytes.Parser.Ascii
  ( Latin.char
  , Latin.char2
  , Latin.char3
  , Latin.char4
  , any
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser(..),uneffectful)
import Data.Bytes.Parser.Internal (InternalResult(..),indexLatinCharArray)
import Data.Word (Word8)
import Data.Char (ord)

import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Primitive as PM

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- | Consumes and returns the next character in the input.
any :: e -> Parser e s Char
any e = uneffectful $ \chunk -> if length chunk > 0
  then
    let c = indexLatinCharArray (array chunk) (offset chunk)
     in if c < '\128'
          then InternalSuccess c (offset chunk + 1) (length chunk - 1)
          else InternalFailure e
  else InternalFailure e

