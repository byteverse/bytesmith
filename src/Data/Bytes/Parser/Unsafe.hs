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

-- | Everything in this module is unsafe and can lead to
-- nondeterministic output or segfaults if used incorrectly.
module Data.Bytes.Parser.Unsafe
  ( cursor
  , expose
  , unconsume
  , jump
  ) where

import Prelude hiding (length)

import Data.Primitive (ByteArray)
import Data.Bytes.Types (Bytes(..))
import Data.Bytes.Parser.Internal (Parser(..),uneffectful,Result#,uneffectful#)
import Data.Bytes.Parser.Internal (InternalResult(..))

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

