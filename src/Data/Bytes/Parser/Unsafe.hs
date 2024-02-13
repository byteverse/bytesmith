{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Everything in this module is unsafe and can lead to
nondeterministic output or segfaults if used incorrectly.
-}
module Data.Bytes.Parser.Unsafe
  ( -- * Types
    Parser (..)

    -- * Functions
  , cursor
  , cursor#
  , expose
  , unconsume
  , jump
  , uneffectful
  ) where

import Prelude hiding (length)

import Data.Bytes.Parser.Internal (Parser (..), Result (..), uneffectful, uneffectfulInt#)
import Data.Bytes.Types (Bytes (..))
import Data.Primitive (ByteArray)
import GHC.Exts (Int (I#), Int#)

{- | Get the current offset into the chunk. Using this makes
it possible to observe the internal difference between 'Bytes'
that refer to equivalent slices. Be careful.
-}
cursor :: Parser e s Int
cursor = uneffectful $ \Bytes {offset, length} ->
  Success offset offset length

-- | Variant of 'cursor' with unboxed result.
cursor# :: Parser e s Int#
cursor# = uneffectfulInt# $ \Bytes {offset = I# off, length = I# len} -> (# | (# off, off, len #) #)

{- | Return the byte array being parsed. This includes bytes
that preceed the current offset and may include bytes that
go beyond the length. This is somewhat dangerous, so only
use this is you know what you're doing.
-}
expose :: Parser e s ByteArray
expose = uneffectful $ \Bytes {length, offset, array} ->
  Success array offset length

{- | Move the cursor back by @n@ bytes. Precondition: you
must have previously consumed at least @n@ bytes.
-}
unconsume :: Int -> Parser e s ()
unconsume n = uneffectful $ \Bytes {length, offset} ->
  Success () (offset - n) (length + n)

{- | Set the position to the given index. Precondition: the index
must be valid. It should be the result of an earlier call to
'cursor'.
-}
jump :: Int -> Parser e s ()
jump ix = uneffectful $ \(Bytes {length, offset}) ->
  Success () ix (length + (offset - ix))
