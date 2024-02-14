{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Data.Bytes.Parser.Base128
  ( -- * Unsigned
    word16
  , word32
  , word64
  ) where

import Control.Monad (when)
import Data.Bits (bit, clearBit, testBit, unsafeShiftL, (.|.))
import Data.Bytes.Parser (Parser)
import Data.Word (Word16, Word32, Word64, Word8)

import qualified Data.Bytes.Parser as P

word16 :: e -> Parser e s Word16
word16 e = fromIntegral @Word64 @Word16 <$> stepBoundedWord e 16 0

word32 :: e -> Parser e s Word32
word32 e = fromIntegral @Word64 @Word32 <$> stepBoundedWord e 32 0

word64 :: e -> Parser e s Word64
word64 e = fromIntegral @Word64 @Word64 <$> stepBoundedWord e 64 0

stepBoundedWord :: e -> Int -> Word64 -> Parser e s Word64
stepBoundedWord e !bitLimit !acc = do
  when (acc >= bit (bitLimit - 7)) $ P.fail e
  raw <- P.any e
  let content = clearBit raw 7
      acc' = unsafeShiftL acc 7 .|. fromIntegral @Word8 @Word64 content
  if testBit raw 7
    then stepBoundedWord e bitLimit acc'
    else pure acc'
