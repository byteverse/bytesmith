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

-- | Parse input as UTF-8-encoded text. Parsers in this module will
-- fail if they encounter a byte above @0x7F@.
module Data.Bytes.Parser.Utf8
  ( -- * Get Character
    any#
  , shortText
  ) where

import Prelude hiding (length,any,fail,takeWhile)

import Data.Bits ((.&.),(.|.),unsafeShiftL,xor)
import Data.Bytes.Parser.Internal (Parser(..))
import Data.Text.Short (ShortText)
import GHC.Exts (Int(I#),Char(C#),Int#,Char#,(-#),(+#),(>#),chr#)
import GHC.Word (Word8(W8#))

import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes.Parser as Parser
import qualified Data.Primitive as PM
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts

-- | Interpret the next one to four bytes as a UTF-8-encoded character.
-- Fails if the decoded codepoint is in the range U+D800 through U+DFFF.
any# :: e -> Parser e s Char#
{-# noinline any# #-}
any# e = Parser
  (\(# arr, off, len #) s0 -> case len ># 0# of
    1# ->
      let !w0 = Exts.indexWord8Array# arr off
       in if | oneByteChar (W8# w0) -> 
                 (# s0, (# | (# chr# (Exts.word2Int# (Exts.word8ToWord# w0)), off +# 1#, len -# 1# #) #) #)
             | twoByteChar (W8# w0) ->
                 if | I# len > 1
                    , w1 <- Exts.indexWord8Array# arr (off +# 1#)
                    , followingByte (W8# w1)
                    , C# c <- codepointFromTwoBytes (W8# w0) (W8# w1)
                      -> (# s0, (# | (# c, off +# 2#, len -# 2# #) #) #)
                    | otherwise -> (# s0, (# e | #) #)
             | threeByteChar (W8# w0) ->
                 if | I# len > 2
                    , w1 <- Exts.indexWord8Array# arr (off +# 1# )
                    , w2 <- Exts.indexWord8Array# arr (off +# 2# )
                    , followingByte (W8# w1)
                    , !c@(C# c#) <- codepointFromThreeBytes (W8# w0) (W8# w1) (W8# w2)
                    , c < '\xD800' || c > '\xDFFF'
                      -> (# s0, (# | (# c#, off +# 3#, len -# 3# #) #) #)
                    | otherwise -> (# s0, (# e | #) #)
             | fourByteChar (W8# w0) ->
                 if | I# len > 3
                    , w1 <- Exts.indexWord8Array# arr (off +# 1# )
                    , w2 <- Exts.indexWord8Array# arr (off +# 2# )
                    , w3 <- Exts.indexWord8Array# arr (off +# 3# )
                    , followingByte (W8# w1)
                    , !(C# c#) <- codepointFromFourBytes (W8# w0) (W8# w1) (W8# w2) (W8# w3)
                      -> (# s0, (# | (# c#, off +# 4#, len -# 4# #) #) #)
                    | otherwise -> (# s0, (# e | #) #)
             | otherwise -> (# s0, (# e | #) #)
    _ -> (# s0, (# e | #) #)
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

followingByte :: Word8 -> Bool
followingByte !w = xor w 0b01000000 .&. 0b11000000 == 0b11000000

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

unI :: Int -> Int#
unI (I# w) = w

-- | Consume input that matches the argument. Fails if the
-- input does not match.
shortText :: e -> ShortText -> Parser e s ()
shortText e !t = Parser.byteArray e
  (shortByteStringToByteArray (TS.toShortByteString t))

shortByteStringToByteArray ::
     BSS.ShortByteString
  -> PM.ByteArray
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x
