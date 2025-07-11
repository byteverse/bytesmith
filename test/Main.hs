{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Bytes.Parser (Slice (Slice))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Int (Int16, Int32)
import Data.Primitive (ByteArray (..), PrimArray (..))
import Data.Text.Short (ShortText)
import Data.WideWord (Word128 (Word128))
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import System.ByteOrder (ByteOrder (BigEndian, LittleEndian), Fixed (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty, (===))

import qualified Data.Bits as Bits
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Ascii as Ascii
import qualified Data.Bytes.Parser.Base128 as Base128
import qualified Data.Bytes.Parser.BigEndian as BigEndian
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Leb128 as Leb128
import qualified Data.Bytes.Parser.LittleEndian as LittleEndian
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ testProperty "decStandardInt" $ \i ->
        withSz (show i) $ \str len ->
          P.parseBytes (Latin.decStandardInt ()) str
            === P.Success (Slice len 0 i)
    , testProperty "big-endian-word16-array" $ \(xs :: [Word16]) ->
        let src = Exts.fromList (coerce xs :: [Fixed 'BigEndian Word16])
            res = Exts.fromList xs :: PrimArray Word16
            sz = length xs * 2
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.Success (Slice (sz + 1) 0 res)
              === P.parseBytes (BigEndian.word16Array () (length xs)) bs
    , testProperty "big-endian-word32-array" $ \(xs :: [Word32]) ->
        let src = Exts.fromList (coerce xs :: [Fixed 'BigEndian Word32])
            res = Exts.fromList xs :: PrimArray Word32
            sz = length xs * 4
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.Success (Slice (sz + 1) 0 res)
              === P.parseBytes (BigEndian.word32Array () (length xs)) bs
    , testProperty "little-endian-word32-array" $ \(xs :: [Word32]) ->
        let src = Exts.fromList (coerce xs :: [Fixed 'LittleEndian Word32])
            res = Exts.fromList xs :: PrimArray Word32
            sz = length xs * 4
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.Success (Slice (sz + 1) 0 res)
              === P.parseBytes (LittleEndian.word32Array () (length xs)) bs
    , testProperty "big-endian-word64-array" $ \(xs :: [Word64]) ->
        let src = Exts.fromList (coerce xs :: [Fixed 'BigEndian Word64])
            res = Exts.fromList xs :: PrimArray Word64
            sz = length xs * 8
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.Success (Slice (sz + 1) 0 res)
              === P.parseBytes (BigEndian.word64Array () (length xs)) bs
    , testProperty "little-endian-word64-array" $ \(xs :: [Word64]) ->
        let src = Exts.fromList (coerce xs :: [Fixed 'LittleEndian Word64])
            res = Exts.fromList xs :: PrimArray Word64
            sz = length xs * 8
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.Success (Slice (sz + 1) 0 res)
              === P.parseBytes (LittleEndian.word64Array () (length xs)) bs
    , testProperty "little-endian-word128-array" $ \(xs :: [Word128]) ->
        let src = Exts.fromList xs
            sz = length xs * 16
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.parseBytes (replicateM (length xs) (LittleEndian.word128 ())) bs
              === P.parseBytes (fmap Exts.toList (LittleEndian.word128Array () (length xs))) bs
    , testProperty "big-endian-word128-array" $ \(xs :: [Word128]) ->
        let src = Exts.fromList xs
            sz = length xs * 16
            bs = Bytes (Exts.fromList [0x42 :: Word8] <> untype src) 1 sz
         in P.parseBytes (replicateM (length xs) (BigEndian.word128 ())) bs
              === P.parseBytes (fmap Exts.toList (BigEndian.word128Array () (length xs))) bs
    , testProperty "cstring" $ \(xs :: [Word8]) ->
        let ys = Exts.fromList xs
            bs = Bytes.singleton 0x31 <> ys
         in P.parseBytes (P.cstring () (Exts.Ptr "1"#) *> P.bytes () ys *> pure 42) bs
              === (P.Success (Slice (Bytes.length ys + 1) 0 42) :: P.Result () Integer)
    , testCase "big-endian-word256" $
        P.parseBytesMaybe
          (BigEndian.word256Array () 1)
          ( Exts.fromList
              [ 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0x90
              , 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0x90
              , 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0x90
              , 0x12
              , 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0x90
              , 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0x90
              , 0x12
              , 0x34
              , 0x56
              , 0x78
              , 0x90
              , 0x12
              ]
          )
          @=? Just (Exts.fromList [0x1234567890123456789012345678901212345678901234567890123456789012])
    , testProperty "big-endian-word64" bigEndianWord64
    , testProperty "big-endian-word32" bigEndianWord32
    , testProperty "little-endian-word32" littleEndianWord32
    , testCase "delimit" $
        P.Success (Slice 13 0 (167, 14625))
          @=? P.parseBytes
            ( do
                len <- Latin.decUnsignedInt ()
                Latin.char () ','
                r <-
                  P.delimit () () len $
                    (,)
                      <$> Latin.decUnsignedInt ()
                      <* Latin.char () '*'
                      <*> Latin.decUnsignedInt ()
                Latin.char () '0'
                pure r
            )
            (bytes "9,167*146250")
    , testGroup
        "decUnsignedInt"
        [ testCase "A" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decUnsignedInt ())
                (bytes "742493495120739103935542")
        , testCase "B" $
            P.Success (Slice 8 3 4654667)
              @=? P.parseBytes
                (Latin.decUnsignedInt ())
                (bytes "4654667,55")
        , testCase "C" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decUnsignedInt ())
                (bytes ('1' : show (maxBound :: Int)))
        , testCase "D" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decUnsignedInt ())
                (bytes "2481030337885070917891")
        , testCase "E" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decUnsignedInt ())
                (bytes (show (fromIntegral @Int @Word maxBound + 1)))
        , testCase "F" $ withSz (show (maxBound :: Int)) $ \str len ->
            P.Success (Slice len 0 maxBound)
              @=? P.parseBytes (Latin.decUnsignedInt ()) str
        , testProperty "property" $ \(QC.NonNegative i) ->
            withSz (show i) $ \str len ->
              P.parseBytes (Latin.decUnsignedInt ()) str
                === P.Success (Slice len 0 i)
        ]
    , testGroup
        "hexNibbleLower"
        [ testCase "A" $
            P.parseBytes (Latin.hexNibbleLower ()) (bytes "Ab") @=? P.Failure ()
        , testCase "B" $
            P.parseBytes (Latin.hexNibbleLower ()) (bytes "bA") @=? P.Success (Slice 2 1 0xb)
        , testCase "C" $
            P.parseBytes (Latin.hexNibbleLower ()) (bytes "") @=? P.Failure ()
        ]
    , testGroup
        "tryHexNibbleLower"
        [ testCase "A" $
            P.Success @() (Slice 1 2 Nothing)
              @=? P.parseBytes Latin.tryHexNibbleLower (bytes "Ab")
        , testCase "B" $
            P.Success @() (Slice 2 1 (Just 0xb))
              @=? P.parseBytes Latin.tryHexNibbleLower (bytes "bA")
        , testCase "C" $
            P.Success @() (Slice 1 0 Nothing)
              @=? P.parseBytes Latin.tryHexNibbleLower (bytes "")
        ]
    , testGroup
        "decPositiveInteger"
        [ testCase "A" $
            P.parseBytes
              (Latin.decUnsignedInteger ())
              (bytes "5469999463123462573426452736423546373235260")
              @=? P.Success
                (Slice 44 0 5469999463123462573426452736423546373235260)
        , testProperty "property" $ \(LargeInteger i) ->
            withSz (show i) $ \str len ->
              i
                >= 0
                  QC.==> P.parseBytes (Latin.decUnsignedInteger ()) str
                  === P.Success (Slice len 0 i)
        ]
    , testGroup
        "decTrailingInteger"
        [ testProperty "property" $ \(LargeInteger i) ->
            withSz (show i) $ \str sz ->
              i
                >= 0
                  QC.==> P.parseBytes (Latin.decTrailingInteger 2) str
                  === (P.Success (Slice sz 0 (read ('2' : show i) :: Integer)) :: P.Result () Integer)
        ]
    , testGroup
        "decSignedInteger"
        [ testCase "A" $
            P.parseBytes
              (Latin.decSignedInteger ())
              (bytes "-54699994631234625734264527364235463732352601")
              @=? P.Success
                ( Slice
                    46
                    0
                    (-54699994631234625734264527364235463732352601)
                )
        , testCase "B" $
            P.Success (Slice 25 0 (3, (-206173954435705292503)))
              @=? P.parseBytes
                ( pure (,)
                    <*> Latin.decSignedInteger ()
                    <* Latin.char () 'e'
                    <*> Latin.decSignedInteger ()
                )
                (bytes "3e-206173954435705292503")
        , testProperty "property" $ \(LargeInteger i) ->
            withSz (show i) $ \str len ->
              P.parseBytes (Latin.decSignedInteger ()) str
                === P.Success (Slice len 0 i)
        ]
    , testGroup
        "decSignedInt"
        [ testProperty "A" $ \i -> withSz (show i) $ \str len ->
            P.parseBytes (Latin.decSignedInt ()) str
              === P.Success (Slice len 0 i)
        , testProperty "B" $ \i ->
            let s = (if i >= 0 then "+" else "") ++ show i
             in withSz s $ \str len ->
                  P.parseBytes (Latin.decSignedInt ()) str
                    === P.Success (Slice len 0 i)
        , testCase "C" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decSignedInt ())
                (bytes ('1' : show (maxBound :: Int)))
        , testCase "D" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decSignedInt ())
                (bytes ('-' : '3' : show (maxBound :: Int)))
        , testCase "E" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decSignedInt ())
                (bytes "2481030337885070917891")
        , testCase "F" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decSignedInt ())
                (bytes "-4305030950553840988981")
        , testCase "G" $ withSz (show (minBound :: Int)) $ \str len ->
            P.Success (Slice len 0 minBound)
              @=? P.parseBytes (Latin.decSignedInt ()) str
        , testCase "H" $ withSz (show (maxBound :: Int)) $ \str len ->
            P.Success (Slice len 0 maxBound)
              @=? P.parseBytes (Latin.decSignedInt ()) str
        , testCase "I" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decSignedInt ())
                (bytes (show (fromIntegral @Int @Word maxBound + 1)))
        , testCase "J" $
            -- This is one number lower than the minimum bound for
            -- a signed 64-bit number, but this test will pass on
            -- 32-bit architectures as well.
            P.Failure ()
              @=? P.parseBytes
                (Latin.decSignedInt ())
                (bytes "-9223372036854775809")
        ]
    , testGroup
        "decWord64"
        [ testCase "A" $
            P.Failure ()
              @=? P.parseBytes
                (Latin.decWord64 ())
                (bytes "2481030337885070917891")
        ]
    , testCase "decWord-composition" $
        P.Success (Slice 6 0 (42, 8))
          @=? P.parseBytes
            ( pure (,)
                <*> Ascii.decWord ()
                <* Ascii.char () '.'
                <*> Ascii.decWord ()
                <* Ascii.char () '.'
            )
            (bytes "42.8.")
    , testCase "decWord-replicate" $
        P.Success (Slice 7 0 (Exts.fromList [42, 93] :: PrimArray Word))
          @=? P.parseBytes
            (P.replicate 2 (Ascii.decWord () <* Ascii.char () '.'))
            (bytes "42.93.")
    , testCase "ascii-takeShortWhile" $
        P.Success (Slice 11 0 (Exts.fromList ["the", "world"] :: PM.Array ShortText))
          @=? P.parseBytes
            (P.replicate 2 (Ascii.takeShortWhile (/= ',') <* Ascii.char () ','))
            (bytes "the,world,")
    , testGroup
        "hexFixedWord8"
        [ testCase "A" $
            P.parseBytes (Latin.hexFixedWord8 ()) (bytes "A") @=? P.Failure ()
        , testCase "B" $
            P.parseBytes (Latin.hexFixedWord8 ()) (bytes "0A") @=? P.Success (Slice 3 0 0x0A)
        , testCase "C" $
            P.parseBytes (Latin.hexFixedWord8 ()) (bytes "") @=? P.Failure ()
        , testCase "D" $
            P.parseBytes (Latin.hexFixedWord8 ()) (bytes "A!") @=? P.Failure ()
        ]
    , testGroup
        "hexFixedWord16"
        [ testCase "A" $
            P.parseBytes (Latin.hexFixedWord16 ()) (bytes "A") @=? P.Failure ()
        , testCase "B" $
            P.parseBytes (Latin.hexFixedWord16 ()) (bytes "0A0A") @=? P.Success (Slice 5 0 0x0A0A)
        , testCase "C" $
            P.parseBytes (Latin.hexFixedWord16 ()) (bytes "") @=? P.Failure ()
        , testCase "D" $
            P.parseBytes (Latin.hexFixedWord16 ()) (bytes "A!A!") @=? P.Failure ()
        ]
    , testGroup
        "hexFixedWord32"
        [ testCase "A" $
            P.parseBytes (Latin.hexFixedWord32 ()) (bytes "A") @=? P.Failure ()
        , testCase "B" $
            P.parseBytes (Latin.hexFixedWord32 ()) (bytes "0A0A0A0A") @=? P.Success (Slice 9 0 0x0A0A0A0A)
        , testCase "C" $
            P.parseBytes (Latin.hexFixedWord32 ()) (bytes "") @=? P.Failure ()
        , testCase "D" $
            P.parseBytes (Latin.hexFixedWord32 ()) (bytes "A!A0A0A0") @=? P.Failure ()
        ]
    , testGroup
        "hexFixedWord64"
        [ testCase "A" $
            P.parseBytes (Latin.hexFixedWord64 ()) (bytes "ABCD01235678BCDE")
              @=? P.Success
                (Slice 17 0 0xABCD01235678BCDE)
        ]
    , testGroup
        "base128-w32"
        [ testCase "A" $
            P.Success (Slice 2 0 0x7E)
              @=? P.parseBytes (Base128.word32 ()) (bytes "\x7E")
        , testCase "B" $
            P.Success (Slice 5 0 0x200000)
              @=? P.parseBytes (Base128.word32 ()) (bytes "\x81\x80\x80\x00")
        , testCase "C" $
            P.Success (Slice 4 0 1656614)
              @=? P.parseBytes (Base128.word32 ()) (bytes "\xE5\x8E\x26")
              -- , testProperty "iso" $ \w -> -- TODO
              --     P.parseBytesMaybe (Base.word32 ()) (encodeBase128 (fromIntegral w))
              --     ===
              --     Just w
        ]
    , testGroup
        "leb128-w32"
        [ testCase "A" $
            P.Success (Slice 2 0 0x7E)
              @=? P.parseBytes (Leb128.word32 ()) (bytes "\x7E")
        , testCase "B" $
            P.Success (Slice 5 0 0x200000)
              @=? P.parseBytes (Leb128.word32 ()) (bytes "\x80\x80\x80\x01")
        , testCase "C" $
            P.Success (Slice 4 0 624485)
              @=? P.parseBytes (Leb128.word32 ()) (bytes "\xE5\x8E\x26")
        , testProperty "iso" $ \w ->
            P.parseBytesMaybe (Leb128.word32 ()) (encodeLeb128 (fromIntegral w))
              === Just w
        ]
    , testGroup
        "leb128-w16"
        [ testCase "A" $
            P.Failure ()
              @=? P.parseBytes (Leb128.word16 ()) (bytes "\x80\x80\x04")
        , testCase "B" $
            P.Success (Slice 4 0 0xFFFF)
              @=? P.parseBytes (Leb128.word16 ()) (bytes "\xFF\xFF\x03")
        , testProperty "iso" $ \w ->
            P.parseBytesMaybe (Leb128.word16 ()) (encodeLeb128 (fromIntegral w))
              === Just w
        ]
    , testGroup
        "leb128-i16"
        [ testProperty "iso" $ \(w :: Int16) ->
            P.parseBytesMaybe
              (Leb128.int16 ())
              (encodeLeb128 (fromIntegral @Word16 @Natural (zigzag16 w)))
              === Just w
        ]
    , testGroup
        "leb128-i32"
        [ testProperty "iso" $ \(w :: Int32) ->
            P.parseBytesMaybe
              (Leb128.int32 ())
              (encodeLeb128 (fromIntegral @Word32 @Natural (zigzag32 w)))
              === Just w
        ]
    , testGroup
        "satisfy"
        [ testCase "A" $
            P.Success (Slice 2 0 0x20)
              @=? P.parseBytes (P.satisfy () (== 0x20)) (bytes "\x20")
        ]
    ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

bigEndianWord64 ::
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  QC.Property
bigEndianWord64 a b c d e f g h =
  let arr = runST $ do
        m <- PM.newByteArray 11
        PM.writeByteArray m 0 (0xFF :: Word8)
        PM.writeByteArray m 1 (0xFF :: Word8)
        PM.writeByteArray m 2 (a :: Word8)
        PM.writeByteArray m 3 (b :: Word8)
        PM.writeByteArray m 4 (c :: Word8)
        PM.writeByteArray m 5 (d :: Word8)
        PM.writeByteArray m 6 (e :: Word8)
        PM.writeByteArray m 7 (f :: Word8)
        PM.writeByteArray m 8 (g :: Word8)
        PM.writeByteArray m 9 (h :: Word8)
        PM.writeByteArray m 10 (0xEE :: Word8)
        PM.unsafeFreezeByteArray m
      expected =
        (0 :: Word64)
          + fromIntegral a * 256 ^ (7 :: Integer)
          + fromIntegral b * 256 ^ (6 :: Integer)
          + fromIntegral c * 256 ^ (5 :: Integer)
          + fromIntegral d * 256 ^ (4 :: Integer)
          + fromIntegral e * 256 ^ (3 :: Integer)
          + fromIntegral f * 256 ^ (2 :: Integer)
          + fromIntegral g * 256 ^ (1 :: Integer)
          + fromIntegral h * 256 ^ (0 :: Integer)
   in P.parseBytes (BigEndian.word64 ()) (Bytes arr 2 9)
        === P.Success (Slice 10 1 expected)

bigEndianWord32 ::
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  QC.Property
bigEndianWord32 a b c d =
  let arr = runST $ do
        m <- PM.newByteArray 7
        PM.writeByteArray m 0 (0xFF :: Word8)
        PM.writeByteArray m 1 (0xFF :: Word8)
        PM.writeByteArray m 2 (a :: Word8)
        PM.writeByteArray m 3 (b :: Word8)
        PM.writeByteArray m 4 (c :: Word8)
        PM.writeByteArray m 5 (d :: Word8)
        PM.writeByteArray m 6 (0xEE :: Word8)
        PM.unsafeFreezeByteArray m
      expected =
        (0 :: Word32)
          + fromIntegral a * 256 ^ (3 :: Integer)
          + fromIntegral b * 256 ^ (2 :: Integer)
          + fromIntegral c * 256 ^ (1 :: Integer)
          + fromIntegral d * 256 ^ (0 :: Integer)
   in P.parseBytes (BigEndian.word32 ()) (Bytes arr 2 5)
        === P.Success (Slice 6 1 expected)

littleEndianWord32 ::
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  QC.Property
littleEndianWord32 a b c d =
  let arr = runST $ do
        m <- PM.newByteArray 7
        PM.writeByteArray m 0 (0xFF :: Word8)
        PM.writeByteArray m 1 (0xFF :: Word8)
        PM.writeByteArray m 2 (a :: Word8)
        PM.writeByteArray m 3 (b :: Word8)
        PM.writeByteArray m 4 (c :: Word8)
        PM.writeByteArray m 5 (d :: Word8)
        PM.writeByteArray m 6 (0xEE :: Word8)
        PM.unsafeFreezeByteArray m
      expected =
        (0 :: Word32)
          + fromIntegral a * 256 ^ (0 :: Integer)
          + fromIntegral b * 256 ^ (1 :: Integer)
          + fromIntegral c * 256 ^ (2 :: Integer)
          + fromIntegral d * 256 ^ (3 :: Integer)
   in P.parseBytes (LittleEndian.word32 ()) (Bytes arr 2 5)
        === P.Success (Slice 6 1 expected)

-- The Arbitrary instance for Integer that comes with
-- QuickCheck only generates small numbers.
newtype LargeInteger = LargeInteger Integer
  deriving (Eq, Show)

instance QC.Arbitrary Word128 where
  arbitrary = liftA2 Word128 QC.arbitrary QC.arbitrary

instance QC.Arbitrary LargeInteger where
  arbitrary = do
    n <- QC.choose (1, 27)
    sign <- QC.arbitrary
    r <-
      (if sign then negate else id) . foldr f 0
        <$> replicateM n QC.arbitrary
    pure (LargeInteger r)
   where
    f :: Word8 -> Integer -> Integer
    f w acc = (acc `Bits.shiftL` 8) + fromIntegral w

-- We add an extra 1 since bytes gives us a slice that
-- starts at that offset.
withSz :: String -> (Bytes -> Int -> a) -> a
withSz str f = f (bytes str) (length str + 1)

untype :: PrimArray a -> ByteArray
untype (PrimArray x) = ByteArray x

encodeLeb128 :: Natural -> Bytes
encodeLeb128 x = Bytes.unsafeDrop 1 (Exts.fromList (0xFF : go [] x))
 where
  go !xs !n =
    let (q, r) = quotRem n 128
        r' = fromIntegral @Natural @Word8 r
        w =
          if q == 0
            then r'
            else Bits.setBit r' 7
        xs' = w : xs
     in if q == 0
          then List.reverse xs'
          else go xs' q

-- x zigzagInteger :: Integer -> Natural
-- x zigzagInteger x
-- x   | x>=0 = fromInteger (x `Bits.shiftL` 1)
-- x   | otherwise = fromInteger (negate (x `Bits.shiftL` 1) - 1)

zigzag16 :: Int16 -> Word16
zigzag16 x = fromIntegral ((x `Bits.shiftL` 1) `Bits.xor` (x `Bits.shiftR` 15))

zigzag32 :: Int32 -> Word32
zigzag32 x = fromIntegral ((x `Bits.shiftL` 1) `Bits.xor` (x `Bits.shiftR` 31))
