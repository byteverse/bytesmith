{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Primitive (ByteArray,PrimArray)
import Data.Word (Word8,Word64)
import Data.Char (ord)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Bytes.Parser.Types (Slice(Slice))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),testCase)
import Test.Tasty.QuickCheck ((===),testProperty)

import qualified Data.Bits as Bits
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Ascii as Ascii
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.BigEndian as BigEndian
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser"
  [ testProperty "decStandardInt" $ \i ->
      withSz (show i) $ \str len ->
        P.parseBytes (Latin.decStandardInt ()) str
        ===
        P.Success (Slice len 0 i)
  , testProperty "big-endian-word64" bigEndianWord64
  , testCase "delimit" $
      P.Success (Slice 13 0 (167,14625))
      @=?
      P.parseBytes
        (do len <- Latin.decUnsignedInt ()
            Latin.char () ','
            r <- P.delimit () () len $ (,)
              <$> Latin.decUnsignedInt ()
              <*  Latin.char () '*'
              <*> Latin.decUnsignedInt ()
            Latin.char () '0'
            pure r
        ) (bytes "9,167*146250")
  , testGroup "decUnsignedInt"
    [ testCase "A" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decUnsignedInt ())
          (bytes "742493495120739103935542")
    , testCase "B" $
        P.Success (Slice 8 3 4654667)
        @=?
        P.parseBytes (Latin.decUnsignedInt ())
          (bytes "4654667,55")
    , testCase "C" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decUnsignedInt ())
          (bytes ('1' : show (maxBound :: Int)))
    , testCase "D" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decUnsignedInt ())
          (bytes "2481030337885070917891")
    , testCase "E" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decUnsignedInt ())
          (bytes (show (fromIntegral @Int @Word maxBound + 1)))
    , testCase "F" $ withSz (show (maxBound :: Int)) $ \str len ->
        P.Success (Slice len 0 maxBound)
        @=?
        P.parseBytes (Latin.decUnsignedInt ()) str
    , testProperty "property" $ \(QC.NonNegative i) ->
        withSz (show i) $ \str len ->
          P.parseBytes (Latin.decUnsignedInt ()) str
          ===
          P.Success (Slice len 0 i)
    ]
  , testGroup "hexNibbleLower"
    [ testCase "A" $
        P.parseBytes (Latin.hexNibbleLower ()) (bytes "Ab") @=? P.Failure ()
    , testCase "B" $
        P.parseBytes (Latin.hexNibbleLower ()) (bytes "bA") @=? P.Success (Slice 2 1 0xb)
    , testCase "C" $
        P.parseBytes (Latin.hexNibbleLower ()) (bytes "") @=? P.Failure ()
    ]
  , testGroup "tryHexNibbleLower"
    [ testCase "A" $
        P.Success @() (Slice 1 2 Nothing)
        @=?
        P.parseBytes Latin.tryHexNibbleLower (bytes "Ab")
    , testCase "B" $
        P.Success @() (Slice 2 1 (Just 0xb))
        @=?
        P.parseBytes Latin.tryHexNibbleLower (bytes "bA")
    , testCase "C" $
        P.Success @() (Slice 1 0 Nothing)
        @=?
        P.parseBytes Latin.tryHexNibbleLower (bytes "")
    ]
  , testGroup "decPositiveInteger"
    [ testCase "A" $
        P.parseBytes (Latin.decUnsignedInteger ())
          (bytes "5469999463123462573426452736423546373235260")
        @=?
        P.Success
          (Slice 44 0 5469999463123462573426452736423546373235260)
    , testProperty "property" $ \(LargeInteger i) ->
        withSz (show i) $ \str len ->
          i >= 0
          QC.==>
          P.parseBytes (Latin.decUnsignedInteger ()) str
          ===
          P.Success (Slice len 0 i)
    ]
  , testGroup "decTrailingInteger"
    [ testProperty "property" $ \(LargeInteger i) ->
        withSz (show i) $ \str sz ->
          i >= 0
          QC.==>
          P.parseBytes (Latin.decTrailingInteger 2) str
          ===
          (P.Success (Slice sz 0 (read ('2' : show i) :: Integer)) :: P.Result () Integer)
    ]
  , testGroup "decSignedInteger"
    [ testCase "A" $
        P.parseBytes (Latin.decSignedInteger ())
          (bytes "-54699994631234625734264527364235463732352601")
        @=?
        P.Success
          ( Slice 46 0
            (-54699994631234625734264527364235463732352601)
          )
    , testCase "B" $ 
        P.Success (Slice 25 0 (3,(-206173954435705292503)))
        @=?
        P.parseBytes
          ( pure (,)
            <*> Latin.decSignedInteger ()
            <*  Latin.char () 'e'
            <*> Latin.decSignedInteger ()
          ) (bytes "3e-206173954435705292503")
    , testProperty "property" $ \(LargeInteger i) ->
        withSz (show i) $ \str len ->
          P.parseBytes (Latin.decSignedInteger ()) str
          ===
          P.Success (Slice len 0 i)
    ]
  , testGroup "decSignedInt"
    [ testProperty "A" $ \i -> withSz (show i) $ \str len ->
        P.parseBytes (Latin.decSignedInt ()) str
        ===
        P.Success (Slice len 0 i)
    , testProperty "B" $ \i ->
        let s = (if i >= 0 then "+" else "") ++ show i in
        withSz s $ \str len ->
          P.parseBytes (Latin.decSignedInt ()) str
          ===
          P.Success (Slice len 0 i)
    , testCase "C" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes ('1' : show (maxBound :: Int)))
    , testCase "D" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes ('-' : '3' : show (maxBound :: Int)))
    , testCase "E" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes "2481030337885070917891")
    , testCase "F" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes "-4305030950553840988981")
    , testCase "G" $ withSz (show (minBound :: Int)) $ \str len ->
        P.Success (Slice len 0 minBound)
        @=?
        P.parseBytes (Latin.decSignedInt ()) str
    , testCase "H" $ withSz (show (maxBound :: Int)) $ \str len ->
        P.Success (Slice len 0 maxBound)
        @=?
        P.parseBytes (Latin.decSignedInt ()) str
    , testCase "I" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes (show (fromIntegral @Int @Word maxBound + 1)))
    , testCase "J" $
        -- This is one number lower than the minimum bound for
        -- a signed 64-bit number, but this test will pass on
        -- 32-bit architectures as well.
        P.Failure ()
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes "-9223372036854775809")
    ]
  , testGroup "decWord64"
    [ testCase "A" $
        P.Failure ()
        @=?
        P.parseBytes (Latin.decWord64 ())
          (bytes "2481030337885070917891")
    ]
  , testCase "decWord-composition" $
      P.Success (Slice 6 0 (42,8))
      @=?
      P.parseBytes
        ( pure (,)
        <*> Ascii.decWord ()
        <*  Ascii.char () '.'
        <*> Ascii.decWord ()
        <*  Ascii.char () '.'
        ) (bytes "42.8.")
  , testCase "decWord-replicate" $
      P.Success (Slice 7 0 (Exts.fromList [42,93] :: PrimArray Word))
      @=?
      P.parseBytes
        (P.replicate 2 (Ascii.decWord () <* Ascii.char () '.'))
        (bytes "42.93.")
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

bigEndianWord64 ::
     Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> QC.Property
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
      expected = (0 :: Word64)
        + fromIntegral a * 256 ^ (7 :: Integer)
        + fromIntegral b * 256 ^ (6 :: Integer)
        + fromIntegral c * 256 ^ (5 :: Integer)
        + fromIntegral d * 256 ^ (4 :: Integer)
        + fromIntegral e * 256 ^ (3 :: Integer)
        + fromIntegral f * 256 ^ (2 :: Integer)
        + fromIntegral g * 256 ^ (1 :: Integer)
        + fromIntegral h * 256 ^ (0 :: Integer)
   in P.parseBytes (BigEndian.word64 ()) (Bytes arr 2 9)
      ===
      P.Success (Slice 10 1 expected)

-- The Arbitrary instance for Integer that comes with
-- QuickCheck only generates small numbers.
newtype LargeInteger = LargeInteger Integer
  deriving (Eq,Show)

instance QC.Arbitrary LargeInteger where
  arbitrary = do
      n <- QC.choose (1, 27)
      sign <- QC.arbitrary
      r <- (if sign then negate else id) . foldr f 0
        <$> replicateM n QC.arbitrary
      pure (LargeInteger r)
    where
      f :: Word8 -> Integer -> Integer
      f w acc = (acc `Bits.shiftL` 8) + fromIntegral w

-- We add an extra 1 since bytes gives us a slice that
-- starts at that offset.
withSz :: String -> (Bytes -> Int -> a) -> a
withSz str f = f (bytes str) (length str + 1)
