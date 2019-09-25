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
      P.parseBytes (Latin.decStandardInt ()) (bytes (show i)) === P.Success i 0
  , testProperty "big-endian-word64" bigEndianWord64
  , testCase "delimit" $
      P.Success (167,14625) 0
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
        P.Success 4654667 3
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
    , testCase "F" $
        P.Success maxBound 0
        @=?
        P.parseBytes (Latin.decUnsignedInt ())
          (bytes (show (maxBound :: Int)))
    , testProperty "property" $ \(QC.NonNegative i) ->
        P.parseBytes (Latin.decUnsignedInt ()) (bytes (show i))
        ===
        P.Success i 0
    ]
  , testGroup "hexNibbleLower"
    [ testCase "A" $
        P.parseBytes (Latin.hexNibbleLower ()) (bytes "Ab") @=? P.Failure ()
    , testCase "B" $
        P.parseBytes (Latin.hexNibbleLower ()) (bytes "bA") @=? P.Success 0xb 1
    , testCase "C" $
        P.parseBytes (Latin.hexNibbleLower ()) (bytes "") @=? P.Failure ()
    ]
  , testGroup "tryHexNibbleLower"
    [ testCase "A" $
        P.parseBytes Latin.tryHexNibbleLower (bytes "Ab") @=? P.Success @() Nothing 2
    , testCase "B" $
        P.parseBytes Latin.tryHexNibbleLower (bytes "bA") @=? P.Success @() (Just 0xb) 1
    , testCase "C" $
        P.parseBytes Latin.tryHexNibbleLower (bytes "") @=? P.Success @() Nothing 0
    ]
  , testGroup "decPositiveInteger"
    [ testCase "A" $
        P.parseBytes (Latin.decUnsignedInteger ())
          (bytes "5469999463123462573426452736423546373235260")
        @=?
        P.Success 5469999463123462573426452736423546373235260 0
    , testProperty "property" $ \(LargeInteger i) ->
        i >= 0
        QC.==>
        P.parseBytes (Latin.decUnsignedInteger ()) (bytes (show i))
        ===
        P.Success i 0
    ]
  , testGroup "decTrailingInteger"
    [ testProperty "property" $ \(LargeInteger i) ->
        i >= 0
        QC.==>
        P.parseBytes (Latin.decTrailingInteger 2) (bytes (show i))
        ===
        (P.Success (read ('2' : show i) :: Integer) 0 :: P.Result () Integer)
    ]
  , testGroup "decSignedInteger"
    [ testCase "A" $
        P.parseBytes (Latin.decSignedInteger ())
          (bytes "-54699994631234625734264527364235463732352601")
        @=?
        P.Success (-54699994631234625734264527364235463732352601) 0
    , testCase "B" $
        P.Success (3,(-206173954435705292503)) 0
        @=?
        P.parseBytes
          ( pure (,)
            <*> Latin.decSignedInteger ()
            <*  Latin.char () 'e'
            <*> Latin.decSignedInteger ()
          ) (bytes "3e-206173954435705292503")
    , testProperty "property" $ \(LargeInteger i) ->
        P.parseBytes (Latin.decSignedInteger ()) (bytes (show i))
        ===
        P.Success i 0
    ]
  , testGroup "decSignedInt"
    [ testProperty "A" $ \i ->
        P.parseBytes (Latin.decSignedInt ()) (bytes (show i))
        ===
        P.Success i 0
    , testProperty "B" $ \i ->
        P.parseBytes
          (Latin.decSignedInt ())
          (bytes ((if i >= 0 then "+" else "") ++ show i))
        ===
        P.Success i 0
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
    , testCase "G" $
        P.Success minBound 0
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes (show (minBound :: Int)))
    , testCase "H" $
        P.Success maxBound 0
        @=?
        P.parseBytes (Latin.decSignedInt ())
          (bytes (show (maxBound :: Int)))
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
  , testCase "decWord-composition" $
      P.Success (42,8) 0
      @=?
      P.parseBytes
        ( pure (,)
        <*> Ascii.decWord ()
        <*  Ascii.char () '.'
        <*> Ascii.decWord ()
        <*  Ascii.char () '.'
        ) (bytes "42.8.")
  , testCase "decWord-replicate" $
      P.Success (Exts.fromList [42,93] :: PrimArray Word) 0
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
      P.Success expected 1

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

