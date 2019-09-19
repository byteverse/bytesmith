{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Control.Exception (throwIO)
import Control.Monad (replicateM)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Bytes.Parser (Parser,Result(..))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),testCase)
import Test.Tasty.QuickCheck ((===),testProperty)

import qualified Data.Bits as Bits
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Ascii as Ascii
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser"
  [ testProperty "decStandardInt" $ \i ->
      P.parseBytes (Latin.decStandardInt ()) (bytes (show i)) === P.Success i 0
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
    , testProperty "property" $ \(QC.NonNegative i) ->
        P.parseBytes (Latin.decUnsignedInt ()) (bytes (show i))
        ===
        P.Success i 0
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
  , testGroup "decSignedInteger"
    [ testCase "A" $ 
        P.parseBytes (Latin.decSignedInteger ())
          (bytes "-54699994631234625734264527364235463732352601")
        @=?
        P.Success (-54699994631234625734264527364235463732352601) 0
    , testProperty "property" $ \(LargeInteger i) ->
        P.parseBytes (Latin.decSignedInteger ()) (bytes (show i))
        ===
        P.Success i 0
    ]
  , testProperty "decSignedInt-A" $ \i ->
      P.parseBytes (Latin.decSignedInt ()) (bytes (show i)) === P.Success i 0
  , testProperty "decSignedInt-B" $ \i ->
      P.parseBytes
        (Latin.decSignedInt ())
        (bytes ((if i >= 0 then "+" else "") ++ show i))
      ===
      P.Success i 0
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
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

-- The Arbitrary instance for Integer that comes with
-- QuickCheck only generates small numbers.
newtype LargeInteger = LargeInteger Integer
  deriving (Eq,Show)

instance QC.Arbitrary LargeInteger where
  arbitrary = QC.sized $ \sz -> do
      n <- QC.choose (1, sz)
      sign <- QC.arbitrary
      r <- (if sign then negate else id) . foldr f 0
        <$> replicateM n QC.arbitrary
      pure (LargeInteger r)
    where
      f :: Word8 -> Integer -> Integer
      f w acc = (acc `Bits.shiftL` 8) + fromIntegral w

