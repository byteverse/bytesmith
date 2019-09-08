{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Control.Exception (throwIO)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Bytes.Parser (Parser,Result(..))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),testCase)
import Test.Tasty.QuickCheck ((===),testProperty)

import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser"
  [ testProperty "decStandardInt" $ \i ->
      P.parseBytes (P.decStandardInt ()) (bytes (show i)) === P.Success i 0
  , testGroup "decUnsignedInt"
    [ testCase "A" $
        P.Failure ()
        @=?
        P.parseBytes (P.decUnsignedInt ())
          (bytes "742493495120739103935542")
    , testProperty "property" $ \(QC.NonNegative i) ->
        P.parseBytes (P.decUnsignedInt ()) (bytes (show i))
        ===
        P.Success i 0
    ]
  , testGroup "decPositiveInteger"
    [ testCase "A" $ 
        P.parseBytes (P.decPositiveInteger ())
          (bytes "5469999463123462573426452736423546373235260")
        @=?
        P.Success 5469999463123462573426452736423546373235260 0
    , testProperty "property" $ \(QC.NonNegative i) ->
        P.parseBytes (P.decUnsignedInt ()) (bytes (show i)) === P.Success i 0
    ]
  , testProperty "decSignedInt-A" $ \i ->
      P.parseBytes (P.decSignedInt ()) (bytes (show i)) === P.Success i 0
  , testProperty "decSignedInt-B" $ \i ->
      P.parseBytes
        (P.decSignedInt ())
        (bytes ((if i >= 0 then "+" else "") ++ show i))
      ===
      P.Success i 0
  , testCase "decWord-composition" $
      P.Success (42,8) 0
      @=?
      P.parseBytes
        ( pure (,)
        <*> P.decWord ()
        <*  P.ascii () '.'
        <*> P.decWord ()
        <*  P.ascii () '.'
        ) (bytes "42.8.")
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)
