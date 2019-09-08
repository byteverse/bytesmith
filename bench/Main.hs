{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Gauge.Main (defaultMain,bgroup,bench,whnf)

import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

main :: IO ()
main = defaultMain
  [ bench "decPositiveInteger" $ whnf
      (\x -> P.parseByteArray (P.decPositiveInteger ()) x)
      encodedBigNumber
  ]

encodedBigNumber :: ByteArray
encodedBigNumber = stringToByteArray $ show $ id @Integer $
  246246357264327645234627753190240202405243024304504230544
  *
  732345623640035232405249305932503920593209520932095234651

stringToByteArray :: String -> ByteArray
stringToByteArray =
  Exts.fromList . map (fromIntegral @Int @Word8 . ord)

pack :: String -> Bytes
pack str =
  let barr = Exts.fromList (map (fromIntegral @Int @Word8 . ord) str)
   in Bytes barr 0 (PM.sizeofByteArray barr)

toSlice :: ByteArray -> Bytes
toSlice b = Bytes b 0 (PM.sizeofByteArray b)


