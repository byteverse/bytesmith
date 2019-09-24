{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Gauge.Main (defaultMain,bench,whnf)

import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified GHC.Exts as Exts

main :: IO ()
main = defaultMain
  [ bench "decPositiveInteger" $ whnf
      (\x -> P.parseByteArray (Latin.decUnsignedInteger ()) x)
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
