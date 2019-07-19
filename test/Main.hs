{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Control.Exception (throwIO)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Bytes.Parser (Parser,Result(..))

import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  putStrLn "Finished"

testA :: IO ()
testA =
  let r = P.parseByteArray
        ( pure (,)
        <*> P.decWord ()
        <*  P.ascii () '.'
        <*> P.decWord ()
        <*  P.ascii () '.'
        ) (pack "42.8.")
   in case r of
        Failure () -> fail "test A failed parsing"
        Success (42,8) 5 0 -> pure ()
        Success _ _ _ -> fail "test A wrong result"

bytes :: String -> Bytes
bytes s = let b = pack s in Bytes b 0 (PM.sizeofByteArray b)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

