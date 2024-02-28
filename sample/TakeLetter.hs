{-# LANGUAGE MagicHash #-}

-- Build with:
--   cabal build --write-ghc-environment-files=always
--   ghc -fforce-recomp -O2 -ddump-simpl -dsuppress-all -ddump-to-file sample/TakeLetter.hs
-- to examine GHC optimizations.
module TakeLetter
  ( takeLetter
  ) where

import Data.Bytes.Parser (Parser)
import Data.Bytes.Parser.Ascii (takeShortWhile)
import Data.Text.Short (ShortText)
import GHC.Exts

takeLetter :: Parser e s ShortText
{-# NOINLINE takeLetter #-}
takeLetter = takeShortWhile (== 'A')
