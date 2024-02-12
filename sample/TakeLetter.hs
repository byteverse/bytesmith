{-# language MagicHash #-}

-- Build with:
--   ghc -fforce-recomp -O2 -ddump-simpl -dsuppress-all -ddump-to-file sample/TakeLetter.hs
-- to examine GHC optimizations.
module TakeLetter
  ( takeLetter
  ) where

import Data.Bytes.Parser (Parser)
import Data.Text.Short (ShortText)
import Data.Bytes.Parser.Ascii (takeShortWhile)
import GHC.Exts

takeLetter :: Parser e s ShortText
{-# noinline takeLetter #-}
takeLetter = takeShortWhile (== 'A')
