module Data.Bytes.Parser.Types
  ( Parser(..)
  , Result(..)
  , Slice(..)
  ) where

import Data.Bytes.Parser.Internal (Parser(..))

-- | The result of running a parser.
data Result e a
  = Failure e
    -- ^ An error message indicating what went wrong.
  | Success {-# UNPACK #-} !(Slice a)
    -- ^ The parsed value and the number of bytes
    -- remaining in parsed slice.
  deriving (Eq,Show)

-- | Slicing metadata (an offset and a length) accompanied
-- by a value. This does not represent a slice into the
-- value. This type is intended to be used as the result
-- of an executed parser. In this context the slicing metadata
-- describe a slice into to the array (or byte array) that
-- from which the value was parsed.
--
-- It is often useful to check the @length@ when a parser
-- succeeds since a non-zero length indicates that there
-- was additional unconsumed input. The @offset@ is only
-- ever needed to construct a new slice (via @Bytes@ or
-- @SmallVector@) from the remaining input.
data Slice a = Slice
  { offset :: {-# UNPACK #-} !Int
    -- ^ Offset into the array.
  , length :: {-# UNPACK #-} !Int
    -- ^ Length of the slice.
  , value :: a
    -- ^ The structured data that was successfully parsed.
  } deriving (Eq,Show)
