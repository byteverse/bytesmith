{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Provides levity-polymorphic variants of @>>=@, @>>@, and @pure@
used to assemble parsers whose result types are unlifted. This
cannot be used with the @RebindableSyntax@ extension because that
extension disallows representations other than @LiftedRep@. Consequently,
users of this module must manually desugar do notation. See the
@url-bytes@ library for an example of this module in action.

Only resort to the functions in this module after checking that
GHC is unable to optimize away @I#@ and friends in your code.
-}
module Data.Bytes.Parser.Rebindable
  ( Bind (..)
  , Pure (..)
  ) where

import Data.Bytes.Parser.Internal (Parser (..))
import GHC.Exts (RuntimeRep (..), TYPE)
import Prelude ()
import GHC.Exts (LiftedRep)

class Bind (ra :: RuntimeRep) (rb :: RuntimeRep) where
  (>>=) ::
    forall e s (a :: TYPE ra) (b :: TYPE rb).
    Parser e s a ->
    (a -> Parser e s b) ->
    Parser e s b
  (>>) ::
    forall e s (a :: TYPE ra) (b :: TYPE rb).
    Parser e s a ->
    Parser e s b ->
    Parser e s b

class Pure (ra :: RuntimeRep) where
  pure :: forall e s (a :: TYPE ra). a -> Parser e s a

pureParser :: a -> Parser e s a
{-# INLINE pureParser #-}
pureParser a =
  Parser
    (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindParser :: Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# INLINE bindParser #-}
bindParser (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceParser :: Parser e s a -> Parser e s b -> Parser e s b
{-# INLINE sequenceParser #-}
sequenceParser (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

pureIntParser ::
  forall (a :: TYPE 'IntRep) e s.
  a ->
  Parser e s a
{-# INLINE pureIntParser #-}
pureIntParser a =
  Parser
    (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindIntParser ::
  forall (a :: TYPE 'IntRep) e s b.
  Parser e s a ->
  (a -> Parser e s b) ->
  Parser e s b
{-# INLINE bindIntParser #-}
bindIntParser (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

bindWordParser ::
  forall (a :: TYPE 'WordRep) e s b.
  Parser e s a ->
  (a -> Parser e s b) ->
  Parser e s b
{-# INLINE bindWordParser #-}
bindWordParser (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceIntParser ::
  forall (a :: TYPE 'IntRep) e s b.
  Parser e s a ->
  Parser e s b ->
  Parser e s b
{-# INLINE sequenceIntParser #-}
sequenceIntParser (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

sequenceWordParser ::
  forall (a :: TYPE 'WordRep) e s b.
  Parser e s a ->
  Parser e s b ->
  Parser e s b
{-# INLINE sequenceWordParser #-}
sequenceWordParser (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

pureIntPairParser ::
  forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) e s.
  a ->
  Parser e s a
{-# INLINE pureIntPairParser #-}
pureIntPairParser a =
  Parser
    (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindIntPairParser ::
  forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) e s b.
  Parser e s a ->
  (a -> Parser e s b) ->
  Parser e s b
{-# INLINE bindIntPairParser #-}
bindIntPairParser (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

pureInt5Parser ::
  forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])) e s.
  a ->
  Parser e s a
{-# INLINE pureInt5Parser #-}
pureInt5Parser a =
  Parser
    (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindInt5Parser ::
  forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])) e s b.
  Parser e s a ->
  (a -> Parser e s b) ->
  Parser e s b
{-# INLINE bindInt5Parser #-}
bindInt5Parser (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceInt5Parser ::
  forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])) e s b.
  Parser e s a ->
  Parser e s b ->
  Parser e s b
{-# INLINE sequenceInt5Parser #-}
sequenceInt5Parser (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

sequenceIntPairParser ::
  forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) e s b.
  Parser e s a ->
  Parser e s b ->
  Parser e s b
{-# INLINE sequenceIntPairParser #-}
sequenceIntPairParser (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

bindInt2to5Parser ::
  forall
    (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep]))
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]))
    e
    s.
  Parser e s a ->
  (a -> Parser e s b) ->
  Parser e s b
{-# INLINE bindInt2to5Parser #-}
bindInt2to5Parser (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceInt2to5Parser ::
  forall
    (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep]))
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]))
    e
    s.
  Parser e s a ->
  Parser e s b ->
  Parser e s b
{-# INLINE sequenceInt2to5Parser #-}
sequenceInt2to5Parser (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

instance Bind LiftedRep LiftedRep where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindParser
  (>>) = sequenceParser

instance Bind 'WordRep LiftedRep where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindWordParser
  (>>) = sequenceWordParser

instance Bind 'IntRep LiftedRep where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindIntParser
  (>>) = sequenceIntParser

instance Bind ('TupleRep '[ 'IntRep, 'IntRep]) LiftedRep where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindIntPairParser
  (>>) = sequenceIntPairParser

instance
  Bind
    ('TupleRep '[ 'IntRep, 'IntRep])
    ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])
  where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindInt2to5Parser
  (>>) = sequenceInt2to5Parser

instance
  Bind
    ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])
    LiftedRep
  where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindInt5Parser
  (>>) = sequenceInt5Parser

instance
  Bind
    'IntRep
    ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])
  where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindFromIntToInt5
  (>>) = sequenceIntToInt5

instance Bind LiftedRep ('TupleRep '[ 'IntRep, 'IntRep]) where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindFromLiftedToIntPair
  (>>) = sequenceLiftedToIntPair

instance
  Bind
    LiftedRep
    ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])
  where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindFromLiftedToInt5
  (>>) = sequenceLiftedToInt5

instance Bind 'IntRep ('TupleRep '[ 'IntRep, 'IntRep]) where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindFromIntToIntPair
  (>>) = sequenceIntToIntPair

instance Bind LiftedRep 'IntRep where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}
  (>>=) = bindFromLiftedToInt
  (>>) = sequenceLiftedToInt

instance Pure LiftedRep where
  {-# INLINE pure #-}
  pure = pureParser

instance Pure 'IntRep where
  {-# INLINE pure #-}
  pure = pureIntParser

instance Pure ('TupleRep '[ 'IntRep, 'IntRep]) where
  {-# INLINE pure #-}
  pure = pureIntPairParser

instance Pure ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]) where
  {-# INLINE pure #-}
  pure = pureInt5Parser

bindFromIntToIntPair ::
  forall
    s
    e
    (a :: TYPE 'IntRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])).
  Parser s e a ->
  (a -> Parser s e b) ->
  Parser s e b
{-# INLINE bindFromIntToIntPair #-}
bindFromIntToIntPair (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceIntToIntPair ::
  forall
    s
    e
    (a :: TYPE 'IntRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])).
  Parser s e a ->
  Parser s e b ->
  Parser s e b
{-# INLINE sequenceIntToIntPair #-}
sequenceIntToIntPair (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

bindFromIntToInt5 ::
  forall
    s
    e
    (a :: TYPE 'IntRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])).
  Parser s e a ->
  (a -> Parser s e b) ->
  Parser s e b
{-# INLINE bindFromIntToInt5 #-}
bindFromIntToInt5 (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceIntToInt5 ::
  forall
    s
    e
    (a :: TYPE 'IntRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])).
  Parser s e a ->
  Parser s e b ->
  Parser s e b
{-# INLINE sequenceIntToInt5 #-}
sequenceIntToInt5 (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

bindFromLiftedToIntPair ::
  forall
    s
    e
    (a :: TYPE LiftedRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])).
  Parser s e a ->
  (a -> Parser s e b) ->
  Parser s e b
{-# INLINE bindFromLiftedToIntPair #-}
bindFromLiftedToIntPair (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceLiftedToIntPair ::
  forall
    s
    e
    (a :: TYPE LiftedRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])).
  Parser s e a ->
  Parser s e b ->
  Parser s e b
{-# INLINE sequenceLiftedToIntPair #-}
sequenceLiftedToIntPair (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

bindFromLiftedToInt5 ::
  forall
    s
    e
    (a :: TYPE LiftedRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])).
  Parser s e a ->
  (a -> Parser s e b) ->
  Parser s e b
{-# INLINE bindFromLiftedToInt5 #-}
bindFromLiftedToInt5 (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceLiftedToInt5 ::
  forall
    s
    e
    (a :: TYPE LiftedRep)
    (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])).
  Parser s e a ->
  Parser s e b ->
  Parser s e b
{-# INLINE sequenceLiftedToInt5 #-}
sequenceLiftedToInt5 (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )

bindFromLiftedToInt ::
  forall
    s
    e
    (a :: TYPE LiftedRep)
    (b :: TYPE 'IntRep).
  Parser s e a ->
  (a -> Parser s e b) ->
  Parser s e b
{-# INLINE bindFromLiftedToInt #-}
bindFromLiftedToInt (Parser f) g =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# y, b, c #) #) ->
            runParser (g y) (# arr, b, c #) s1
    )

sequenceLiftedToInt ::
  forall
    s
    e
    (a :: TYPE LiftedRep)
    (b :: TYPE 'IntRep).
  Parser s e a ->
  Parser s e b ->
  Parser s e b
{-# INLINE sequenceLiftedToInt #-}
sequenceLiftedToInt (Parser f) (Parser g) =
  Parser
    ( \x@(# arr, _, _ #) s0 -> case f x s0 of
        (# s1, r0 #) -> case r0 of
          (# e | #) -> (# s1, (# e | #) #)
          (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
    )
