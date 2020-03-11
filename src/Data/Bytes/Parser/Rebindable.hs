{-# language FlexibleInstances #-}
{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeInType #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

-- | Provides levity-polymorphic variants of @>>=@, @>>@, and @pure@
-- used to assemble parsers whose result types are unlifted. This
-- cannot be used with the @RebindableSyntax@ extension because that
-- extension disallows representations other than @LiftedRep@. Consequently,
-- users of this module must manually desugar do notation. See the
-- @url-bytes@ library for an example of this module in action.
--
-- Only resort to the functions in this module after checking that
-- GHC is unable to optimize away @I#@ and friends in your code.
module Data.Bytes.Parser.Rebindable
  ( Bind(..)
  , Pure(..)
  ) where

import Prelude () 
import GHC.Exts (TYPE,RuntimeRep(..))
import Data.Bytes.Parser.Internal (Parser(..))

class Bind (ra :: RuntimeRep) (rb :: RuntimeRep) where
  (>>=) :: forall e s (a :: TYPE ra) (b :: TYPE rb).
    Parser e s a -> (a -> Parser e s b) -> Parser e s b
  (>>) :: forall e s (a :: TYPE ra) (b :: TYPE rb).
    Parser e s a -> Parser e s b -> Parser e s b

class Pure (ra :: RuntimeRep) where
  pure :: forall e s (a :: TYPE ra). a -> Parser e s a

pureParser :: a -> Parser e s a
{-# inline pureParser #-}
pureParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindParser :: Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# inline bindParser #-}
bindParser (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceParser :: Parser e s a -> Parser e s b -> Parser e s b
{-# inline sequenceParser #-}
sequenceParser (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

pureIntParser :: forall (a :: TYPE 'IntRep) e s.
  a -> Parser e s a
{-# inline pureIntParser #-}
pureIntParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindIntParser :: forall (a :: TYPE 'IntRep) e s b.
  Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# inline bindIntParser #-}
bindIntParser (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceIntParser :: forall (a :: TYPE 'IntRep) e s b.
  Parser e s a -> Parser e s b -> Parser e s b
{-# inline sequenceIntParser #-}
sequenceIntParser (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

pureIntPairParser :: forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) e s.
  a -> Parser e s a
{-# inline pureIntPairParser #-}
pureIntPairParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindIntPairParser :: forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) e s b.
  Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# inline bindIntPairParser #-}
bindIntPairParser (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

pureInt5Parser :: forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])) e s.
  a -> Parser e s a
{-# inline pureInt5Parser #-}
pureInt5Parser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

bindInt5Parser :: forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])) e s b.
  Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# inline bindInt5Parser #-}
bindInt5Parser (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceInt5Parser :: forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])) e s b.
  Parser e s a -> Parser e s b -> Parser e s b
{-# inline sequenceInt5Parser #-}
sequenceInt5Parser (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

sequenceIntPairParser :: forall (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) e s b.
  Parser e s a -> Parser e s b -> Parser e s b
{-# inline sequenceIntPairParser #-}
sequenceIntPairParser (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

bindInt2to5Parser :: forall 
  (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep])) 
  (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]))
  e s.
  Parser e s a -> (a -> Parser e s b) -> Parser e s b
{-# inline bindInt2to5Parser #-}
bindInt2to5Parser (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceInt2to5Parser :: forall 
  (a :: TYPE ('TupleRep '[ 'IntRep, 'IntRep]))
  (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]))
  e s.
  Parser e s a -> Parser e s b -> Parser e s b
{-# inline sequenceInt2to5Parser #-}
sequenceInt2to5Parser (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

instance Bind 'LiftedRep 'LiftedRep where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindParser
  (>>) = sequenceParser

instance Bind 'IntRep 'LiftedRep where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindIntParser
  (>>) = sequenceIntParser

instance Bind ('TupleRep '[ 'IntRep, 'IntRep]) 'LiftedRep where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindIntPairParser
  (>>) = sequenceIntPairParser


instance Bind ('TupleRep '[ 'IntRep, 'IntRep])
              ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]) 
  where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindInt2to5Parser
  (>>) = sequenceInt2to5Parser

instance Bind ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]) 
              'LiftedRep
  where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindInt5Parser
  (>>) = sequenceInt5Parser


instance Bind 'IntRep
              ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]) 
  where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindFromIntToInt5
  (>>) = sequenceIntToInt5

instance Bind 'LiftedRep ('TupleRep '[ 'IntRep, 'IntRep]) where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindFromLiftedToIntPair
  (>>) = sequenceLiftedToIntPair

instance Bind 'LiftedRep
              ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]) 
  where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindFromLiftedToInt5
  (>>) = sequenceLiftedToInt5

instance Bind 'IntRep ('TupleRep '[ 'IntRep, 'IntRep]) where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindFromIntToIntPair
  (>>) = sequenceIntToIntPair

instance Bind 'LiftedRep 'IntRep where
  {-# inline (>>=) #-}
  {-# inline (>>) #-}
  (>>=) = bindFromLiftedToInt
  (>>) = sequenceLiftedToInt

instance Pure 'LiftedRep where
  {-# inline pure #-}
  pure = pureParser

instance Pure 'IntRep where
  {-# inline pure #-}
  pure = pureIntParser

instance Pure ('TupleRep '[ 'IntRep, 'IntRep]) where
  {-# inline pure #-}
  pure = pureIntPairParser

instance Pure ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep]) where
  {-# inline pure #-}
  pure = pureInt5Parser

bindFromIntToIntPair ::
     forall s e
       (a :: TYPE 'IntRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep ])).
     Parser s e a
  -> (a -> Parser s e b)
  -> Parser s e b
{-# inline bindFromIntToIntPair #-}
bindFromIntToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceIntToIntPair ::
     forall s e
       (a :: TYPE 'IntRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep ])).
     Parser s e a
  -> Parser s e b
  -> Parser s e b
{-# inline sequenceIntToIntPair #-}
sequenceIntToIntPair (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

bindFromIntToInt5 ::
     forall s e
       (a :: TYPE 'IntRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep ])).
     Parser s e a
  -> (a -> Parser s e b)
  -> Parser s e b
{-# inline bindFromIntToInt5 #-}
bindFromIntToInt5 (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceIntToInt5 ::
     forall s e
       (a :: TYPE 'IntRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep ])).
     Parser s e a
  -> Parser s e b
  -> Parser s e b
{-# inline sequenceIntToInt5 #-}
sequenceIntToInt5 (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

bindFromLiftedToIntPair ::
     forall s e
       (a :: TYPE 'LiftedRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep ])).
     Parser s e a
  -> (a -> Parser s e b)
  -> Parser s e b
{-# inline bindFromLiftedToIntPair #-}
bindFromLiftedToIntPair (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceLiftedToIntPair ::
     forall s e
       (a :: TYPE 'LiftedRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep ])).
     Parser s e a
  -> Parser s e b
  -> Parser s e b
{-# inline sequenceLiftedToIntPair #-}
sequenceLiftedToIntPair (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )


bindFromLiftedToInt5 ::
     forall s e
       (a :: TYPE 'LiftedRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep])).
     Parser s e a
  -> (a -> Parser s e b)
  -> Parser s e b
{-# inline bindFromLiftedToInt5 #-}
bindFromLiftedToInt5 (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceLiftedToInt5 ::
     forall s e
       (a :: TYPE 'LiftedRep)
       (b :: TYPE ('TupleRep '[ 'IntRep, 'IntRep, 'IntRep, 'IntRep, 'IntRep ])).
     Parser s e a
  -> Parser s e b
  -> Parser s e b
{-# inline sequenceLiftedToInt5 #-}
sequenceLiftedToInt5 (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )

bindFromLiftedToInt ::
     forall s e
       (a :: TYPE 'LiftedRep)
       (b :: TYPE 'IntRep).
     Parser s e a
  -> (a -> Parser s e b)
  -> Parser s e b
{-# inline bindFromLiftedToInt #-}
bindFromLiftedToInt (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

sequenceLiftedToInt ::
     forall s e
       (a :: TYPE 'LiftedRep)
       (b :: TYPE 'IntRep).
     Parser s e a
  -> Parser s e b
  -> Parser s e b
{-# inline sequenceLiftedToInt #-}
sequenceLiftedToInt (Parser f) (Parser g) = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# _, b, c #) #) -> g (# arr, b, c #) s1
  )
