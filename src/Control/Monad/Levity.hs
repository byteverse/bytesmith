{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeInType #-}

module Control.Monad.Levity
  ( Monad(..)
  ) where

import Prelude () 
import Data.Kind (Type)
import GHC.Exts (TYPE,RuntimeRep)

class Monad (f :: forall (r :: RuntimeRep). Type -> Type -> TYPE r -> Type) (ra :: RuntimeRep) where
  pure :: forall e s (a :: TYPE ra). a -> f e s a
  (>>=) :: forall e s (a :: TYPE ra) (b :: Type). f e s a -> (a -> f e s b) -> f e s b
  (>>) :: forall e s (a :: TYPE ra) (b :: Type). f e s a -> f e s b -> f e s b
