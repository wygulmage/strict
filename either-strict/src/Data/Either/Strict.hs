{-# LANGUAGE CPP
  #-}


module Data.Either.Strict (
Either'(..),
) where

import Control.DeepSeq

import Data.Either.Class

import Prelude hiding (either)
import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,10,0)
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
#endif
import Data.Foldable
import Data.Functor.Classes


data Either' a b = Right' !b | Left' !a


instance IsEither Either' where
    either g f xy = case xy of
        Right' y -> f y
        Left' x -> g x

    inL = Left'
    inR = Right'

#if MIN_VERSION_base(4,10,0)
instance Bitraversable Either' where
    bitraverse = bitraverseEither
    {-# INLINABLE bitraverse #-}

instance Bifoldable Either' where
    bifoldMap = either
    {-# INLINE bifoldMap #-}

instance Bifunctor Either' where
    bimap = bimapEither
    second = fmap
#endif

instance Traversable (Either' c) where
    traverse = traverseEither
    {-# INLINABLE traverse #-}

instance Foldable (Either' c) where
    foldMap = either mempty
    {-# INLINABLE foldMap #-}
#if MIN_VERSION_base(4,13,0)
    foldMap' = foldMap
    {-# INLINE foldMap' #-}
#endif

instance Monad (Either' c) where
    (>>=) = flip bindEither

instance (Monoid c)=> Alternative (Either' c) where
    empty = inL mempty
    {-# INLINABLE empty #-}
    (<|>) = altEither
    {-# INLINABLE (<|>) #-}
    many = manyEither
    some = someEither

instance Applicative (Either' c) where
    pure = inR
#if MIN_VERSION_base(4,10,0)
    liftA2 = liftM2
#else
    (<*>) = ap
#endif
    (*>) = (>>)
    (<*) = thenBackEither

instance Functor (Either' c) where
    fmap = liftM

instance Ord2 Either' where
    liftCompare2 = liftCompare2Either

instance (Ord c)=> Ord1 (Either' c) where
    liftCompare = liftCompare2 compare
    {-# INLINABLE liftCompare #-}

instance (Ord a, Ord b)=> Ord (Either' a b) where
    compare = compare1
    {-# INLINABLE compare #-}
    Right' x <= Right' y = x <= y
    Right'{} <= Left'{} = False
    Left'{} <= Right'{} = True
    Left' x <= Left' y = x <= y
    {-# INLINABLE (<=) #-}
    (>=) = flip (<=)
    {-# INLINE (>=) #-}
    ex > ey = not (ex <= ey)
    {-# INLINE (>) #-}
    (<) = flip (>)
    {-# INLINE (<) #-}

instance (Bounded low, Bounded high)=> Bounded (Either' low high) where
    maxBound = Right' maxBound
    {-# INLINABLE maxBound #-}
    minBound = Left' minBound
    {-# INLINABLE minBound #-}

instance Eq2 Either' where
    liftEq2 = liftEq2Either

instance (Eq c)=> Eq1 (Either' c) where
    liftEq = liftEq2 (==)
    {-# INLINABLE liftEq #-}

instance (Eq a, Eq b)=> Eq (Either' a b) where
    (==) = eq1
    {-# INLINABLE (==) #-}

instance NFData2 Either' where
    liftRnf2 = either

instance (NFData c)=> NFData1 (Either' c) where
    liftRnf = liftRnf2 rnf
    {-# INLINABLE liftRnf #-}

instance (NFData a, NFData b)=> NFData (Either' a b) where
    rnf = rnf1
    {-# INLINABLE rnf #-}
