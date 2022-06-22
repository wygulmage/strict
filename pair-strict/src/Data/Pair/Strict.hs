{-# LANGUAGE NoImplicitPrelude #-}

module Data.Pair.Strict (
Pair'(..),
) where


import Data.Pair.Class

import Control.DeepSeq

import Prelude (Read, Show, Eq(..), seq)
import Control.Applicative
import Control.Monad
import Data.Bitraversable
import Data.Traversable
import Data.Bifoldable
import Data.Foldable
import Data.Bifunctor
import Data.Functor.Classes
import Data.Ord
import Data.Bool
import Data.Monoid
import Data.Semigroup


data Pair' a b = (:!:) !a !b
  deriving (Read, Show)


instance IsPair Pair' where
    pair = (:!:)
    uncurry' f = f `seq` \ (x :!: y) -> f x y


instance Bitraversable Pair' where
    bitraverse = bifoldMapPairWith $ liftA2 pair

instance Traversable (Pair' c) where
   traverse = bifoldMapPairWith fmap pair

instance Bifoldable Pair' where
    bifoldMap = bifoldMapPairWith (<>)

instance Foldable (Pair' c) where
    foldMap = foldMapPair
    -- Definitions below are unnecessary but simplify things:
    foldMap' = foldMap
    {-# INLINE foldMap' #-}
    foldr' = foldr
    {-# INLINE foldr' #-}
    foldl' = foldl
    {-# INLINE foldl' #-}
    fold = snd
    {-# INLINE fold #-}
    foldr1 _ = snd
    foldl1 _ = snd
    maximum = snd
    {-# INLINE maximum #-}
    minimum = snd
    {-# INLINE minimum #-}
    product = snd
    {-# INLINE product #-}
    sum = snd
    {-# INLINE sum #-}

instance Bifunctor Pair' where
    bimap = bimapPair
    second = fmap

instance Functor (Pair' c) where
    fmap = fmapPair

instance (Monoid c)=> Applicative (Pair' c) where
    pure = pair mempty
    liftA2 = biliftPair2 (<>)

instance (Monoid c)=> Monad (Pair' c) where
   (>>=) = flip $ bindPairWith (<>)
   (>>) = (*>)

instance (Semigroup a, Semigroup b)=> Semigroup (Pair' a b) where
    (<>) = biliftPair2 (<>) (<>)

instance (Monoid a, Monoid b)=> Monoid (Pair' a b) where
    mempty = pair mempty mempty

instance Ord2 Pair' where
    liftCompare2 c1 c2 = c1 `seq` c2 `seq` go
      where go (u :!: x) (v :!: y) = c1 u v <> c2 x y

instance (Ord c)=> Ord1 (Pair' c) where
    liftCompare = liftCompare2 compare

instance (Ord a, Ord b)=> Ord (Pair' a b) where
{-^ The @Ord@ instance of @Pair' a b@ compares the @a@ arguments first, and if they are equal compares the @b@ arguments.
-}
    compare = compare1

    -- Just in case <= is more efficient for type 'b', define <= manually.
    (u :!: x) <= (v :!: y) = case compare u v of
        GT -> False
        EQ -> x <= y
        LT -> True

    -- 'compare' forces 'Ord' to define a total order, so we can flip things around like this without worrying about an incorrect definition:
    (>=) = flip (<=)
    ux > vy = not $ ux <= vy -- default: compare ux vy == GT
    (<) = flip (>) -- default: compare vy ux == LT


instance Eq2 Pair' where
    liftEq2 c1 c2 = c1 `seq` c2 `seq` go
      where go (u :!: x) (v :!: y) = c1 u v && c2 x y

instance (Eq c)=> Eq1 (Pair' c) where
    liftEq = liftEq2 (==)

instance (Eq a, Eq b)=> Eq (Pair' a b) where
    (==) = eq1


instance NFData2 Pair' where
    liftRnf2 = bifoldMapPairWith seq

instance (NFData c)=> NFData1 (Pair' c) where
    liftRnf = liftRnf2 rnf

instance (NFData a, NFData b)=> NFData (Pair' a b) where
    rnf = rnf1


{-# SPECIALIZE fst :: Pair' a b -> a #-}
{-# SPECIALIZE snd :: Pair' a b -> b #-}
{-# SPECIALIZE curry :: (Pair' a b -> c) -> a -> b -> c #-}
{-# SPECIALIZE uncurry :: (a -> b -> c) -> Pair' a b -> c #-}


infixr 0 $
($) :: a -> a
($) x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f = f `seq` \ x y -> f y x
{-# INLINABLE flip #-}
