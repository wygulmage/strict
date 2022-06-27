{-# LANGUAGE CPP
           , NoImplicitPrelude
  #-}

module Data.Pair.Strict (
Pair'(..),
) where


import Data.Pair.Class

import Control.DeepSeq

import Prelude (Bounded, Read, Show (..), Eq(..), (+), seq, showParen, showString)
import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,10,0)
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
#endif
import Data.Traversable
import Data.Foldable
import Data.Functor.Classes
import Data.Ord
import Data.Bool
import Data.Monoid
import Data.Semigroup


infix 1 :!:
data Pair' a b = (:!:) !a !b
  deriving (Bounded, Read)


instance IsPair Pair' where
    pair = (:!:)
    uncurry' f = f `seq` \ (x :!: y) -> f x y

#if MIN_VERSION_base(4,10,0)
instance Bitraversable Pair' where
    bitraverse = bifoldMapPairWith $ liftA2 pair
    {-# INLINABLE bitraverse #-}

instance Bifoldable Pair' where
    bifoldMap = bifoldMapPairWith (<>)
    {-# INLINABLE bifoldMap #-}

instance Bifunctor Pair' where
    bimap = bimapPair
    second = fmap
#endif

instance Traversable (Pair' c) where
    traverse = bifoldMapPairWith fmap pair
    {-# INLINABLE traverse #-}

instance Foldable (Pair' c) where
    foldMap = foldMapPair
    {-# INLINABLE foldMap #-}
    -- Definitions below are unnecessary but simplify things:
#if MIN_VERSION_base(4,13,0)
    foldMap' = foldMap
    {-# INLINE foldMap' #-}
#endif
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

instance Functor (Pair' c) where
    fmap = fmapPair

instance (Monoid c)=> Applicative (Pair' c) where
    pure = pair mempty
    {-# INLINABLE pure #-}
#if MIN_VERSION_base(4,10,0)
    liftA2 = biliftPair2 (<>)
    {-# INLINABLE liftA2 #-}
#else
    (<*>) = biliftPair2 (<>) ($)
    {-# INLINABLE (<*>) #-}
#endif

instance (Monoid c)=> Monad (Pair' c) where
   (>>=) = flip $ bindPairWith (<>)
   {-# INLINABLE (>>=) #-}
   (>>) = (*>)
   {-# INLINE (>>) #-}

instance (Semigroup a, Semigroup b)=> Semigroup (Pair' a b) where
    (<>) = biliftPair2 (<>) (<>)
    {-# INLINABLE (<>) #-}

instance (Monoid a, Monoid b)=> Monoid (Pair' a b) where
    mempty = pair mempty mempty
    {-# INLINABLE mempty #-}

instance Ord2 Pair' where
    liftCompare2 c1 c2 = c1 `seq` c2 `seq` go
      where go (u :!: x) (v :!: y) = c1 u v <> c2 x y

instance (Ord c)=> Ord1 (Pair' c) where
    liftCompare = liftCompare2 compare
    {-# INLINE liftCompare #-}

instance (Ord a, Ord b)=> Ord (Pair' a b) where
{-^ The @Ord@ instance of @Pair' a b@ compares the @a@ arguments first, and if they are equal compares the @b@ arguments.
-}
    compare = compare1
    {-# INLINABLE compare #-}
    -- Just in case <= is more efficient for type 'b', define <= manually.
    (u :!: x) <= (v :!: y) = case compare u v of
        GT -> False
        EQ -> x <= y
        LT -> True
    {-# INLINABLE (<=) #-}
    -- 'compare' forces 'Ord' to define a total order, so we can flip things around like this without worrying about an incorrect definition:
    (>=) = flip (<=)
    {-# INLINE (>=) #-}
    ux > vy = not $ ux <= vy -- default: compare ux vy == GT
    {-# INLINE (>) #-}
    (<) = flip (>) -- default: compare vy ux == LT
    {-# INLINE (<) #-}

instance Eq2 Pair' where
    liftEq2 c1 c2 = c1 `seq` c2 `seq` go
      where go (u :!: x) (v :!: y) = c1 u v && c2 x y

instance (Eq c)=> Eq1 (Pair' c) where
    liftEq = liftEq2 (==)
    {-# INLINABLE liftEq #-}

instance (Eq a, Eq b)=> Eq (Pair' a b) where
    (==) = eq1
    {-# INLINABLE (==) #-}

instance Show2 Pair' where
    liftShowsPrec2 s1 _ s2 _ d (x :!: y) = s1 `seq` s2 `seq`
        showParen (d > pairPrec) $
            s1 (pairPrec + 1) x . showString " :!: " . s2 (pairPrec + 1) y
      where
        pairPrec = 1
    {-# NOTINLINE liftShowsPrec2 #-}

instance (Show c)=> Show1 (Pair' c) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show b)=> Show (Pair' a b) where
    showsPrec = showsPrec1


instance NFData2 Pair' where
    liftRnf2 = bifoldMapPairWith seq

instance (NFData c)=> NFData1 (Pair' c) where
    liftRnf = liftRnf2 rnf
    {-# INLINABLE liftRnf #-}

instance (NFData a, NFData b)=> NFData (Pair' a b) where
    rnf = rnf1
    {-# INLINABLE rnf #-}


{-# SPECIALIZE fst :: Pair' a b -> a #-}
{-# SPECIALIZE snd :: Pair' a b -> b #-}
{-# SPECIALIZE curry :: (Pair' a b -> c) -> a -> b -> c #-}
{-# SPECIALIZE uncurry :: (a -> b -> c) -> Pair' a b -> c #-}


infixr 0 $
($) :: a -> a
($) x = x

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f = f `seq` \ g x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f = f `seq` \ x y -> f y x
{-# INLINABLE flip #-}
