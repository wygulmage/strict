{-# LANGUAGE CPP
  #-}


module Data.Maybe.Strict (
Maybe'(..),
) where


import Control.DeepSeq

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Foldable
import Data.Functor.Classes
import Data.Semigroup


data Maybe' a = Just' !a | Nothing'

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' z f mx = case mx of{ Just' x -> f x ; Nothing' -> z }


instance Traversable Maybe' where
    traverse f = maybe' (pure empty) (fmap pure . f)
    {-# INLINABLE traverse #-}

instance Foldable Maybe' where
    foldMap = maybe' mempty
    {-# INLINABLE foldMap #-}
#if MIN_VERSION_base(4,13,0)
    foldMap' = foldMap
    {-# INLINABLE foldMap' #-}
#endif
    foldl' = foldl
    foldr' = foldr

instance Fail.MonadFail Maybe' where
    fail _ = mzero
    {-# INLINE fail #-}

instance MonadPlus Maybe' where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Monad Maybe' where
    (>>=) = flip (maybe' empty)

instance Alternative Maybe' where
{-^ @forall f. empty >>= f = empty@
@forall x my. pure x <|> my = pure x@
@forall mf mg mx. (mf <|> mg) <*> mx = (mf <*> mx) <|> (mg <*> mf)@
-}
    empty = Nothing'
    mx <|> my | null mx = my | otherwise = mx
    many = maybe' (pure empty) (pure . repeat)
    some = fmap repeat

instance Applicative Maybe' where
    pure = Just'
#if MIN_VERSION_base(4,10,0)
    liftA2 = liftM2
#else
    (<*>) = ap
#endif
    (*>) = (>>)
    mx <* my | null mx || null my = empty | otherwise = mx

instance Functor Maybe' where
    fmap = liftM

instance Ord1 Maybe' where
    liftCompare c = c `seq` go
      where
        go (Just' x) (Just' y) = c x y
        go Just'{} Nothing' = GT
        go Nothing' Just'{} = LT
        go Nothing' Nothing' = EQ

instance (Ord a)=> Ord (Maybe' a) where
    compare = compare1
    {-# INLINABLE compare #-}
    Just' x <= Just' y = x <= y
    Just'{} <= Nothing' = False
    _ <= _ = True
    {-# INLINABLE (<=) #-}
    (>=) = flip (<=)
    {-# INLINE (>=) #-}
    mx > my = not (mx <= my)
    {-# INLINE (>) #-}
    (<) = flip (>)
    {-# INLINE (<) #-}

instance (Bounded a)=> Bounded (Maybe' a) where
    maxBound = Just' maxBound
    {-# INLINABLE maxBound #-}
    minBound = Nothing'
    {-# INLINABLE minBound #-}

instance Eq1 Maybe' where
    liftEq eq = eq `seq` go
      where
        go (Just' x) (Just' y) = eq x y
        go Nothing' Nothing' = True
        go _ _ = False

instance (Eq a)=> Eq (Maybe' a) where
    (==) = eq1
    {-# INLINABLE (==) #-}

instance (Semigroup a)=> Semigroup (Maybe' a) where
    Just' x <> Just' y = Just' (x <> y)
    mx@Just'{} <> Nothing' = mx
    Nothing'{} <> my = my
    {-# INLINABLE (<>) #-}
    stimes n = fmap (stimes n)
    {-# INLINABLE stimes #-}

instance (Semigroup a)=> Monoid (Maybe' a) where
    mempty = Nothing'


instance NFData1 Maybe' where
    liftRnf = foldMap

instance (NFData a)=> NFData (Maybe' a) where
    rnf = rnf1
    {-# INLINEABLE rnf #-}
