

module Data.Maybe.Strict (
Maybe'(..),
) where


import Control.DeepSeq

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor.Classes
import Data.Semigroup


data Maybe' a = Just' !a | Nothing'

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' z f mx = case mx of{ Just' x -> f x ; Nothing' -> z }


instance Traversable Maybe' where
    traverse f = maybe' (pure empty) (fmap pure . f)

instance Foldable Maybe' where
    foldMap = maybe' mempty
    foldMap' = foldMap
    foldl' = foldl
    foldr' = foldr

instance Monad Maybe' where
    (>>=) = flip (maybe' empty)

instance Alternative Maybe' where
    empty = Nothing'
    mx <|> my | null mx = my | otherwise = mx
    many = maybe' (pure empty) (pure . repeat)
    some = fmap repeat

instance Applicative Maybe' where
    pure = Just'
    liftA2 = liftM2
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

    Just' x <= Just' y = x <= y
    Just'{} <= Nothing' = False
    _ <= _ = True

instance Eq1 Maybe' where
    liftEq eq = eq `seq` go
      where
        go (Just' x) (Just' y) = eq x y
        go Nothing' Nothing' = True
        go _ _ = False

instance (Eq a)=> Eq (Maybe' a) where
    (==) = eq1

instance (Semigroup a)=> Semigroup (Maybe' a) where
    Just' x <> Just' y = Just' (x <> y)
    mx@Just'{} <> Nothing' = mx
    Nothing'{} <> my = my

instance (Monoid a)=> Monoid (Maybe' a) where
    mempty = pure mempty


instance NFData1 Maybe' where
    liftRnf = foldMap

instance (NFData a)=> NFData (Maybe' a) where
    rnf = rnf1
