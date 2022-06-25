

module Data.Either.Class (
IsEither (..),
bitraverseEither, traverseEither, bimapEither, bindEither, altEither, manyEither, someEither, thenBackEither, liftCompare2Either, liftEq2Either
) where


import Prelude hiding (either, isLeft, isRight)


class IsEither p where
    either :: (a -> c) -> (b -> c) -> p a b -> c
    inL :: a -> p a b
    inR :: b -> p a b

isLeft, isRight :: (IsEither p)=> p a b -> Bool
isLeft = either (\_-> True) (\_-> False)
isRight = either (\_-> False) (\_-> True)

fromLeft :: (IsEither p)=> a -> p a b -> a
fromLeft z = either id (\_-> z)

fromRight :: (IsEither p)=> b -> p a b -> b
fromRight z = either (\_-> z) id


instance IsEither Either where
    either g f xy = case xy of
        Left x -> g x
        Right y -> f y

    inL = Left
    inR = Right


bitraverseEither ::
    (IsEither p, Functor m)=> (a -> m a') -> (b -> m b') -> p a b -> m (p a' b')
bitraverseEither g f = either (fmap inL . g) (fmap inR . f)
{-# INLINE bitraverseEither #-}

traverseEither ::
    (IsEither p, Applicative m)=> (a -> m a') -> p c a -> m (p c a')
traverseEither f = either (pure . inL) (fmap inR . f)
{-# INLINE traverseEither #-}

bimapEither :: (IsEither p)=> (a -> a') -> (b -> b') -> p a b -> p a' b'
bimapEither g f = either (inL . g) (inR . f)
{-# INLINABLE bimapEither #-}

bindEither :: (IsEither p)=> (a -> p c a') -> p c a -> p c a'
bindEither = either inL
{-# INLINABLE bindEither #-}

altEither :: (IsEither p, Semigroup c)=> p c a -> p c a -> p c a
altEither = altEitherWith (<>)
{-# INLINE altEither #-}

altEitherWith :: (IsEither p)=> (c -> c -> c) -> p c a -> p c a -> p c a
altEitherWith f mx my =
    f `seq` either (\ u -> either (inL . f u) (\_-> my) my) (\_-> mx) mx
{-# INLINABLE altEitherWith #-}

manyEither :: (IsEither p)=> p c a -> p c [a]
{-^ @inL e@ is changed to @inR []@; @inR x@ becomes @inR ('repeat' x)@.
-}
-- manyEither mx = someEither mx <|> pure []
manyEither = either (\_-> inR []) (inR . repeat)
{-# INLINABLE manyEither #-}

someEither :: (IsEither p)=> p c a -> p c [a]
{-^ @inL e@ is unaltered; @inR x@ becomes @inR ('repeat' x)@.
-}
-- someEither = fmap repeat
someEither = bindEither (inR . repeat)
{-# INLINABLE someEither #-}

thenBackEither :: (IsEither p)=> p c a -> p c b -> p c a
{-^ @thenBackEither@ is a suitable definition for 'Control.Applicative.<*'.
-}
-- thenBackEither mx my = either (\_-> mx) (\_-> either inL (\_-> mx) my) mx
thenBackEither mx my | isRight mx = bindEither (\_-> mx) my | otherwise = mx
-- thenBackEither mx my | null mx = mx | otherwise my >> mx
{-# INLINABLE thenBackEither #-}

liftCompare2Either :: (IsEither p)=>
    (a -> c -> Ordering) -> (b -> d -> Ordering) -> p a b -> p c d -> Ordering
{-^ Given two ordering functions, this compares lefts or rights, and considers all rights greater than all lefts. -}
liftCompare2Either c2 c1 = c2 `seq` c1 `seq` \ ux vy ->
    either (\ u -> either (c2 u) (\_-> LT) vy) (\ x -> either (\_-> GT) (c1 x) vy) ux
{-# INLINABLE liftCompare2Either #-}

liftEq2Either ::
    (IsEither p)=> (a -> c -> Bool) -> (b -> d -> Bool) -> p a b -> p c d -> Bool
liftEq2Either e2 e1 = e2 `seq` e1 `seq` \ ux vy ->
    either (\ u -> either (e2 u) (\_-> False) vy) (\ x -> either (\_-> False) (e1 x) vy) ux
