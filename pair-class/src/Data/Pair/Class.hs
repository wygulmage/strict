-- -fpedantic-bottoms is needed to ensure η equivalence.
{-# OPTIONS_GHC
   -fpedantic-bottoms
  #-}

-- | The purpose of this module is to provide a consistent interface for pair types, i.e. the lazy pair @(,)@, strict pairs like @data x :!: y = (:!:) !x !y@,partially strict pairs & Church-encoded pairs like @newtype PairC a b = PairC (forall r. (a -> b -> r) -> r)@.

module Data.Pair.Class (
IsPair (..), fst, snd, curry, uncurry, swap,
bifoldMapPairWith, foldMapPair, bimapPair, fmapPair, biliftPair2, bindPairWith,
) where


import Prelude hiding (($), fst, snd, curry, uncurry)


class IsPair p where
{-^ Laws:
@forall x. 'fst' ('pair' x ()) = x@
@forall y. 'snd' ('pair' () y) = y@
@'uncurry'' '.' 'pair' = 'id'@
@'uncurry'' 'undefined' = 'undefined'@
-}
    pair :: a -> b -> p a b
    {-^ Construct a pair from 2 values.
    -}
    uncurry' :: (a -> b -> c) -> p a b -> c
    {-^ Apply a function to both values of a pair. Because of the @IsPair@ laws, @uncurry'@ must evaluate the pair and the function strictly: @uncurry' (\ _ _ -> ()) 'undefined' = 'undefined'@
    For example, for @(,)@ @uncurry'@ is defined as @uncurry' f = f `seq` \ (x, y) -> f x y@.
    -}

fst :: (IsPair p)=> p a b -> a
{-^ Get the 1st value of a pair. -}
fst = uncurry' $ \ x _ -> x
{-# INLINE fst #-}
{-# SPECIALIZE fst :: (a, b) -> a #-}

snd :: (IsPair p)=> p a b -> b
{-^ Get the 2nd value of a pair. -}
snd = uncurry' $ \ _ y -> y
{-# INLINE snd #-}
{-# SPECIALIZE snd :: (a, b) -> b #-}

curry :: (IsPair p)=> (p a b -> c) -> a -> b -> c
{-^ Apply a function of a pair to each component individually.
@curry@ preserves η equivalence: @curry 'undefined'@ = 'undefined'.
-}
curry f = f `seq` \ x y -> f $ pair x y
{-# INLINE curry #-}
{-# SPECIALIZE curry :: ((a, b) -> c) -> a -> b -> c#-}

uncurry :: (IsPair p)=> (a -> b -> c) -> p a b -> c
{-^ Apply a function to both values of a pair. @uncurry@ gets the values lazily and the function strictly:
@uncurry (\ _ _ -> ()) 'undefined' = ()@
@uncurry@ preserves η equivalence: @uncurry 'undefined'@ = 'undefined'.
-}
uncurry f = f `seq` \ xy -> fst xy `f` snd xy
{-# INLINE uncurry #-}
{-# SPECIALIZE uncurry :: (a -> b -> c) -> (a, b) -> c #-}

swap :: (IsPair p)=> p a b -> p b a
{-^ The last shall be first, and the first last.
@swap 'undefined'@ = 'undefined'.
-}
swap = uncurry' $ \ x y -> pair y x
{-# INLINE swap #-}
{-# SPECIALIZE swap :: (a, b) -> (b, a) #-}


instance IsPair (,) where
    pair = (,)
    uncurry' f =  f `seq` \ (x, y) -> f x y


-- There's little point to SPECIALIZE-ing the derived methods below to (,), since all the relevant instances are already defined in base.

bifoldMapPairWith ::
    (IsPair p)=> (u -> v -> w) -> (a -> u) -> (b -> v) -> p a b -> w
{-^ @bifoldMapPairWith (<>)@ is a suitable definition for 'Data.Bifoldable.bifoldMap'.
@bifoldMapPairWith 'pair'@ is a suitable definition for 'Data.Bifunctor.bimap'.
@bifoldMapPairWith ('liftA2' 'pair')@ is a suitable definition for 'Data.Bitraversable.bitraverse'.
@bifoldMapPairWith 'fmap' 'pair'@ is a suitable definition for 'Data.Traversable.traverse'.
@bifoldMapPairWith 'undefined'@ = 'undefined'.
-}
bifoldMapPairWith f = f `seq` \ g h -> uncurry' $ \ x y -> g x `f` h y
{-# INLINE bifoldMapPairWith #-}

foldMapPair :: (IsPair p)=> (a -> b) -> p c a -> b
{-^ @foldMapPair@ is a suitable definition for @foldMap@.
@foldMapPair 'undefined'@ = 'undefined'.

The definition @\ f -> uncurry' (\ _ x -> f x)@, rather than the lazier @\ f -> f . snd@, is required by the 'Traversable' law @'foldMap' = 'foldMapDefault'@,
because of 'Traversable' law @'fmap' = 'fmapDefault'@, because of the 'Functor' law @'fmap' 'id' = 'id'@.
-}
foldMapPair f = f `seq` uncurry' (\ _ x -> f x)
{-# INLINE foldMapPair #-}

bimapPair :: (IsPair p)=> (a -> c) -> (b -> d) -> p a b -> p c d
bimapPair = bifoldMapPairWith pair
{-# INLINE bimapPair #-}

fmapPair :: (IsPair p)=> (a -> b) -> p c a -> p c b
{-^ @fmapPair@ is a suitable definition for @fmap@.
The definition @\ f -> 'uncurry'' (\ u x -> u `pair` f x)@, rather than the lazier @\ f xy -> fst xy `pair` f (snd xy)@, is required by the functor law @'fmap' 'id' = 'id'@.
-}
fmapPair f = uncurry' $ \ u x -> u `pair` f x
{-# INLINE fmapPair #-}

biliftPair2 ::
    (IsPair p)=> (u -> v -> w) -> (a -> b -> c) -> p u a -> p v b -> p w c
{-^ @biliftA2 (<>)@ is a suitable definition for @liftA2@. -}
biliftPair2 f g ux vy =
    uncurry' (\ u x -> uncurry' (\ v y -> f u v `pair` g x y) vy) ux
{-# INLINE biliftPair2 #-}

bindPairWith ::
    (IsPair p)=> (u -> v -> w) -> (a -> p v b) -> p u a -> p w b
{-^ @bindWith (<>) f ux@ is a suitable definition for @ux >>= f@.
@bindPairWith@ preserves η equivalence with its second argument: @bindPairWith f 'undefined'@ = 'undefined'.
@bindPairWith@ strictly evaluates its first argument: @bindPairWith 'undefined' f@ = 'undefined' -- but this is optimization, not semantic.
-}
bindPairWith f g =
    f `seq` g `seq` uncurry' (\ u x -> uncurry' (\ v y -> f u v `pair` y) (g x))
{-# INLINE bindPairWith #-}

bifoldPair2With ::
    (IsPair p)=> (r1 -> r2 -> r) -> (a -> b -> r1) -> (c -> d -> r2) -> p a c -> p b d -> r
bifoldPair2With f = f `seq`
    \ g h wx yz -> uncurry' (\ w x -> uncurry' (\ y z -> g w y `f` h x z) yz) wx
{-# INLINE bifoldPair2With #-}

{- Note: Preserving η equivalence
If simpfied subsumption has taught anything, it's that η equivalence is very important to language semantics, and not to be exchanged for "a minor improvement in programming convenience". Therefore, this module strives to preserve η equivalence. When the result of a method will depend on the definedness of an argument, the argument is forced as soon as it is acquired.
-}

{- Note: Dictionary Unpacking
;
-}

infixr 0 $
($) :: a -> a
{-^ Redefined locally to preserve η equivalence. NOT EXPORTED -}
($) f = f
