{-# LANGUAGE PatternSynonyms
           , ViewPatterns
  #-}


module Data.Pair.Pattern (
) where


import Data.Pair.Class


pattern (:&) :: (IsPair p)=> a -> b -> p a b
pattern x :& y <- (uncurry' (,) -> (x, y))
  where x :& y = pair x y
