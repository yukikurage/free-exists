module Data.Functor.Pairing.Com where

import Prelude

data T a = T (forall b. (T b -> a) -> b)

iso :: forall a b. (T b -> a) -> T a -> b
iso f (T g) = g f
