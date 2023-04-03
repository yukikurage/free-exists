module Data.Functor.Pairing.Com where

import Prelude

data T a = T (forall b. (T b -> a) -> b)
