module FinArray where

import Prelude

data FArray a = FArray (forall x. x -> (x -> x -> x) -> (a -> x) -> x)

instance Functor FArray where
  map f (FArray g) = FArray (\z c h -> g z c (h <<< f))

instance Semigroup (FArray a) where
  append (FArray f) (FArray g) = FArray (\z c h -> c (f z c h) (g z c h))

instance Monoid (FArray a) where
  mempty = FArray (\z _ _ -> z)

{-

(f <> g) <> h ~ f <> (g <> h) ?

(f <> g) <> h
  ~ (\z c h -> c (f z c h) (g z c h)) <> h
  ~ (\z c h -> c (c (f z c h) (g z c h)) (h z c h))

f <> (g <> h)
  ~ (\z c h -> c (f z c h) (c (g z c h) (h z c h)))
-}
