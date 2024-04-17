module Cat.End where

import Prelude

import Data.Profunctor.Star (Star)

newtype End f g = End (f ~> g)

-- | Coend f g = exists x. f x -> g x
newtype Coend f g = Coend (forall r. ((f ~> g) -> r) -> r)

-- 何に使えるんだ……？

-- 一般化

newtype EndP p f g = EndP (forall x. p (f x) (g x))

-- EndP Kleisli

type EndK m f g = EndP (Star m) f g

-- forall x. f x -> m (g x)

-- う～ん
