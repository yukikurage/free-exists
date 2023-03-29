module Contravariant where

import Prelude

import Control.Monad.ST (run)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Effect (Effect)
import QualifiedDo.Semigroup as Semigroup

data F2 f a = F2 (f (f a))

instance Contravariant f => Functor (F2 f) where
  map f (F2 x) = F2 (cmap (cmap f) x)

runF2 :: forall f a b. Contravariant f => F2 f a -> (b -> f a) -> f b
runF2 (F2 x) f = cmap f x

class Hoge a where
  hoge :: a
  fuga :: a

defaultFuga = hoge

z = run (pure unit)

newtype Phantom a = Phantom Int

x :: forall a. Phantom a
x = Phantom 1

f :: (forall a. Phantom a) -> Int
f (Phantom x) = x

hello :: String
hello = Semigroup.do
  "Hello"
  " "
  "World"
  "!"
