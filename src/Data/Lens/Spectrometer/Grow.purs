module Data.Lens.Spectrometer.Grow
  ( Grow(..)
  ) where

import Prelude

import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.Functor.Representable (class Representable, Key, tabulate)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldlDefault, foldrDefault, sequenceDefault, traverse)

data Grow f a = Grow a (f a)

cons :: forall f a. a -> f a -> Grow f a
cons = Grow

head :: forall f a. Grow f a -> a
head (Grow a _) = a

tail :: forall f a. Grow f a -> f a
tail (Grow _ fa) = fa

instance Functor f => Functor (Grow f) where
  map f (Grow a fa) = Grow (f a) $ map f fa

instance Distributive f => Distributive (Grow f) where
  distribute :: forall g a. Functor g => g (Grow f a) -> Grow f (g a)
  distribute gga = ga `cons` fga
    where
    ga = map head gga
    gfa = map tail gga
    fga = distribute gfa
  collect = collectDefault

instance Representable f => Representable (Grow f) where
  tabulate :: forall a. (Key (Grow f) -> a) -> Grow f a
  tabulate dict = a `cons` fa
    where
    a = dict head
    fa = tabulate dictF
    dictF keyF = dict \grow -> keyF $ tail grow

instance Foldable f => Foldable (Grow f) where
  foldMap f (Grow a fa) = f a <> foldMap f fa
  foldr = foldrDefault
  foldl = foldlDefault

instance Traversable f => Traversable (Grow f) where
  traverse :: forall g a b. Applicative g => (a -> g b) -> Grow f a -> g (Grow f b)
  traverse f (Grow a fa) = cons <$> f a <*> traverse f fa
  sequence = sequenceDefault
