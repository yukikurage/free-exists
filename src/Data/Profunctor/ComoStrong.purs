module Data.Profunctor.ComoStrong where

import Prelude

import Control.Comonad (class Comonad, extend, extract)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, first)
import Data.String (length)
import Data.Tuple (Tuple(..), swap)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

class Profunctor p <= ComoStrong p where
  strong :: forall f a b. Comonad f => p a b -> p (f a) (f b)

firstToStrong :: forall p f a b. Strong p => Comonad f => p a b -> p (f a) (f b)
firstToStrong = dimap left right <<< first
  where
  left :: f a -> Tuple a (f a)
  left fa = Tuple (extract fa) (identity fa)

  right :: Tuple b (f a) -> f b
  right (Tuple b fa) = extend (const b) fa

strongToFirst :: forall p a b c. ComoStrong p => p a b -> p (Tuple a c) (Tuple b c)
strongToFirst = dimap swap swap <<< strong

class C a b | a -> b where
  c :: a -> b

instance C Int Int where
  c = identity

test x = case c x of _ -> c x <> ""

main :: Effect Unit
main = logShow $ test 0
x
