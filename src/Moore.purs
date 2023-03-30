module Moore where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend, extend)
import Data.Functor.Pairing (type (⋈), zap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

data Moore s a = Moore a (s -> Moore s a)

derive instance Functor (Moore s)
instance Extend (Moore s) where
  extend f (Moore a g) = Moore (f (Moore a g)) (extend f <<< g)

instance Comonad (Moore s) where
  extract (Moore a _) = a

unfoldMoore :: forall s a b. (b -> Tuple a (s -> b)) -> b -> Moore s a
unfoldMoore f b =
  let
    Tuple a g = f b
  in
    Moore a (unfoldMoore f <<< g)

iterateMoore :: forall s a. (a -> s -> a) -> a -> Moore s a
iterateMoore f a = unfoldMoore (\a' -> Tuple a' (f a')) a

logMoore :: forall a. Monoid a => Moore a a
logMoore = iterateMoore append mempty

data Leaf s a = Leaf a | Branch s (Leaf s a)

derive instance Functor (Leaf s)
instance Apply (Leaf s) where
  apply (Leaf f) l = map f l
  apply l (Leaf a) = map (_ $ a) l
  apply (Branch s nextF) l = Branch s $ apply nextF l

instance Applicative (Leaf s) where
  pure = Leaf

instance Bind (Leaf s) where
  bind (Leaf a) f = f a
  bind (Branch s next) f = Branch s $ bind next f

-- f ⋈ g ~ forall a b c. (a -> b -> c) -> f a -> g b -> c

leafMoorePair :: forall s. Leaf s ⋈ Moore s
leafMoorePair f (Leaf a) (Moore b _) = f a b
leafMoorePair f (Branch s nextL) (Moore _ nextM) = leafMoorePair f nextL (nextM s)

branch :: forall s. s -> Leaf s Unit
branch s = Branch s (Leaf unit)

program :: Leaf Int (Int -> Int)
program = do
  branch 1
  branch 3
  branch 2
  pure $ identity

main :: Effect Unit
main = do
  logShow $ zap leafMoorePair program $ iterateMoore (+) 0
