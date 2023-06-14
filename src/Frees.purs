module Frees where

import Prelude

import Control.Monad.Free (Free, hoistFree, liftF, resume)
import Data.Array (fold)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Coyoneda (Coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Either (Either(..))
import Data.Semigroup.Foldable (fold1)

-- 色々な Free を集める

-- Free Monoid
type FreeMonoid = Array

-- Free Monad
type FreeMonad = Free

-- Free Functor
type FreeFunctor = Coyoneda

type FreeSemigroup = NonEmptyArray

-- Free -| Forgetful

-- unit
unitMonoid :: forall a. a -> FreeMonoid a
unitMonoid = pure

unitMonad :: forall f. Functor f => f ~> FreeMonad f
unitMonad = liftF

unitFunctor :: forall s. s ~> FreeFunctor s
unitFunctor = liftCoyoneda

unitSemigroup :: forall a. a -> FreeSemigroup a
unitSemigroup = pure

-- counit
counitMonoid :: forall m. Monoid m => FreeMonoid m -> m
counitMonoid = fold

counitMonad :: forall m. Monad m => FreeMonad m ~> m
counitMonad free = case resume free of
  Right a -> pure a
  Left m -> m >>= counitMonad

counitFunctor :: forall f. Functor f => FreeFunctor f ~> f
counitFunctor = lowerCoyoneda

counitSemigroup :: forall s. Semigroup s => FreeSemigroup s -> s
counitSemigroup = fold1

-- As a Functor
mapMonoid :: forall a b. (a -> b) -> FreeMonoid a -> FreeMonoid b
mapMonoid = map

mapMonad :: forall f g. Functor f => Functor g => (f ~> g) -> FreeMonad f ~> FreeMonad g
mapMonad = hoistFree

mapFunctor :: forall s t. (s ~> t) -> FreeFunctor s ~> FreeFunctor t
mapFunctor = hoistCoyoneda

mapSemigroup :: forall a b. (a -> b) -> FreeSemigroup a -> FreeSemigroup b
mapSemigroup = map

-- right adjunct
rightAdjunctMonoid :: forall a m. Monoid m => (a -> m) -> FreeMonoid a -> m
rightAdjunctMonoid f = mapMonoid f >>> counitMonoid

rightAdjunctMonad :: forall f m. Functor f => Monad m => (f ~> m) -> FreeMonad f ~> m
rightAdjunctMonad f = mapMonad f >>> counitMonad

rightAdjunctFunctor :: forall s f. Functor f => (s ~> f) -> FreeFunctor s ~> f
rightAdjunctFunctor f = mapFunctor f >>> counitFunctor

rightAdjunctSemigroup :: forall a s. Semigroup s => (a -> s) -> FreeSemigroup a -> s
rightAdjunctSemigroup f = mapSemigroup f >>> counitSemigroup
