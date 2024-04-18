module Cat.Lim where

import Prelude

import Data.Maybe (Maybe(..))

newtype Lim :: forall k. (k -> Type) -> Type
newtype Lim f = Lim (forall x. f x)

unlim :: forall f x. Lim f -> f x
unlim (Lim x) = x

mediate :: forall f x. (forall y. x -> f y) -> x -> Lim f
mediate f x = Lim (f x)

limMaybe :: Lim Maybe
limMaybe = Lim Nothing

-- | Colim = exists x. f x
newtype Colim :: forall k. (k -> Type) -> Type
newtype Colim f = Colim (forall r. (forall x. f x -> r) -> r)

colim :: forall f x. f x -> Colim f
colim x = Colim \f -> f x

comediate :: forall f x. (forall y. f y -> x) -> Colim f -> x
comediate f (Colim g) = g f

colimMaybe :: Colim Maybe
colimMaybe = colim $ Just 0
