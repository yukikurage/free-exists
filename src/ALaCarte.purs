module ALaCarte where

import Prelude

import Control.Monad.Free (Free, liftF, runFree, runFreeM)
import Data.Maybe (Maybe(..))

data Coproduct :: forall k. (k -> Type) -> (k -> Type) -> k -> Type
data Coproduct f g a = Inl (f a) | Inr (g a)

infixr 6 type Coproduct as :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  map f (Inl x) = Inl (map f x)
  map f (Inr x) = Inr (map f x)

class (Functor sub, Functor sup) <= Extends sub sup where
  inj :: forall a. sub a -> sup a
  prj :: forall a. sup a -> Maybe (sub a)

instance Functor f => Extends f f where
  inj = identity
  prj = Just

else instance (Functor f, Functor g) => Extends f (f :+: g) where
  inj = Inl
  prj (Inl x) = Just x
  prj (Inr _) = Nothing

else instance (Functor f, Functor g, Extends f h) => Extends f (g :+: h) where
  inj = Inr <<< inj
  prj (Inl _) = Nothing
  prj (Inr x) = prj x

newtype VoidF :: Type -> Type
newtype VoidF a = VoidF Void

derive instance Functor VoidF

evalFin :: forall a. Free VoidF a -> a
evalFin fr = runFree (\(VoidF v) -> absurd v) fr

data Add a = Add Int Int (Int -> a)

derive instance Functor Add

add :: forall f. (Extends Add f) => Free f Int -> Free f Int -> Free f Int
add frx fry = do
  x <- frx
  y <- fry
  liftF $ inj (Add x y identity)

interpretAdd
  :: forall f a
   . Functor f
  => (Add :+: f) (Free (Add :+: f) a)
  -> Free f (Free (Add :+: f) a)
interpretAdd (Inl (Add x y f)) = pure $ f $ x + y
interpretAdd (Inr f) = liftF f

evalAdd :: forall f a. Functor f => Free (Add :+: f) a -> Free f a
evalAdd fr = runFreeM interpretAdd fr

data Mul a = Mul Int Int (Int -> a)

derive instance Functor Mul

mul :: forall f. (Extends Mul f) => Free f Int -> Free f Int -> Free f Int
mul frx fry = do
  x <- frx
  y <- fry
  liftF $ inj (Mul x y identity)

interpretMul
  :: forall f a
   . Functor f
  => (Mul :+: f) (Free (Mul :+: f) a)
  -> Free f (Free (Mul :+: f) a)
interpretMul (Inl (Mul x y f)) = pure $ f $ x * y
interpretMul (Inr f) = liftF f

evalMul :: forall f a. Functor f => Free (Mul :+: f) a -> Free f a
evalMul fr = runFreeM interpretMul fr

data Equal v a = Equal v v (Boolean -> a)

derive instance Functor (Equal v)

equals :: forall f v. (Extends (Equal v) f) => Free f v -> Free f v -> Free f Boolean
equals frx fry = do
  x <- frx
  y <- fry
  liftF $ inj (Equal x y identity)

interpretEquals
  :: forall f v a
   . Eq v
  => Functor f
  => (Equal v :+: f) (Free (Equal v :+: f) a)
  -> Free f (Free (Equal v :+: f) a)
interpretEquals (Inl (Equal x y f)) = pure $ f $ x == y
interpretEquals (Inr f) = liftF f

evalEquals :: forall @v f a. Eq v => Functor f => Free (Equal v :+: f) a -> Free f a
evalEquals fr = runFreeM interpretEquals fr

test :: Boolean
test = evalFin <<< evalEquals @Boolean <<< evalMul <<< evalEquals @Int <<< evalAdd
  $ equals (pure true) (equals (pure 3) (pure 3))
