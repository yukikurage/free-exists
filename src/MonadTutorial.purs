module MonadTutorial where

import Prelude

import Data.Array (cons, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

class Add :: Type -> Constraint
class Add a where
  add :: a -> a -> a

instance Add Int where
  add a b = a + b

instance Add Number where
  add a b = a + b

add3 :: forall a. Add a => a -> (a -> (a -> a))
add3 a b c = add a (add b c)

data Option :: Type -> Type
data Option a = None | Some a

instance Show a => Show (Option a) where
  show opt = case opt of
    None -> "None"
    Some a -> "Some " <> show a

opt0 :: forall a. Option a
opt0 = None

opt1 :: Option Int
opt1 = Some 1

class Functor :: (Type -> Type) -> Constraint
class Functor f where
  map :: forall a b. (a -> b) -> (f a -> f b)

instance Functor Array where
  map f arr = case uncons arr of
    Nothing -> []
    Just { head, tail } -> f head `cons` map f tail -- cons (f head) (map f tail)

instance Functor Option where
  map f opt = case opt of
    None -> None
    Some a -> Some (f a)

add1F :: forall f a. Functor f => Add a => a -> f a -> f a
add1F a = map (add a)

class Apply :: (Type -> Type) -> Constraint
class Apply f where
  map2 :: forall a b c. (a -> b -> c) -> (f a -> f b -> f c)

instance Apply Option where
  map2 f opt1 opt2 = case (opt1 /\ opt2) of
    (None /\ _) -> None
    (_ /\ None) -> None
    (Some a /\ Some b) -> Some (f a b)

class Applicative f where
  pure :: forall a. a -> f a

instance Applicative Option where
  pure a = Some a

class Monad f where
  bind :: forall a b. f a -> (a -> f b) -> f b

instance Monad Option where
  bind opt f = case opt of
    None -> None
    Some a -> f a

divTest :: Number -> Option Number
divTest a = if a == 0.0 then None else Some (1.0 / a)

divTest2 :: Number -> Option String
divTest2 a = if a < 0.0 then None else Some (show a)

program :: Number -> Option String
program a =
  ( divTest a
      `bind` \x -> if x < 0.0 then None else Some (show x)
  )
    `bind` \str -> Some ("Result: " <> str)

program2 :: Number -> Option String
program2 a = do
  x <- divTest a
  str <- if x < 0.0 then None else Some (show x)
  Some ("Result: " <> str)
