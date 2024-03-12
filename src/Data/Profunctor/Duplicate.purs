module Duplicate where

import Prelude

import Data.Distributive (class Distributive)
import Data.Functor.Costar (Costar)
import Data.Identity (Identity)
import Data.Lens (class Wander, Forget, Tagged, wander)
import Data.Lens.Internal.Zipping (Zipping)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Closed (class Closed, closed)
import Data.Profunctor.Star (Star)
import Data.Traversable (class Traversable, traverse)
import Data.Functor.Representable (class Representable, index, tabulate)

-- | 入力と出力をそれぞれ拡張できる
class Profunctor p <= Duplicate p where
  duplicate :: forall f a b. Representable f => Traversable f => p a b -> p (f a) (f b)

-- | Closed is Duplicate
duplicateFromClosed
  :: forall p f a b
   . Closed p
  => Representable f
  => p a b
  -> p (f a) (f b)
duplicateFromClosed = dimap index tabulate <<< closed

-- | つまりヒエラルキー的には Profunctor <= Duplicate <= Closed となる

-- | Wander is Duplicate?
duplicateFromWander
  :: forall p f a b
   . Wander p
  => Traversable f
  => p a b
  -> p (f a) (f b)
duplicateFromWander = wander walker
  where
  walker :: forall g. Applicative g => (a -> g b) -> f a -> g (f b)
  walker f fa = traverse f fa

-- | 下に示す同値の問題を回避するため、Applicative と Distributive の両方を要求する class を作り law を導入する
-- | law: Pair <$> x <*> y = distribute (Pair x y)
class (Applicative f, Distributive f) <= Dispersive f

instance Dispersive (Function e)

instance Dispersive Identity

-- | For Setter
instance Duplicate (->) where
  duplicate = map

-- | For traverse
-- | ~~Spectrometer の Law によって duplicateFromWander == duplicateFromClosed になるはず~~
-- | Representable に一般化してしまったのでもう分からん
instance Dispersive f => Duplicate (Star f) where
  duplicate = duplicateFromWander

-- | For Review
instance Duplicate Tagged where
  duplicate = duplicateFromClosed

-- | For Fold
instance Monoid r => Duplicate (Forget r) where
  duplicate = duplicateFromWander

-- | For Zipper
instance Functor f => Duplicate (Costar f) where
  duplicate = duplicateFromClosed

-- | For Zipping
instance Duplicate Zipping where
  duplicate = duplicateFromClosed

-- | Closed から導出できる Duplicate と Wander から導出できる Duplicate が同じであることは示せる？
-- | そもそも Closed と Wander に関係が無いから厳しそう
-- | これまであるインスタンスに対して証明するのが良いか
-- | (Monoid r) => Wander (Forget r)
-- | (Applicative f) => Wander (Star f)
-- | Wander (->)
-- | かな？

{-
Monoid r => Wander (Forget r) について

Forget r は Closed じゃないので証明の必要はない
-}

{-
Applicative f => Wander (Star f) について
Distributive f => Closed (Star f) があるので証明の必要がある

newtype Star f a b = Star (a -> f b)

Closed のインスタンス

instance closedStar :: Distributive f => Closed (Star f) where
  closed (Star f) = Star \g -> distribute (f <<< g)

Wander のインスタンス

instance wanderStar :: Applicative f => Wander (Star f) where
  wander t (Star f) = Star (t f)

Distributive の Laws
    distribute = collect identity
    distribute <<< distribute = identity
    collect f = distribute <<< map f
    map f = unwrap <<< collect (Identity <<< f)
    map distribute <<< collect f = unwrap <<< collect (Compose <<< f)

Applicative の Laws
    Identity: (pure identity) <*> v = v
    Composition: pure (<<<) <*> f <*> g <*> h = f <*> (g <*> h)
    Homomorphism: (pure f) <*> (pure x) = pure (f x)
    Interchange: u <*> (pure y) = (pure (_ $ y)) <*> u

これらから同じことが言えそう？？？？　ダル過ぎ！？！？！？！？


Star f :: Star f a b に対して duplicateFromClosed と duplicateFromWander が同じであることを示したい

duplicateFromClosed (Star f)
  = dimap toFunc fromFunc $ closed (Star f)
  = dimap toFunc fromFunc $ Star \g -> distribute (f <<< g)

wander walker f
  = Star $ walker f
  = \(Tuple a1 a2) -> Tuple <$> f a1 <*> f a2

無理かも

具体的な Duplicate (Star f) インスタンスを作ればよい？

~~ 色々あって ~~

f a が 2 つ手に入ったとき(x, y とする)、f (Pair a) を作るのには 2 通りの方法がある
1. Distributive f に対して、distribute (Pair x y)
2. Applicative f に対して Pair <$> x <*> y

Distributive かつ Applicative な f について、これらが同じそうであることを示せばよさそう

Tuple <$> x <*> y /= distribute (Tuple x y) とする
-}
