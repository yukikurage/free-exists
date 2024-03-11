module Data.TypeVariantF where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.TypeList (class Append, class FindIndex, TypeList)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | 任意の Functor を表した型 Unsafe
foreign import data ValueF :: Type -> Type

data TypeVariantF :: TypeList (Type -> Type) -> Type -> Type
data TypeVariantF l a = TypeVariant (forall x y. (x -> y) -> (ValueF x -> ValueF y)) Int (ValueF a)

instance Functor (TypeVariantF l) where
  map f (TypeVariant g i v) = TypeVariant g i $ g f v

-- | Variant を作る
inj :: forall f fs index a. FindIndex f fs index => Functor f => f a -> TypeVariantF fs a
inj a = TypeVariant (unsafeCoerce (map @f)) (reflectType @index Proxy) (unsafeCoerce a)

-- | Variant から値を取り出す
prj :: forall f fs index a. FindIndex f fs index => TypeVariantF fs a -> Maybe (f a)
prj (TypeVariant _ i v) = if i == reflectType @index Proxy then Just (unsafeCoerce v) else Nothing

-- | Variant の後ろ側拡張
extend :: forall fs1 fs2 fs3. Append fs1 fs2 fs3 => TypeVariantF fs1 ~> TypeVariantF fs3
extend = unsafeCoerce
