module Data.TypeVariant where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Type.Proxy (Proxy(..))
-- import Unsafe.Coerce (unsafeCoerce)
-- import Data.TypeList (TypeList, class Index)

-- -- | 任意の型を表した型 Unsafe
-- foreign import data Value :: Type

-- data TypeVariant :: TypeList Type -> Type
-- data TypeVariant l = TypeVariant Int Value

-- -- | Variant を作る
-- inj :: forall a as index. Index a as index => a -> TypeVariant as
-- inj a = TypeVariant (reflectType @index Proxy) (unsafeCoerce a)

-- prj :: forall a as index. Index a as index => TypeVariant as -> Maybe a
-- prj (TypeVariant i v) = if i == reflectType @index Proxy then Just (unsafeCoerce v) else Nothing

-- injTest :: forall as index. Index Boolean as index => TypeVariant as
-- injTest = inj true

-- injTest2 :: forall as index. Index Int as index => TypeVariant as
-- injTest2 = inj 0

-- injTest3 :: forall i1 as i2
--   . Index Boolean as i1
--   => Index Int as i2
--   => Array (TypeVariant as)
-- injTest3 = [injTest, injTest2]
