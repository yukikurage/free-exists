module Data.Functor.Variant.Helper where

import Prelude

import Data.Functor.Variant (VariantF, inj, on)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

-- -- | 指定したラベルについて拡張する
expandOne
  :: forall r sym f a r1 r2
   . Cons sym f r2 r
  => IsSymbol sym
  => Cons sym f r1 r2
  => Functor f
  => Proxy sym
  -> VariantF r a
  -> VariantF r2 a
expandOne p = on p (inj p) identity

-- | on の逆操作
-- | 指定したラベルに対する操作を除外する
-- eliminate :: forall sym f a b r1 r2. Cons sym f r2 r1 => IsSymbol sym => Proxy sym -> (VariantF r1 a -> b) -> VariantF r2 a -> b
-- eliminate _ f v =
--   let
--     UnvariantF uvf = unvariantF v
--     uv' = UnvariantF $ \cb -> uvf cb
--   in
--     f $ revariantF uv'
