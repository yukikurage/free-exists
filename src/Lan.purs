module Lan where

import Prelude

import Data.Exists (Exists, mkExists, runExists)

data LanHelper :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
data LanHelper g h a b = LanHelper (g b -> a) (h b)

-- | GADTs
-- | Lan :: (g b -> a) -> h b -> Lan g h a
newtype Lan g h a = Lan (Exists (LanHelper g h a))

instance Functor (Lan g h) where
  map f (Lan e) = Lan $ runExists (\(LanHelper g h) -> mkExists $ LanHelper (f <<< g) h) e

lan :: forall g h a. h a -> Lan g h (g a)
lan h = Lan $ mkExists $ LanHelper identity h

unLan :: forall g h a x. (forall b. h b -> (g b -> a) -> x) -> Lan g h a -> x
unLan f (Lan e) = runExists (\(LanHelper g b) -> f b g) e
