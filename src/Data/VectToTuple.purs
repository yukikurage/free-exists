-- | Fast Vect を Tuple の組に変換
module Data.VectToTuple where

import Prelude

import Data.FastVect.FastVect (Vect, empty, index, singleton, splitAt, (:))
import Data.Lens (Iso, iso)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))

class VectToTuple vec tuple | vec -> tuple, tuple -> vec where
  toTuple :: vec -> tuple
  fromTuple :: tuple -> vec

instance VectToTuple (Vect 0 a) Unit where
  toTuple _ = unit
  fromTuple _ = empty

else instance (VectToTuple (Vect n a) tuple, Add 1 n m, Compare n (-1) GT) => VectToTuple (Vect m a) (a /\ tuple) where
  toTuple vec = index (Proxy :: _ 0) before /\ toTuple after
    where
    { before, after } = splitAt (Proxy :: _ 1) vec
  fromTuple (a /\ tuple) = a : fromTuple tuple

vectTupleIso
  :: forall vec1 vec2 tuple1 tuple2
   . VectToTuple vec1 tuple1
  => VectToTuple vec2 tuple2
  => Iso vec1 vec2 tuple1 tuple2
vectTupleIso = iso toTuple fromTuple
