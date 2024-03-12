module Data.Lens.Spectrometer where

import Prelude

import Data.FastVect.FastVect (Vect)
import Data.Functor.Representable (class Representable)
import Data.Lens (Optic, re)
import Data.Reflectable (class Reflectable)
import Data.Traversable (class Traversable)
import Data.VectToTuple (class VectToTuple, vectTupleIso)
import Duplicate (class Duplicate, duplicate)
import Prim.Int (class Compare)
import Prim.Ordering (GT)

-- | Between Iso and Grate
-- | and between Iso and Traversal !
type Spectrometer s t a b = forall p. Duplicate p => Optic p s t a b

represented
  :: forall f a b
   . Traversable f
  => Representable f
  => Spectrometer (f a) (f b) a b
represented = duplicate

spectrometer
  :: forall n a b tupleA tupleB
   . Reflectable n Int
  => Compare n (-1) GT
  => VectToTuple (Vect n a) tupleA
  => VectToTuple (Vect n b) tupleB
  => Spectrometer tupleA tupleB a b
spectrometer = re vectTupleIso <<< represented
