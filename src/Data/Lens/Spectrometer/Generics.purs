-- | Use Generics to automatically derive Spectrometer Lens for a data type.

module Data.Lens.Spectrometer.Generics where

import Prelude

import Data.FastVect.FastVect as V
import Data.Generic.Rep as G
import Data.Lens (Iso, iso, withIso)
import Data.Lens.Spectrometer (Spectrometer, represented)
import Data.Reflectable (class Reflectable)
import Data.Tuple (Tuple(..))
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT)
import Type.Proxy (Proxy(..))

class GenericSpectrometer :: Type -> Type -> Type -> Type -> Constraint
class GenericSpectrometer s t a b | s -> a, t -> b where
  genericSpectrometer :: Spectrometer s t a b

-- | Rep 用
class GenericSpectrometerRep s t a b | s -> a, t -> b where
  genericSpectrometerRep :: Spectrometer s t a b

-- | Argument 用
class ArgumentIso n s t a b | s -> a, t -> b, s -> n, t -> n where
  argumentVectIso :: Iso s t (V.Vect n a) (V.Vect n b)

instance
  ( G.Generic s repS
  , G.Generic t repT
  , GenericSpectrometerRep repS repT a b
  ) =>
  GenericSpectrometer s t a b where
  genericSpectrometer = genericSpectrometerRep >>> iso G.from G.to

-- | Constructor 1 つにしか対応しないので Product は考えない
instance (ArgumentIso n s t a b, Reflectable n Int, Compare n (-1) GT) => GenericSpectrometerRep (G.Constructor symS s) (G.Constructor symT t) a b where
  genericSpectrometerRep = constructorIso <<< argumentVectIso <<< represented
    where
    constructorIso :: Iso (G.Constructor _ s) (G.Constructor _ t) s t
    constructorIso = iso (\(G.Constructor s) -> s) G.Constructor

instance ArgumentIso 0 G.NoArguments G.NoArguments a b where
  argumentVectIso = iso (\G.NoArguments -> V.empty) (\_ -> G.NoArguments)

instance ArgumentIso 1 (G.Argument a) (G.Argument b) a b where
  argumentVectIso = iso (\(G.Argument a) -> V.singleton a) (\vec -> G.Argument $ V.head vec)

instance (ArgumentIso n sl tl a b, ArgumentIso m sr tr a b, Add n m nPlusM, Compare n (-1) GT, Compare m (-1) GT, Reflectable n Int) => ArgumentIso nPlusM (G.Product sl sr) (G.Product tl tr) a b where
  argumentVectIso = iso (\(G.Product sl sr) -> V.append (slToVec sl) (srToVec sr)) \vec ->
    let
      { before, after } = V.splitAt (Proxy :: _ n) vec
    in
      G.Product (vecToSl before) (vecToSr after)
    where
    Tuple (slToVec :: sl -> V.Vect n a) (vecToSl :: V.Vect n b -> tl) = withIso argumentVectIso Tuple
    Tuple (srToVec :: sr -> V.Vect m a) (vecToSr :: V.Vect m b -> tr) = withIso argumentVectIso Tuple
