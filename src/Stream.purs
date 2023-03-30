module Stream where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Functor.Pairing (sym, zap, type (⋈))
import Data.Functor.Pairing.Co (Co)
import Data.Lazy (Lazy, defer, force)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

data StreamW a = StreamW a (Lazy (StreamW a))

derive instance Functor StreamW
instance Extend StreamW where
  extend :: forall a b. (StreamW a -> b) -> StreamW a -> StreamW b
  extend f = unfoldS \s' -> Tuple (f s') $ tailS s'

instance Comonad StreamW where
  extract :: forall a. StreamW a -> a
  extract = headS

iterateS :: forall a. (a -> a) -> a -> StreamW a
iterateS f a = StreamW a $ defer \_ -> iterateS f (f a)

unfoldS :: forall a b. (b -> Tuple a b) -> b -> StreamW a
unfoldS f b =
  let
    Tuple a b' = f b
  in
    StreamW a $ defer \_ -> unfoldS f b'

unconsS :: forall a. StreamW a -> { head :: a, tail :: StreamW a }
unconsS (StreamW a as) = { head: a, tail: force as }

headS :: forall a. StreamW a -> a
headS (StreamW a _) = a

tailS :: forall a. StreamW a -> StreamW a
tailS (StreamW _ as) = force as

indexS :: forall a. StreamW a -> Int -> a
indexS s 0 = headS s
indexS s i = indexS (tailS s) (i - 1)

type StreamM = Co StreamW

shift :: Shift Unit
shift = Shift $ NoShift unit

streamW :: StreamW Int
streamW = iterateS (_ * 2) 1

streamM :: Shift (Int -> String)
streamM = do
  shift
  shift
  shift
  shift
  pure $ \x -> "x: " <> show x

result :: String
result = zap (sym streamShiftPair) streamM streamW

main :: Effect Unit
main = do
  log result
  logShow $ apply (pure (_ + 1) <* shift <* shift) (pure 1 <* shift <* shift)

data Shift a = NoShift a | Shift (Shift a)

instance Show a => Show (Shift a) where
  show (NoShift a) = "(NoShift " <> show a <> ")"
  show (Shift s) = "(Shift " <> show s <> ")"

instance Functor Shift where
  map :: forall a b. (a -> b) -> Shift a -> Shift b
  map f (NoShift a) = NoShift (f a)
  map f (Shift s) = Shift (map f s)

instance Apply Shift where
  apply :: forall a b. Shift (a -> b) -> Shift a -> Shift b
  apply (NoShift f) a = map f a
  apply s (NoShift a) = map (_ $ a) s
  apply (Shift sf) s = Shift (apply sf s)

instance Applicative Shift where
  pure :: forall a. a -> Shift a
  pure = NoShift

instance Bind Shift where
  bind :: forall a b. Shift a -> (a -> Shift b) -> Shift b
  bind (NoShift a) f = f a
  bind (Shift s) f = Shift (bind s f)

instance Monad Shift

-- | f ⋈ g = forall a b c. (a -> b -> c) -> f a -> g b -> c

streamShiftPair :: StreamW ⋈ Shift
streamShiftPair f (StreamW a as) = case _ of
  NoShift b -> f a b
  Shift s -> streamShiftPair f (force as) s
