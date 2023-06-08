module Wtf where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, tail)
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.State (State, get, put)
import Data.Coyoneda (Coyoneda, liftCoyoneda, unCoyoneda)
import Data.Either (Either(..))
import Data.Functor.Pairing (type (⋈), freeCofree, sym, zap)
import Data.Leibniz (type (~), coerce, symm)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

-- From
-- https://github.com/wasabi315/kitchen-sink/blob/main/haskell/shallow-handlers/app/Main.hs#L64

-- Freer

type Freer f = Free (Coyoneda f)

send :: forall f. f ~> Freer f
send = liftF <<< liftCoyoneda

-- Pairing

-- What is this??
newtype Wtf f a = Wtf (forall x. f x -> Tuple x a)

instance Functor (Wtf f) where
  map f (Wtf g) = Wtf \fx -> case g fx of
    Tuple x a -> Tuple x (f a)

coyonedaWtf :: forall f. Coyoneda f ⋈ Wtf f
coyonedaWtf f coyo (Wtf g) = unCoyoneda
  ( \xToA fx ->
      let Tuple x b = g fx in f (xToA x) b
  )
  coyo

type Cofreer f = Cofree (Wtf f)

freerCofreer :: forall f. Freer f ⋈ Cofreer f
freerCofreer = freeCofree coyonedaWtf

-- State

data StateF s a
  = Get (a ~ s)
  | Put s (a ~ Unit)

getF :: forall a. Freer (StateF a) a
getF = send $ Get identity

putF :: forall a. a -> Freer (StateF a) Unit
putF s = send $ Put s identity

handleState :: forall s. s -> Wtf (StateF s) s
handleState s = Wtf case _ of
  Get lbz -> Tuple (coerce (symm lbz) s) s
  Put s' lbz -> Tuple (coerce (symm lbz) unit) s'

coiter :: forall f a. Functor f => (a -> f a) -> a -> Cofree f a
coiter f a = buildCofree (\acc -> Tuple acc (f acc)) a

evalState :: forall s a. Freer (StateF s) a -> s -> a
evalState m s = zap (sym freerCofreer) (identity <$ coiter handleState s) m

test :: Freer (StateF Int) (Array Int)
test = do
  a <- getF
  b <- getF
  putF 100
  c <- getF
  putF b
  d <- getF
  pure [ a, b, c, d ]

main :: Effect Unit
main = logShow $ evalState test 0

newtype WtfT :: forall k. (k -> Type) -> (k -> Type) -> Type -> Type
newtype WtfT f m a = WtfT (forall x. f x -> Tuple (m x) a)

instance Functor (WtfT f m) where
  map f (WtfT g) = WtfT \fx -> case g fx of
    Tuple mx a -> Tuple mx (f a)

type CofreerT :: forall k. (k -> Type) -> (k -> Type) -> Type -> Type
type CofreerT f m = Cofree (WtfT f m)

handleStateT :: forall s. WtfT (StateF s) (State s) Unit
handleStateT = WtfT case _ of
  Get lbz -> Tuple (get <#> \s -> coerce (symm lbz) s) unit
  Put s' lbz -> Tuple (put s' $> coerce (symm lbz) unit) unit

monadicWithCofreeT :: forall f m a. Monad m => Freer f a -> CofreerT f m Unit -> m a
monadicWithCofreeT fr cf = case resume fr of
  Right a -> pure a
  Left coyo -> unCoyoneda
    ( \bToFr fb -> do
        let
          WtfT f = tail cf
          Tuple mb t = f fb
        b <- mb
        monadicWithCofreeT (bToFr b) t
    )
    coyo

coReplicate :: forall f. Functor f => f Unit -> Cofree f Unit
coReplicate f = coiter (\_ -> f) unit

convertToState :: forall s a. Freer (StateF s) a -> State s a
convertToState fr = monadicWithCofreeT fr $ coReplicate handleStateT
