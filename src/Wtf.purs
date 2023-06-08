module Wtf where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree, tail)
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.State (State, evalState, get, put)
import Data.Coyoneda (Coyoneda, liftCoyoneda, unCoyoneda)
import Data.Either (Either(..))
import Data.Functor.Pairing (type (⋈), freeCofree, sym, zap)
import Data.Leibniz (type (~), coerce, symm)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

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

evalStateT :: forall s a. Freer (StateF s) a -> s -> a
evalStateT m s = zap (sym freerCofreer) (identity <$ coiter handleState s) m

test :: Freer (StateF Int) (Array Int)
test = do
  a <- getF
  b <- getF
  putF 100
  c <- getF
  putF b
  d <- getF
  pure [ a, b, c, d ]

newtype WtfT f m a = WtfT (forall x. f x -> m (Tuple x a))

instance Functor m => Functor (WtfT f m) where
  map f (WtfT g) = WtfT \fx -> map f <$> g fx

type CofreerT f m = Cofree (WtfT f m)

handleStateT :: forall s. WtfT (StateF s) (State s) Unit
handleStateT = WtfT case _ of
  Get lbz -> get <#> \s -> Tuple (coerce (symm lbz) s) unit
  Put s' lbz -> put s' $> Tuple (coerce (symm lbz) unit) unit

monadicWithCofreeT :: forall f m a. Monad m => Freer f a -> CofreerT f m Unit -> m a
monadicWithCofreeT fr cf = case resume fr of
  Right a -> pure a
  Left coyo -> unCoyoneda
    ( \bToFr fb -> do
        let
          WtfT f = tail cf
          mbt = f fb
        Tuple b t <- mbt
        monadicWithCofreeT (bToFr b) t
    )
    coyo

coReplicate :: forall f. Functor f => f Unit -> Cofree f Unit
coReplicate f = coiter (\_ -> f) unit

convertToState :: forall s a. Freer (StateF s) a -> State s a
convertToState fr = monadicWithCofreeT fr $ coReplicate handleStateT

handleStateT2 :: forall s. Show s => s -> WtfT (StateF s) Effect s
handleStateT2 s = WtfT case _ of
  Get lbz ->
    do
      logShow "get"
      pure $ Tuple (coerce (symm lbz) s) s
  Put s' lbz ->
    do
      log $ "put: " <> show s'
      pure $ Tuple (coerce (symm lbz) unit) s'

evalStateWithEffect :: forall s a. Show s => Freer (StateF s) a -> s -> Effect a
evalStateWithEffect fr s = monadicWithCofreeT fr $ (void $ coiter handleStateT2 s)

main :: Effect Unit
main = do
  logShow $ evalStateT test 0
  logShow $ evalState (convertToState test) 0
  a <- evalStateWithEffect test 0
  logShow a
