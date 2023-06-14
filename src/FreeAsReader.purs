module FreeAsReader where

import Prelude

import Control.Monad.Free (Free, liftF, resume')
import Control.Monad.State (State, get, put, runState)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

data RFree f a = RFree (forall m. Monad m => (f ~> m) -> m a)

runRFree :: forall f m. Monad m => (f ~> m) -> RFree f ~> m
runRFree f (RFree m) = m f

toFree :: forall f. RFree f ~> Free f
toFree (RFree run) = run liftF

fromFree :: forall f a. Free f a -> RFree f a
fromFree m = RFree (run m)
  where
  run :: forall m. Monad m => (Free f a) -> (f ~> m) -> m a
  run m' hoist = resume' evalLeft evalRight m'
    where
    evalLeft :: forall b. f b -> (b -> Free f a) -> m a
    evalLeft fb k = do
      b <- hoist fb
      let m'' = k b
      run m'' hoist

    evalRight :: a -> m a
    evalRight a = pure a

liftRF :: forall f. f ~> RFree f
liftRF f = RFree \hoist -> hoist f

instance Functor (RFree f) where
  map f (RFree run) = RFree \hoist -> f <$> run hoist

instance Apply (RFree f) where
  apply (RFree fRun) (RFree aRun) = RFree \hoist -> fRun hoist <*> aRun hoist

instance Applicative (RFree f) where
  pure a = RFree \_ -> pure a

instance Bind (RFree f) where
  bind :: forall a b. RFree f a -> (a -> RFree f b) -> RFree f b
  bind (RFree aRun) f = RFree \hoist -> do
    a <- aRun hoist
    let RFree bRun = f a
    bRun hoist

-- Test

data StateF s a = GetF (s -> a) | PutF s a

runStateRF :: forall s. RFree (StateF s) ~> State s
runStateRF (RFree run) = run case _ of
  GetF f -> get <#> f
  PutF s a -> put s *> pure a

getF :: forall s. RFree (StateF s) s
getF = liftRF (GetF identity)

putF :: forall s. s -> RFree (StateF s) Unit
putF s = liftRF (PutF s unit)

-- x |-> x + (x + 1) == 2 * x + 1
program :: RFree (StateF Int) Int
program = do
  x <- getF
  putF (x + 1)
  y <- getF
  pure (x + y)

main :: Effect Unit
main = do
  let Tuple result state = runState (runStateRF program) 5
  logShow result -- 11
  logShow state -- 6
