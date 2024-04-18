module Resumption where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF, resume)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (State, evalState, get, put)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)

-- https://harrisonwl.github.io/assets/papers/hosc-cheapthreads.pdf
-- Resumption Monad

type ResumptionT m a = Free m a

liftR :: forall m. m ~> ResumptionT m
liftR = liftF

runR :: forall m. MonadRec m => ResumptionT m ~> m
runR = foldFree identity

-- | 先頭から左右順に消費していく
concurrent :: forall m a b. MonadRec m => ResumptionT m a -> ResumptionT m b -> ResumptionT m (Tuple a b)
concurrent left right = goL left right
  where
  goL :: ResumptionT m a -> ResumptionT m b -> ResumptionT m (Tuple a b)
  goL l r = case resume l of
    Left ml -> liftF ml >>= \l' -> goR l' r
    Right a -> (\a' -> Tuple a a') <$> r

  goR :: ResumptionT m a -> ResumptionT m b -> ResumptionT m (Tuple a b)
  goR l r = case resume r of
    Left mr -> liftF mr >>= \r' -> goL l r'
    Right a -> (\a' -> Tuple a' a) <$> l

-- | Lifting State
polling :: forall a. ResumptionT (State a) (Array a)
polling = do
  s1 <- liftR get
  s2 <- liftR get
  s3 <- liftR get
  pure [ s1, s2, s3 ]

processing :: ResumptionT (State Int) Unit
processing = do
  liftR $ put 1
  liftR $ put 2
  liftR $ put 3
  pure unit

program :: ResumptionT (State Int) (Array Int)
program = do
  Tuple results _ <- concurrent polling processing
  pure results -- [0,1,2]

-- With Aff ?
concurrentAff :: forall m a b. MonadAff m => ResumptionT m a -> ResumptionT m b -> ResumptionT m (Tuple a b)
concurrentAff left right = goL left right
  where
  goL :: ResumptionT m a -> ResumptionT m b -> ResumptionT m (Tuple a b)
  goL l r = case resume l of
    Left ml -> liftF ml >>= \l' -> goR l' r
    Right a -> (\a' -> Tuple a a') <$> r

  goR :: ResumptionT m a -> ResumptionT m b -> ResumptionT m (Tuple a b)
  goR l r = case resume r of
    Left mr -> liftF mr >>= \r' -> goL l r'
    Right a -> (\a' -> Tuple a' a) <$> l

main :: Effect Unit
main = do
  let
    results = evalState (runR program) 0
  logShow results -- [0,1,2]
