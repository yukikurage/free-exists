module AffT where

import Prelude

import Control.Alt (class Alt, alt, (<|>))
import Control.Monad.State (StateT)
import Control.Monad.State as S
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), forkAff, joinFiber, launchAff_, makeAff, parallel, sequential)
import Effect.Aff.AVar (AVar, empty, new, put, read, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Timer (clearTimeout, setTimeout)

data AffT m a
  = Done a
  | Async (Aff (AffT m a))
  | Monadic (m (AffT m a))

instance Functor m => Functor (AffT m) where
  map f = case _ of
    Done a -> Done (f a)
    Async aff -> Async (map (map f) aff)
    Monadic m -> Monadic (map (map f) m)

instance Functor m => Apply (AffT m) where
  apply ff fa = case ff of
    Done f -> map f fa
    Async aff -> Async (map (\ff' -> apply ff' fa) aff)
    Monadic m -> Monadic (map (\ff' -> apply ff' fa) m)

instance Functor m => Applicative (AffT m) where
  pure = Done

instance Functor m => Bind (AffT m) where
  bind ma f = case ma of
    Done a -> f a
    Async aff -> Async (map (\ma' -> bind ma' f) aff)
    Monadic m -> Monadic (map (\ma' -> bind ma' f) m)

instance Functor m => Monad (AffT m)

instance MonadTrans AffT where
  lift m = Monadic (map Done m)

instance Functor m => MonadEffect (AffT m) where
  liftEffect eff = liftAff (liftEffect eff)

instance Functor m => MonadAff (AffT m) where
  liftAff aff = Async (map Done aff)

runAffT :: forall m a. MonadAff m => AffT m a -> m a
runAffT = case _ of
  Done a -> pure a
  Async aff -> liftAff aff >>= runAffT
  Monadic m -> m >>= runAffT

concurrent :: forall m a b. Monad m => AffT m a -> AffT m b -> AffT m (Tuple a b)
concurrent ma mb = do
  (fromL :: AVar (Either (m (AffT m a)) a)) <- liftAff empty
  (toL :: AVar (AffT m a)) <- liftAff empty
  (fromR :: AVar (Either (m (AffT m b)) b)) <- liftAff empty
  (toR :: AVar (AffT m b)) <- liftAff empty

  let
    thread :: forall x. AVar (Either (m (AffT m x)) x) -> AVar (AffT m x) -> AffT m x -> Aff Unit
    thread from to program = case program of
      Done a -> do
        put (Right a) from
      Async aff -> do
        next <- aff
        thread from to next
      Monadic m -> do
        put (Left m) from
        next <- take to
        thread from to next

    threadL = thread fromL toL ma
    threadR = thread fromR toR mb

    altWithEither :: forall f x y. Alt f => f x -> f y -> f (Either x y)
    altWithEither left right = alt (map Left left) (map Right right)

    takeFirst :: forall x y. AVar x -> AVar y -> Aff (Either x y)
    takeFirst left right = do
      res <- sequential $ altWithEither (parallel (read left)) (parallel (read right))
      case res of
        Left a -> do
          void $ take left
          pure (Left a)
        Right b -> do
          void $ take right
          pure (Right b)

    mainLoop :: Maybe a -> Maybe b -> AffT m (Tuple a b)
    mainLoop (Just l) (Just r) = Done (Tuple l r)
    mainLoop l r = do
      next <- liftAff $ takeFirst fromL fromR
      case next of
        Left (Left m) -> do
          res <- lift m
          liftAff $ put res toL
          mainLoop l r
        Left (Right a) -> do
          mainLoop (Just a) r
        Right (Left m) -> do
          res <- lift m
          liftAff $ put res toR
          mainLoop l r
        Right (Right b) -> do
          mainLoop l (Just b)

  _ <- liftAff $ forkAff threadL
  _ <- liftAff $ forkAff threadR

  Tuple l r <- mainLoop Nothing Nothing

  pure (Tuple l r)

wait :: forall m. MonadAff m => Int -> m Unit
wait n = liftAff $ makeAff \callback -> do
  id <- setTimeout n do
    callback (Right unit)
  pure $ Canceler \_ -> liftEffect $ clearTimeout id

testThreadL :: AffT (StateT Int Aff) Unit
testThreadL = do
  liftEffect $ log "Thread L"
  s1 <- lift $ S.get
  liftEffect $ log $ "Thread L: " <> show s1
  wait 200
  s2 <- lift $ S.get
  liftEffect $ log $ "Thread L: " <> show s2
  wait 600
  s3 <- lift $ S.get
  liftEffect $ log $ "Thread L: " <> show s3
  pure unit

testThreadR :: AffT (StateT Int Aff) Unit
testThreadR = do
  liftEffect $ log "Thread R"
  wait 100
  lift $ S.put 1
  wait 200
  lift $ S.put 2
  wait 200
  lift $ S.put 3
  pure unit

program :: AffT (StateT Int Aff) Unit
program = do
  liftEffect $ log "Start"
  void $ concurrent testThreadL testThreadR
  liftEffect $ log "End"

main :: Effect Unit
main = launchAff_ do
  void $ S.runStateT (runAffT program) 0
  pure unit
