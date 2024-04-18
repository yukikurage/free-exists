module AffEffExtension where

import Prelude

import Control.Alt (class Alt, alt)
import Data.Either (Either(..))
import Data.Generic.Rep (from)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), forkAff, launchAff_, makeAff, parallel, sequential)
import Effect.Aff.AVar (AVar, empty, read, take)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Class as Effect
import Effect.Class.Console (log, logShow)
import Effect.Timer (clearTimeout, setTimeout)
import Prim.Row (class Union)
import Run (AFF, Run, VariantF, expand, lift, liftAff, liftEffect, on, peel, runBaseAff, send)
import Run.Reader (READER)
import Run.State (STATE, evalState, get, put)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | Extensions for AFF Effect in purescript-run

-- | Call and Response

parAff :: forall r a b t. Union r t (AFF + r) => Run (AFF + r) a -> Run (AFF + r) b -> Run (AFF + r) (Tuple a b)
parAff ma mb = do
  (fromL :: AVar (Either (VariantF r (Run (AFF + r) a)) a)) <- liftAff empty
  (fromR :: AVar (Either (VariantF r (Run (AFF + r) b)) b)) <- liftAff empty

  let
    thread :: forall x. AVar (Either (VariantF r (Run (AFF + r) x)) x) -> Run (AFF + r) x -> Aff Unit
    thread avar program = case peel program of
      Right a -> AVar.put (Right a) avar
      Left variant -> variant # on (Proxy :: _ "aff")
        ( \aff -> do
            res <- aff
            thread avar res
        )
        (\others -> AVar.put (Left others) avar)

    threadL = thread fromL ma
    threadR = thread fromR mb

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

    mainLoop :: Maybe a -> Maybe b -> Run (AFF + r) (Tuple a b)
    mainLoop (Just l) (Just r) = pure (Tuple l r)
    mainLoop l r = do
      next <- liftAff $ takeFirst fromL fromR
      case next of
        Left (Left m) -> do
          res <- expand $ send m
          let threadL = thread fromL res
          _ <- liftAff $ forkAff threadL
          mainLoop l r
        Left (Right a) -> do
          mainLoop (Just a) r
        Right (Left m) -> do
          res <- expand $ send m
          let threadR = thread fromR res
          _ <- liftAff $ forkAff threadR
          mainLoop l r
        Right (Right b) -> do
          mainLoop l (Just b)

  _ <- liftAff $ forkAff threadL
  _ <- liftAff $ forkAff threadR

  Tuple l r <- mainLoop Nothing Nothing

  pure (Tuple l r)

type MyOps r = AFF + STATE Int + r

wait n = liftAff $ makeAff \callback -> do
  id <- setTimeout n do
    callback (Right unit)
  pure $ Canceler \_ -> Effect.liftEffect $ clearTimeout id

program :: Run (MyOps ()) Unit
program = do
  let
    left = do
      s1 <- get
      wait 200
      s2 <- get
      wait 200
      s3 <- get
      pure [ s1, s2, s3 ]

    right = do
      wait 100
      put 1
      wait 200
      put 2
      wait 200
      put 3
      pure unit

  Tuple results _ <- parAff left right
  liftAff $ logShow results

main :: Effect Unit
main = do
  _ <- launchAff_ $ runBaseAff $ evalState 0 program
  pure unit