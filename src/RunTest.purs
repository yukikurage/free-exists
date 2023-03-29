module RunTest where

import Prelude

import Control.Safely (for_, traverse_)
import Data.Functor (mapFlipped)
import Data.Identity (Identity)
import Data.Newtype (un, unwrap, wrap)
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Class.Console (log)
import Lan (Lan, lan, unLan)
import Run (EFFECT, Run, interpret, interpretRec, liftEffect, match, on, runRec, send)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data TalkF a = Speak String a

derive instance functorLogF :: Functor TalkF

type TALK r = (talk :: TalkF | r)

_talk = Proxy :: Proxy "talk"

speak :: forall r. String -> Run (TALK + r) Unit
speak str = Run.lift _talk (Speak str unit)

talkHandlerA :: forall r. Run (EFFECT + TALK + r) Unit -> Run (EFFECT + r) Unit
talkHandlerA = interpretRec do
  on _talk
    ( case _ of
        Speak str next -> do
          liftEffect $ log $ "[A] " <> str
          pure next
    )
    send

talkHandlerB :: forall r. Run (EFFECT + TALK + r) Unit -> Run (EFFECT + r) Unit
talkHandlerB = interpretRec do
  on _talk
    ( case _ of
        Speak str next -> do
          liftEffect $ log $ "[B] " <> str
          pure next
    )
    send

type ARRAY r = (array :: Array | r)

_array = Proxy :: Proxy "array"

array :: forall r a. Array a -> Run (ARRAY + r) a
array arr = Run.lift _array arr

arrayHandler :: forall r. Run (ARRAY + r) Unit -> Run r Unit
arrayHandler = runRec do
  on _array
    (\arr -> pure $ traverse_ identity arr)
    send

hello :: forall r. Run (EFFECT + TALK + ARRAY + r) Unit
hello = do
  x <- array [ 0, 1, 2 ]
  y <- array [ "!", "?", "." ]
  speak $ "Hello" <> show x {- <> y -}

helloHandledA :: forall r. Run (EFFECT + ARRAY + r) Unit
helloHandledA = talkHandlerA hello

helloHandledB :: forall r. Run (EFFECT + ARRAY + r) Unit
helloHandledB = talkHandlerB do
  helloHandledA
  speak $ "World" {- <> y -}

main :: Effect Unit
main = do
  Run.runBaseEffect $ arrayHandler helloHandledB

getX :: forall r. { x :: Int | r } -> Int
getX { x } = x
