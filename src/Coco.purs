module Coco where

import Prelude

import Control.Comonad.Store (Store, store)
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Functor.Pairing (type (⋈), zap)
import Data.Functor.Pairing as Pairing
import Effect (Effect)
import Effect.Console (logShow)

ssPair :: forall s. State s ⋈ Store s
ssPair = Pairing.stateStore Pairing.identity

ssProgram :: State Int (Int -> String)
ssProgram = do
  x <- State.get
  State.modify_ (_ + 1)
  State.modify_ (_ + 2)
  y <- State.get
  State.put (x + y)
  pure \z -> "x = " <> show x <> ", y = " <> show y <> ", z = " <> show z

ssInterpreter :: Store Int Int
ssInterpreter = store (_ + 2) 0

ssResult :: String
ssResult = zap ssPair ssProgram ssInterpreter

main :: Effect Unit
main = do
  logShow ssResult
