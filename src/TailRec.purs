module TailRec where

import Prelude

import Control.Monad.State (State, execState, get, put)
import Data.Array ((..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Safely (for_)

sumStateRec :: Int -> Int -> State Int Unit
sumStateRec n m = for_ (n .. m) \i -> do
  s <- get
  put (s + i)

main :: Effect Unit
main = do
  logShow $ execState (sumStateRec 1 1000000) 0
  logShow $ execState (sumState 1 1000000) 0

sumState :: Int -> Int -> State Int Unit
sumState n m =
  if n > m then
    pure unit
  else do
    s <- get
    put $ s + n
    sumState (n + 1) m
