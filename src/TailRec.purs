module TailRec where

import Prelude

import Control.Monad.State (State, execState, get, put)
import Control.Safely as Safely
import Data.Array ((..))
import Data.Foldable as Foldable
import Effect (Effect)
import Effect.Class.Console (logShow)

sumStateRec :: Int -> Int -> State Int Unit
sumStateRec n m = Safely.for_ (n .. m) \i -> do
  s <- get
  put (s + i)

main :: Effect Unit
main = do
  -- logShow $ execState (sumStateRec 1 1000000) 0
  -- logShow $ execState (sumState 1 1000000) 0
  logShow $ execState (stateLong) 0

sumState :: Int -> Int -> State Int Unit
sumState n m = Foldable.for_ (n .. m) \i -> do
  s <- get
  put (s + i)

stateLong :: State Int Unit
stateLong = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong2

stateLong2 :: State Int Unit
stateLong2 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong3

stateLong3 :: State Int Unit
stateLong3 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong4

stateLong4 :: State Int Unit
stateLong4 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong5

stateLong5 :: State Int Unit
stateLong5 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong6

stateLong6 :: State Int Unit
stateLong6 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong7

stateLong7 :: State Int Unit
stateLong7 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  stateLong8

stateLong8 :: State Int Unit
stateLong8 = do
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  put 1
  pure unit
