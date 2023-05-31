module FizzBuzz where

import Prelude

import Control.Safely (for_)
import Data.Array ((..))
import Effect (Effect)
import Effect.Class.Console (log)

fizz :: Int -> String
fizz i = if i `mod` 3 == 0 then "Fizz" else ""

buzz :: Int -> String
buzz i = if i `mod` 5 == 0 then "Buzz" else ""

fallback :: forall m. Monoid m => Eq m => m -> m -> m
fallback s t = if t == mempty then s else t

fizzbuzz :: Int -> String
fizzbuzz = (fallback <<< show) <*> (fizz <> buzz)

main :: Effect Unit
main = do
  for_ (0 .. 100) (log <<< fizzbuzz)
