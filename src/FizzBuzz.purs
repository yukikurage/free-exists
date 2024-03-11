module FizzBuzz where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Safely (for_)
import Data.Array ((..))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)

withMultiple :: Int -> String -> Int -> Maybe String
withMultiple n s i = guard (i `mod` n == 0) $> s

fizz :: Int -> Maybe String
fizz = withMultiple 3 "Fizz"

buzz :: Int -> Maybe String
buzz = withMultiple 5 "Buzz"

fizzBuzz :: Int -> Maybe String
fizzBuzz = fizz <> buzz

fizzBuzzWithNum :: Int -> String
fizzBuzzWithNum = lift2 fromMaybe show fizzBuzz

main :: Effect Unit
main = do
  for_ (0 .. 100) (log <<< fizzBuzzWithNum)
