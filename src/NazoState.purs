module NazoState where

import Prelude

import Data.Tuple (Tuple(..))

data NazoState s a = NazoState (s -> Tuple s a)

derive instance Functor (NazoState s)

instance Apply (NazoState s) where
  apply (NazoState f) (NazoState a) = NazoState \s ->
    let
      Tuple _ a' = a s
      Tuple s' f' = f s
    in
      Tuple s' (f' a')

instance Bind (NazoState s) where
  bind (NazoState a) f = NazoState \s ->
    let
      Tuple _ a' = a s
      NazoState f' = f a'
    in
      f' s

instance Applicative (NazoState s) where
  pure a = NazoState (\s -> Tuple s a)

instance Monad (NazoState s)

{-

Monad 則

1. (pure x >>= f) == f x

pure x >>= f
= (\s -> Tuple s x) >>= f
= (\s ->
    let
      Tuple _ x' = (\s -> Tuple s x) s
    in
      f x' s
  )
= \s -> f x s
= f x　

2. (m >>= pure) == m

m >>= pure
= \s ->
  let
    Tuple _ a' = m s
  in
    pure a' s
= \s ->
  let
    Tuple _ a' = m s
  in
    (\s -> Tuple s a') s
= \s ->
  let
    Tuple _ a' = m s
  in
    Tuple s a'

-}
