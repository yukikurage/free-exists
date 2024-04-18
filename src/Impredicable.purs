module Impredicable where

import Prelude

import Data.Maybe (Maybe(..))

newtype T = T (forall a. (a -> a) -> a -> a)

m :: Maybe T
m = Just f
  where
  f :: T
  f = T \f x -> f x